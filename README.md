# Dotted Version Vector Sets - Managing Values with Causality

**TL;DR** Dotted Version Vector Sets are similar to Version Vectors (Vector Clocks for some), but with a better API and are more well suited to distributed use cases, such has distributed databases. It also prevents false conflicts that occur with Version Vectors.

### Summary
- Intro
- Why not Version Vectors (Vector Clocks)?
- The Solution: Dotted Version Vector Sets
- How to use
- Real World Use Case (Riak)

## Intro

We are presenting the **compact** version of the original [Dotted Version Vectors][paper dvv], which we call **Dotted Version Vector Set (DVVSet)**. It provides a container for sets of conflicting values with causal order information.

Let's assume the scenario of a Distributed Key-Value Storage (Ex: Riak, Cassandra, etc), where the entities are the clients and the replicas, and we can also **PUT** (write a value) and **GET** (read a value).

We can use DVVSet to keep a value and its causal history, with the support for multiples values if they are causally conflicting. One DVVSet is supposed to have 1 value, unless we synchronize or write conflicting DVVSet (we cannot decide through their causal history, which one is newer).
Therefore, this data structure abstracts the process of tracking and reasoning about the causal relationship of multiple values.


## Why not Version Vectors (Vector Clocks)?

First, [Version Vectors are not Vector Clocks][blog VV are not VC]. We are targeting Version Vectors (VV) in this case. Specifically, VV are not suited to support conflicting values. When using VV, we couple a value to a VV, and if we try to reconcile two values using their VV, sometimes we have conflicts and want to keep both values. In this situation, want to do with their VV? We have a couple of options: (#1) keep both values and both VV; (#2) keep both values and merge their VV.
Let's analyze each option with an example with one server, one client (or multiple, since it does does not matter in this case) reading and writing to a single key-value.


#### Approach #1 - **Keep both**:

You can keep both VV, but you are requiring more space for this key. More importantly, VV are not always sufficient to represent conflicts created by two "concurrent writes".

![VV with conflicts #1][VV 1]

**Problem**: As we can see, there is no way for *vanilla* VV to represent two conflicting values, since we can only increment the VV with the id of the server: if we use (A,1) for both, we are saying that they are causally equal, which is not true; using (A,2) implies that *v2* is causally newer than *v1*, which is also not true. If we assume (A,1) for both values *v1* and *v2*, we then have an additional problem where the behavior is undefined or incorrect: in the 3rd write, we receive a value that read *v1* ~ (A,1), so it should conflict with *v2* and overwrite *v1*; however, when writing *v2*, we did not increment its VV to (A,2), thus the new write *v3* wins over (A,1) and overwrites both *v1* and *v2*.

A possible solution could be to use some **metadata** to tell us how the values came from and how to deal with conflicting values, but it would not be straightforward and would be full of corner cases and prone to bugs. We should not put the burden on the developer to implement causal behaviors of conflicting values on top of VV.


#### Approach #2 - **Merge both**

Another solution is to keep both values and merge their VV into one, which saves space when compared to the previous approach. If a new write conflicts with an existing VV, then merge both VV and increment it normally using the server id. Both values are now related to this new merged VV.

![VV with conflicts #2][VV 2]

**Problem**: We lose the information that *v2* was associated with (A,2) and not (A,3). In the third write we can see how this approach could lead to **false conflicts**. *v3* is being written with (A,1), therefore we know that it should win over *v1* and conflict with *v3*, but since we lost information about the causal past of *v1*, the server has no other option but to keep all three values and merge the VV again. Now the problem is even worse, since we are saying that all three values are related to (A,3). It's easy to see that this could lead to undesired behavior and an explosion of false conflicts.



## The Solution: Dotted Version Vector Sets

We solve the problems above with Dotted Version Vector Sets (DVVSet). Instead of having a mapping between values and their causal information (be it Version Vectors or some other mechanism), we provide a data structure that keeps values and causal information together, while providing a sensible API to manipulate it.

Let's see how DVVSet would manage in our previous example:

![Dotted Version Vector Set][DVVSet1]

As we can see from the example, we have a compact representation like approach #2, while preserving sufficient individual causality to infer that *v1* could be discard, since in the 3rd write we have a VV that already describes the causal information associated with *v1*.

A DVVSet is a set of triplets **(ID, Counter, Values)**: *ID* is a unique identifier; *Counter* is a regular monotonic growing counter, starting at 1 for the first event (much like a normal Version Vector); *Values* is an ordered list of values, where each new value is put at the head of the list. 
There is an implicit mapping between values and their causal history. Considering the triplet *(I, C, V)*, for each value V[i] of V (V is zero-based index) we can see the mapping `value ~ clock` as `V[i] ~ (I, C-i)`.

Lets take a more detailed and graphical look at how *GET* and *PUT* worked in our example.

##### Reading (GET #1)

![Dotted Version Vector Set in a GET][DVVSet2]

We extract the *global* causal information (simply a version vector) from DVVSet using a function called `join` and extract all values using the function `values`. The causal information should be treated as an opaque object that should be returned in a subsequent write. The values are for the client to do want it wants, and possibly write a single value back later.

##### Writing (PUT #3)

![Dotted Version Vector Set in a PUT][DVVSet3]

We receive a value and a version vector from the client and we have a local server DVVSet. We synchronize the DVVSet with the VV with `sync` to discard old information. In this case, *v1* is outdated because the client VV already knows (A,1), therefore we discard it. On the other hand, *v2* is associated with (A,2), thus we keep it. We can imagine this `sync` operation as a function that overlaps the VV on the DVVSet, and discard every value that is "overshadowed" by the VV.
Having done that, we advance our causal information in DVVSet and put the new value *v3* inside it, using `event`. Our API provides a function called `update` that does this two steps at once.


#### The Anonymous List

We actually simplified the DVVSet structure a bit for explanation purposes. A valid DVVSet is a list of triplets **(ID, Counter, Values)** like before, with an additional list of values. We call this the **anonymous list**, because it stores values that are not associated any specific point of the causal history, but is instead related to the global causal history. Thus, to "win" over values in *DVVSet1* with an `update` or a `sync`, the other VV or DVVSet must causally dominate *DVVSet1* completely.

But why do we need or want this list? There are use cases where we want to simplify values into a single value. We have a function named `reconcile` that receives a DVVSet and a function. This function receives a list of values and returns a new value (the *winner*). Since this is (or could be) a completely new value, therefore not created directly by a client request, we store it in the anonymous list. It would be dangerous store it in any triplet, since subsequent synchronizations or comparisons with this clock could be unsafe (for example, two DVVSet could be causally equal but have different values, thus one of them could be incorrectly thrown away).

Here is an (Erlang) example using `reconcile`:

```Erlang
    F = fun (L) -> lists:sum(L) end,
    DVVSet = {[{a,4,[5,2]}, {b,1,[]}], [10,1]},
    Res = reconcile(F, DVVSet),
    {[{a,4,[]}, {b,1,[]}], [18]} = Res.
```

We pass to `reconcile` a function that adds all values. The returning DVVSet has the same causal information, but now has only one value (18), which was not present in the previous DVVSet. Thus, we store it in the anonymous list.

There is a special case of reconcile, named **last-write-wins** (lww), where we also want to reduce all values to a single one, but that value *must* be already present in the DVVSet. Thus, we let the winning value stay in the same triplet as it was before. This function named `lww` has the same parameters as `reconcile`, but the function now takes two values and returns true if the first is newer than the second, or false otherwise. Using `lww`, it is impossible to have two DVVSet comparing equal and having different values.

Here is an (Erlang) example of `lww`:

```Erlang
    F = fun (A,B) -> A > B end,
    DVVSet = {[{a,4,[5,2]}, {b,1,[1]}], [3]},
    Res = lww(F, DVVSet),
    {[{a,4,[5]},{b,1,[]}],[]} = Res.
```

We use a function that simply picks the highest value between two values, and use it to sort out which value is the "latest" according to this function. Naturally, in this case it is value 5. Notice how it stay in its original triplet instead of going to anonymous list.


## How to use

The major use case we thought for DVVSet was a client-server system like a distributed database. So here are the common uses of DVVSet to implement in that case:

1. **A client writes a new value**

    ```Erlang
        %% create a new DVVSet for the new value V
        NewDVVSet = new(V),
        %% update the causal history of DVVSet using the server identifier
        DVVSet = update(NewDVVSet, ServerID),
        %% store DVVSet...
    ```

2. **A client reads a value**

    ```Erlang
        %% synchronize from different server DVVSet
        DVVSet = sync(ListOfDVVSet),
        %% get the value(s)
        Val = values(DVVSet),
        %% get the causal information (version vector)
        VV = join(DVVSet),
        %% return both to client...
    ```

3. **A client writes an updated value and an opaque (unaltered) version vector (obtained from a previous read on this key)**

    ```Erlang
        %% create a new DVVSet for the new value V, using the client's version vector VV
        NewDVVSet = new(VV, V),
        %% update the new DVVSet with the local server DVVSet and the server ID
        DVVSet = update(NewDVVSet, LocalDVVSet, ServerID),
        %% store DVVSet...
    ```

4. **A replica receives a DVVSet from the coordinator to (synchronize and) store locally**

    ```Erlang
        %% synchronize the new DVVSet with the local DVVSet
        DVVSet = sync(NewDVVSet, LocalDVVSet),
        %% store DVVSet...
    ```

5. **A replica receives a DVVSet from another replica to synchronize for anti-entropy (keeps replicas up-to-date)**

    ```Erlang
        %% test if the local DVVSet is causally newer than the remote DVVSet
        case less(NewDVVSet, LocalDVVSet) of
            %% we already have the newest DVVSet so do nothing
            true  -> do_nothing;
            %% reconcile both and write locally the resulting DVVSet
            false -> DVVSet = sync(NewDVVSet, LocalDVVSet),
                     %% store DVVSet...
        end.
    ```

6. **A client writes a new value V with the *last-write-wins* policy**

    ```Erlang
        %% create a new DVVSet for the new value, using the client's version vector
        NewDVVSet = new(VV, V),
        %% update the new DVVSet with the local server DVVSet and the server ID
        UpdDVVSet = update(NewDVVSet, LocalDVVSet, ServerID),
        %% preserve the causal information of UpdDVVSet, but keep only 1 value 
        %% according to the ordering function F
        DVVSet = lww(F, UpdDVVSet)
        %% store DVVSet...
    ```
    We could do only `DVVSet = new(V)` and write DVVSet immediately, saving the cost of a local read, but generally its safer to preserve causal information, especially if the *lww* policy can be turn on and off per request or changed during a key lifetime;



## Real World Use Case

We implemented DVVSet in our fork of [Basho's][riak site] [Riak][riak github] NoSQL database, in favor of their VV implementation, as a proof of concept. 

You can view it here: https://github.com/ricardobcl/riak_kv/tree/dvvset

Feel free to clone it and confirm that *It Works (TM)* :) (don't forget to turn on the flag *allow_mult*) 


[paper dvv]: http://gsd.di.uminho.pt/members/vff/dotted-version-vectors-2012.pdf
[blog VV are not VC]: http://haslab.wordpress.com/2011/07/08/version-vectors-are-not-vector-clocks
[VV 1]: images/VV1.png
[VV 2]: images/VV2.png
[DVVSet1]: images/DVVS1.png
[DVVSet2]: images/DVVS2.png
[DVVSet3]: images/DVVS3.png
[riak site]: http://basho.com/
[riak github]: https://github.com/basho/riak