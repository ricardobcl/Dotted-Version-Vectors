# Dotted Version Vector Sets - Managing Values with Causality

**TL;DR** Dotted Version Vector Sets are similar to Version Vectors (Vector Clocks for some), but prevent false conflicts that can occur with Version Vectors. I also has a better API and is better suited to distributed use cases, such has distributed databases (has shown below).

### Summary
- Intro
- Why not Version Vectors (Vector Clocks)?
- The Solution: Dotted Version Vector Sets
- How to use
- Real World with Riak

## Intro

We are presenting the **compact** version of the original [Dotted Version Vectors][paper dvv], which we call **Dotted Version Vector Set (DVVSet)**. It provides a container for a set of conflicting values with causal order information.

Let's assume the scenario of a Distributed Key-Value Storage (Ex: Riak, Cassandra, etc), where the entities are the clients and the replicas, and we can also **PUT** (write a value) and **GET** (read a value).

We can use DVVSet to keep a value and its causal history, with the support for multiples values if they are causally conflicting. One DVVSet is supposed to have 1 value, unless we synchronize or write conflicting DVVSet (we cannot decide through their causal history, which one is newer).
Therefore, this data structure abstracts the process of tracking and reasoning about the causal relationship of multiple values.


## Why not Version Vectors (Vector Clocks)?

First, [Version Vectors are not Vector Clocks][blog VV are not VC]. We are targeting Version Vectors (VV) in this case. Specifically, VV are not suited to support conflicting values. When using VV, we map a value to a VV, and if we try to reconcile two values using their VV, sometimes we have conflicts and want to keep both values. In this situation, want to do with their VV? We have a couple of options: (#1) keep both values and both VV; (#2) keep both values and merge their VV.
Let's analyze each option with an example with one server and two clients reading and writing to a single key.


#### Approach #1 - **Keep both**:

You can keep both VV, but you are requiring more space for this key. More importantly, VV are not always sufficient to represent conflicts created by two "concurrent writes".

![VV with conflicts #1][VV 1]

**Problem**: As we can see, there is no way for *vanilla* VV to represent two conflicting values, since we can only increment the VV with the id of the server: if we use (A,1) for both, we are saying that they are causally equal, which is not true; using (A,2) implies that *v2* is causally newer than *v1*, which is also not true. If we assume (A,1) for both values *v1* and *v2*, we then have an additional problem where the behavior is undefined or incorrect: in the 3rd write, we receive a value that read *v1* ~ (A,1), so it should conflict with *v2* and overwrite *v1*; however, when writing *v2*, we did not increment its VV to (A,2), thus the new write *v3* wins over (A,1) and overwrites both *v1* and *v2*.

A possible solution could be to use some **metadata** to tell us how the values came from and how to deal with conflicting values, but it would not be straightforward and would be full of corner cases and prone to bugs. We should not put the burden on the developer to implement causal behaviors of conflicting values on top of VV.


#### Approach #2 - **Merge both**

Another solution is to keep both values and merge their VV into one, which saves space when compared to the previous approach. If a new write conflicts with an existing VV, then merge both VV and increment it normally using the server id. Both values are now related to this new merged VV.

![VV with conflicts #2][VV 2]

**Problem**: We lose the information that *v1* was associated with (A,1) and not (A,2). In the third write we can see how this approach could lead to **false conflicts**. *v3* is being written with (A,1), therefore we know that it should win over *v1* and conflict with *v2*, but since we lost information about the causal past of *v1*, the server has no other option but to keep all three values and merge the VV again. Now the problem is even worse, since we are saying that all three values are related to (A,3). It's easy to see that this could lead to undesired behavior and an explosion of false conflicts.



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
    Res = dvvset:reconcile(F, DVVSet),
    {[{a,4,[]}, {b,1,[]}], [18]} = Res.
```

We pass to `reconcile` a function that adds all values. The returning DVVSet has the same causal information, but now has only one value (18), which was not present in the previous DVVSet. Thus, we store it in the anonymous list.

There is a special case of reconcile, named **last-write-wins** (lww), where we also want to reduce all values to a single one, but that value *must* be already present in the DVVSet. Thus, we let the winning value stay in the same triplet as it was before. This function named `lww` has the same parameters as `reconcile`, but the function is a **less or equal** [ordering function][ord fun], from which we keep the greater value of all (*note*: we preemptively discard values in a triplet that are not the last, meaning we only take the head of the list in each triplet). Thus, `Fun(A,B)` returns `true` if A compares less than or equal to B, `false` otherwise. 
Using `lww`, it is impossible to have two DVVSet comparing equal and having different values (provided that the ordering function used is the same).

Here is an (Erlang) example of using `lww` in a DVVSet with values types `{Value, Timestamp}`:

```Erlang
    Fun = fun ({_,TS1}, {_,TS2}) -> TS1 =< TS2 end,
    DVVSet = {[{a,4,[{5, 1002345}, {7, 1002340}]}, {b,1,[{4, 1001340}]}], [{2, 1001140}]},
    Res = dvvset:lww(Fun, DVVSet),
    {[{a,4,[{5, 1002345}]}, {b,1,[]}], []} = Res.
```

We define a *less or equal* function for our type of values and use it in `lww`, which return a DVVSet with the same causal information, but only with the *greatest* value remaining. Naturally, in this case it's {5, 1002345}, which has the highest timestamp. Notice how the *winning* value stays in its original triplet instead of going to anonymous list, unlike `reconcile`.


## How to use

The major use case we thought for DVVSet was a client-server system like a distributed database. So here are the common uses of DVVSet to implement in that case:

1. **A client writes a new value**

    ```Erlang
        %% create a new DVVSet for the new value V
        NewDVVSet = dvvset:new([V]),
        %% update the causal history of DVVSet using the server identifier
        DVVSet = dvvset:update(NewDVVSet, ServerID),
        %% store DVVSet...
    ```

2. **A client reads a value**

    ```Erlang
        %% synchronize from different server DVVSet
        DVVSet = dvvset:sync(ListOfDVVSet),
        %% get the value(s)
        Val = dvvset:values(DVVSet),
        %% get the causal information (version vector)
        VV = dvvset:join(DVVSet),
        %% return both to client...
    ```

3. **A client writes an updated value and an opaque (unaltered) version vector (obtained from a previous read on this key)**

    ```Erlang
        %% create a new DVVSet for the new value V, using the client's version vector VV
        NewDVVSet = dvvset:new(VV, [V]),
        %% update the new DVVSet with the local server DVVSet and the server ID
        DVVSet = dvvset:update(NewDVVSet, LocalDVVSet, ServerID),
        %% store DVVSet...
    ```

4. **A replica receives a DVVSet from the coordinator to (synchronize and) store locally**

    ```Erlang
        %% synchronize the new DVVSet with the local DVVSet
        DVVSet = dvvset:sync([NewDVVSet, LocalDVVSet]),
        %% store DVVSet...
    ```

5. **A replica receives a DVVSet from another replica to synchronize for anti-entropy (keeps replicas up-to-date)**

    ```Erlang
        %% test if the local DVVSet is causally newer than the remote DVVSet
        case dvvset:less(NewDVVSet, LocalDVVSet) of
            %% we already have the newest DVVSet so do nothing
            true  -> do_nothing;
            %% reconcile both and write locally the resulting DVVSet
            false -> DVVSet = dvvset:sync([NewDVVSet, LocalDVVSet]),
                     %% store DVVSet...
        end.
    ```

6. **A client writes a new value V with the *last-write-wins* policy**

    ```Erlang
        %% create a new DVVSet for the new value, using the client's version vector
        NewDVVSet = dvvset:new(VV, [V]),
        %% update the new DVVSet with the local server DVVSet and the server ID
        UpdDVVSet = dvvset:update(NewDVVSet, LocalDVVSet, ServerID),
        %% preserve the causal information of UpdDVVSet, but keep only 1 value 
        %% according to the ordering function F
        DVVSet = dvvset:lww(F, UpdDVVSet)
        %% store DVVSet...
    ```
    We could do only `DVVSet = dvvset:new([V])` and write DVVSet immediately, saving the cost of a local read, but generally its safer to preserve causal information, especially if the *lww* policy can be turn on and off per request or changed during a key lifetime;



## Real World with Riak

We implemented DVVSet in our fork of [Basho's][riak site] [Riak][riak github] NoSQL database, in favor of their VV implementation, as a proof of concept. 
You can view it here: https://github.com/ricardobcl/riak_kv/tree/dvvset

Lets take a look at 2 different scenarios, where we can clearly see real world advantages of DVVSet:

* **Scenario 1**
    1. client A writes and reads;
    2. some other client writes a new value (without causal information);
    3. repeat 1 and 2.

* **Scenario 2**
    1. client A writes and reads;
    1. client B writes and reads;
    3. repeat 1 and 2.

These are two patterns use cases where the traditional VV degenerates, while DVVSet does well. Lets see some actual code:

```bash
$ erlc sib.erl; erl sib -pa ebin deps/*/ebin
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.2  (abort with ^G)

1> % Using riak with Version Vectors
1>
1> % Scenario 1
1> sib:run1(101).

Siblings: 101
Values: v79 v10 v18 v34 v61 v68 v45 v1 v25 v30 v19 v55 v63 v29 v53 v89 v90 v49 v14 v67 v36 v65 v31 v27 v91 v72 v2 v86 v99 v11 v21 v20 v85 v22 v71 v3 v26 v7 v59 v93 v57 v40 v17 v9 v77 v4 v41 v62 v80 v33 v43 v54 v76 v37 v98 v92 v15 v56 v16 v66 v60 v46 v48 v52 v5 v13 v44 v8 v32 v101 v70 v69 v97 v28 v73 v50 v83 v6 v42 v51 v75 v81 v74 v100 v64 v12 v88 v94 v78 v47 v82 v95 v96 v23 v35 v39 v87 v24 v58 v38 v84!

ok
2> % Scenario 2
2> sib:run2(101).

Siblings: 101
Values: v32 v75 v49 v19 v83 v50 v72 v1 v33 v22 v14 v26 v92 v64 v9 v86 v37 v85 v16 v17 v99 v43 v24 v47 v56 v11 v87 v52 v67 v94 v35 v81 v95 v6 v28 v27 v8 v20 v10 v100 v53 v97 v13 v62 v38 v93 v55 v34 v31 v74 v5 v3 v54 v25 v59 v84 v12 v76 v23 v42 v36 v39 v58 v45 v73 v78 v96 v66 v51 v48 v41 v80 v71 v101 v79 v57 v30 v7 v68 v77 v82 v65 v15 v89 v63 v40 v18 v91 v60 v21 v29 v70 v46 v98 v4 v2 v69 v90 v88 v61 v44!

ok

3>
3> % Swith to riak with DVVSet
3>
3> % Scenario 1
3> sib:run1(101).

Siblings: 2
Values: v101 v100!

ok
4> % Scenario 2
4> sib:run2(101).

Siblings: 2
Values: v101 v100!

ok
4> 
```

The code can be found here: https://gist.github.com/ricardobcl/4992839

As we can see, both cases have equal results: with VV you have an exploding number of siblings in your Riak database; with DVVSet you have always 2 or 3 siblings.

[paper dvv]: http://gsd.di.uminho.pt/members/vff/dotted-version-vectors-2012.pdf
[blog VV are not VC]: http://haslab.wordpress.com/2011/07/08/version-vectors-are-not-vector-clocks
[VV 1]: images/VV1.png
[VV 2]: images/VV2.png
[DVVSet1]: images/DVVS1.png
[DVVSet2]: images/DVVS2.png
[DVVSet3]: images/DVVS3.png
[ord fun]: http://www.erlang.org/doc/man/lists.html#ordering_function
[riak site]: http://basho.com/
[riak github]: https://github.com/basho/riak