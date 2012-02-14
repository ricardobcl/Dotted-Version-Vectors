Dotted Version Vectors
=======

In cloud computing environments, data storage systems often rely on optimistic
replication to provide good performance to geographically disperse users
and to allow operation even in the presence of failures or network partitions.
In this scenario, it is important to be able to accurately and efficiently identify
updates executed concurrently.
In this paper, first we review, and expose problems with current approaches
to causality tracking in optimistic replication: these either lose
information about causality or do not scale, as they require replicas to
maintain information that grows linearly with the number of clients or
updates.
Then, we propose a novel, scalable solution that fully captures causality:
it maintains very concise information that grows
linearly only with the number of servers that register updates for a given
data element, bounded by the degree of replication.
Moreover, causality can be checked in $O(1)$ time instead of $O(n)$ time
for version vectors.
We have integrated our solution in Riak, and results with realistic benchmarks
show that it can use as little as 10\% of the space consumed by current version
vector implementation, which includes an unsafe pruning mechanism.

Core Functions
--------------



*	__sync(S1, S2)__:
		Takes two clock sets and returns a clock set. It returns a set of
		the more up-to-date concurrent clocks for the union of S1 and
		S2, while discarding obsolete clocks;

*	__update(S, Sr, r)__:
	Takes a set of clocks S (the context supplied by the client), the
	set of clocks Sr in the replica node (for the given key), and the
	replica node id r, and returns a clock for a new replica. This
	clock should: 1) dominate all clocks in S, 2) dominate only those
	clocks in the system that are already dominated by S and
	3) not be dominated by any join of clocks in the system.



API
----

It exports the following functions:

*	fresh/0
*	descends/2
*	sync/2
*	get_counter/2
*	get_timestamp/2
*	update/3
*	all_nodes/1
*	equal/2
*	increment/2
*	merge/1
*	get_max_counter/2

Author
------

Article: 

*	Carlos Baquero <cbm@di.uminho.pt>
*	Paulo Sérgio Almeida <psa@di.uminho.pt>
*	Victor Fonte <vff@di.uminho.pt>
*	Nuno Preguiça <nmp@di.fct.unl.pt>
*	Ricardo Gonçalves <tome@di.uminho.pt>


Implementation: 

*	Ricardo Gonçalves <tome.wave@gmail.com>

How To Use
----------

```Erlang
		% Create a new clock	
		% C = []
        C = dottedvv:fresh(),

		% Increment count of id
		% C2 = [(id,1)]
		C2 = dottedvv:increment(id, C),
		
		% Update
		% C3 = [(id,2)]
		C3 = dottedvv:update(C2, C2, id),
		
		% Update an existing value
		C3 = dottedvv:sync(C3, C).
```

	
	
TODO
----

*	Clean code (debug code)
*	Add new cover tests
*	Change structure to 