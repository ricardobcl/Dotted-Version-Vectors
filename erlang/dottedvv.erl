
%%-------------------------------------------------------------------
%%
%% File:      dottedvv.erl
%%
%% @author    Ricardo Gonçalves <tome.wave@gmail.com>
%%
%% @copyright 2011 Ricardo Gonçalves 
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc  
%%	A simple Erlang implementation of Dotted Version Vectors.
%%  Some functions were adapted from the vclock.erl file (for Version Vectors) of Basho's Riak.
%% @end  
%%
%% @reference Dotted Version Vectors: Logical Clocks for Optimistic Replication
%% URL: http://arxiv.org/abs/1011.5808
%%
%%-------------------------------------------------------------------

-module(dottedvv).

-author('Ricardo Goncalves <tome@di.uminho.pt>').

-export([fresh/0,descends/2,sync/2,update/3,equal/2,increment/2,merge/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-export_type([dottedvv/0, timestamp/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STRUCTURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type dottedvv() :: {vector(), dot()}.
-type vector() :: [dot()].
-type dot() :: {id(), {counter(), timestamp()}} | null.

-type id() :: term().
-type counter() :: integer().
-type timestamp() :: integer().





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Returns a new dottedvv.
-spec fresh() -> dottedvv().

fresh() -> {}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% update(Sc,Sr,r) -> S

% @doc Returns a clock that newer that the client and server clocks at Id.
% Sc = Set of clocks from the Client
% Sr = Set of clocks from the Replica
% Id = Id that will be incremented (Replica id)
-spec update(Sc :: [dottedvv()], Sr :: [dottedvv()], IDr :: id()) -> dottedvv().

update(Sc, Sr, Id) when is_list(Sc) -> update(merge(Sc), Sr, Id);
update(Sc, Sr, Id) when is_list(Sr) -> update(Sc, merge(Sr), Id);
update(Sc, Sr, Id) ->
	{MaxC, _TS} = max_counter(Id, Sc),
	{MaxR, _TS} = max_counter(Id, Sr),
	Max = max(MaxC, MaxR),
	V = [{Id2, max_counter(Id2, Sc)} || Id2 <- ids(Sc)],
	Dot = {Id, {Max + 1 , new_timestamp()}},
	{V, Dot}.
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Increment DottedVV at Node.
-spec increment(id(), dottedvv()) -> dottedvv().
increment(Id, C) ->
	update(C, C, Id).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Return true if Va is a direct descendant of Vb, else false -- remember, a dottedvv is its own descendant!
-spec descends(dottedvv(), dottedvv()) -> boolean().
			
 % all clocks descend from the empty clock
descends(_, {}) -> true;
descends(A, B) -> equal(A,B) orelse descends2(A,B).

descends2({V,{I,{C,TA}}}, {V,{I,{C,TB}}}) -> (TA >= TB);
descends2({V,{I,{CA,_TA}}}, {V,{I,{CB,_TB}}}) -> (CA > CB);
descends2({VA,_DA}, {_VB,{IB,{CB,TB}}}) ->
	case lists:keyfind(IB, 1, VA) of
		{_, {CA, TA}} ->	
			(CA > CB) orelse ((CA =:= CB) and (TA >= TB));
		false -> false %% they are not equal, as it was tested in descends
    end.

			


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Merges the set of clocks, removing redundant information (old entries)
% We assume that all clocks togethor have no "holes"
-spec merge([dottedvv()]) -> dottedvv() | {}.
merge([]) -> {};
merge({}) -> {};
merge(S={_,_}) -> merge_dot(S);
merge([S={_,_}]) -> merge_dot(S);
merge([First|Rest]) -> merge(Rest, sort_and_merge_dot(First)).

merge([], NClock) -> {NClock, null};
merge([AClock|VClocks], NClock) ->
    merge(VClocks, merge(sort_and_merge_dot(AClock), NClock, [])).

merge([], [], AccClock) -> lists:reverse(AccClock);
merge([], Left, AccClock) -> lists:reverse(AccClock, Left);
merge(Left, [], AccClock) -> lists:reverse(AccClock, Left);
merge(V=[{Node1,{Ctr1,TS1}=CT1}=NCT1|VClock],
      N=[{Node2,{Ctr2,TS2}=CT2}=NCT2|NClock], AccClock) ->
    if Node1 < Node2 ->
            merge(VClock, N, [NCT1|AccClock]);
       Node1 > Node2 ->
            merge(V, NClock, [NCT2|AccClock]);
       true ->
            ({_Ctr,_TS} = CT) = if Ctr1 > Ctr2 -> CT1;
                                   Ctr1 < Ctr2 -> CT2;
                                   true        -> {Ctr1, erlang:max(TS1,TS2)}
                                end,
            merge(VClock, NClock, [{Node1,CT}|AccClock])
    end.



%% AUX
sort_and_merge_dot(S) -> 
	{S2, null} = merge_dot(S),
	lists:keysort(1, S2).
	

%% AUX 2
merge_dot({S, null}) -> {S, null};
merge_dot({S, {Id, C}}) -> {lists:keystore(Id, 1, S, {Id, C}), null}.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% sync(S1,S2) -> S
% @doc  Takes two clock sets and returns a clock set. 
%		It returns a set of concurrent clocks, 
%		each belonging to one of the sets, and that 
%		together cover both sets while discarding obsolete knowledge.
-spec sync([dottedvv()], [dottedvv()]) -> [dottedvv()].
sync(S1={_,_},S2) -> sync([S1],S2);
sync(S1,S2={_,_}) -> sync(S1,[S2]);
sync(Set1, Set2) -> 
	S = Set1 ++ Set2,
	SU = [ sets:to_list(sets:from_list(B)) || B <- S],
	Old = [[S2 || S2 <- SU,
%%		equal(S1,S2) == false,
 		descends(S1,S2)]
                || S1 <- SU],
	%io:format("Old: ~p~n", [Old]),
    Old2 = lists:flatten(Old),
    VOld = sets:from_list(Old2),
    VS = sets:from_list(SU),
	%io:format("Old2: ~p~n", [Old2]),
    VRes = sets:subtract(VS, VOld),
	sets:to_list(VRes).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% ids(X) -> [id]
% @doc Return the list of all nodes that have ever incremented dottedvv.
-spec ids(dottedvv()) -> [id()].

ids(S) when is_list(S) -> ids2(merge(S));
ids(S) -> sets:to_list(sets:from_list(ids2(S))).

ids2({}) -> [];
ids2([]) -> [];
ids2({S, {Id,_}}) -> [Id] ++ ids2(S);
ids2({[], _}) -> [];
ids2({S, _}) -> [X || {X,{_,_}} <- S];
ids2(S) when is_list(S) -> [X || {X,{_,_}} <- S].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% [S]r -> max(Sr)
%% Note: Could be improved if we checked all dots before merging (if it is a set)
-spec max_counter(id(), [dottedvv()]) -> counter().

max_counter(Id, S) when is_list(S) -> max_counter(Id, merge(S));
max_counter(Id, S) -> get_counter(Id, S).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Get the counter value and timestamp in dottedvv set from Id.
-spec get_counter(id(), dottedvv()) -> counter().
get_counter(_, {}) -> {0,0};
get_counter(Id, {_, {Id, C}}) -> C;
get_counter(Id, {S, _}) ->
    case lists:keyfind(Id, 1, S) of
		{_, CT} -> CT;
		false -> {0,0}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Get the timestamp value in a dottedvv set from Id.
-spec get_timestamp(id(), dottedvv()) -> timestamp() | undefined.
get_timestamp(_, {}) -> undefined;
get_timestamp(Id, {_,{Id,{_,TS}}}) -> TS;
get_timestamp(Id, {S,_}) ->
    case lists:keyfind(Id, 1, S) of
		{_, {_, TS}} -> TS;
		false -> undefined
    end.




-define(DAYS_FROM_GREGORIAN_BASE_TO_EPOCH, (1970*365+478)).
-define(SECONDS_FROM_GREGORIAN_BASE_TO_EPOCH,
	(?DAYS_FROM_GREGORIAN_BASE_TO_EPOCH * 24*60*60)
	%% == calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
       ).

% @doc Return a timestamp for a dotted vector clock
-spec new_timestamp() -> timestamp().
new_timestamp() ->
    %% Same as calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    %% but significantly faster.
    {MegaSeconds, Seconds, _} = os:timestamp(),
    ?SECONDS_FROM_GREGORIAN_BASE_TO_EPOCH + MegaSeconds*1000000 + Seconds.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Compares two dottedvvs for equality.
-spec equal(Dottedvv :: [dottedvv()], Dottedvv :: [dottedvv()]) -> boolean().
equal({}, {}) -> true;
equal({}, _) -> false;
equal(_, {}) -> false;
equal({SA,DA}, {SB,DB}) -> DA =:= DB andalso lists:sort(SA) =:= lists:sort(SB);
equal([_], {_,_}) -> false;
equal({_,_}, [_]) -> false;
equal(A, B) -> contains(A, B) andalso contains(B, A).


contains([], []) -> true;
contains({_,_}, []) -> false;
contains(A={_,_}, [H|T]) -> equal(A,H) orelse contains(A, T);
contains([H|T], B) -> contains(H,B) andalso contains(T,B).

	







%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Serves as both a trivial test and some example code.
example_test() ->
    A = fresh(),
    B = fresh(),
    A1 = increment(a, A),
    B1 = increment(b, B),
    true = descends(A1,A),
    true = descends(B1,B),
    false = descends(A1,B1),
    A2 = increment(a, A1),
    C = merge([A2, B1]),
    C1 = increment(c, C),
	%io:format("C1 ~p  A2 ~p \n",[C1,A2]),
    true = descends(C1, A2),
    true = descends(C1, B1),
    false = descends(B1, C1),
    false = descends(B1, A1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for DESCENDS

descends_test() ->
	C1 = {[], {a,{1,1}}},
	C2 = {[], {a,{1,2}}},
	false = equal(C1, C2),
	false = descends(C1, C2),
	true = descends(C2, C1),
    C3 = increment(a, C2),
    C4 = increment(a, C3),
    C5 = increment(a, C4),
	true = descends(C5, C3),
	true = descends(C5, C4),
	true = descends(C5, C5),
	false = descends(C4, C5),
	false = descends(C1, C5),
    C6 = increment(b, C5),
    C7 = increment(a, C6),
    C8 = increment(b, C7),
	true = descends(C8, C3),
	true = descends(C8, C5),
	true = descends(C8, C6),
	true = descends(C8, C7),
	true = descends(C8, C8),
	false = descends(C1, C8),
	false = descends(C4, C8),
	false = descends(C5, C8),
	false = descends(C6, C8),
	false = descends(C7, C8),
	ok.

accessor_test() ->
    C = {[{<<"1">>,{1,1}}], {<<"2">>,{2,2}}},
    ?assertEqual({1,1}, get_counter(<<"1">>, C)),
    ?assertEqual(1, get_timestamp(<<"1">>, C)),
    ?assertEqual({2,2}, get_counter(<<"2">>, C)),
    ?assertEqual(2, get_timestamp(<<"2">>, C)),
    ?assertEqual({0,0}, get_counter(<<"3">>, C)),
    ?assertEqual(undefined, get_timestamp(<<"3">>, C)),
    ?assertEqual([<<"1">>, <<"2">>], lists:sort(ids(C))).


%% TODO: MORE TESTS


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for MERGE
merge_test() ->
    C1 = {[{<<"1">>, {1, 1}},
           {<<"2">>, {2, 2}}],
			{<<"4">>, {4, 4}}},
    C2 = {[{<<"3">>, {3, 3}}],
           {<<"4">>, {3, 3}}},
    ?assertEqual({}, merge(fresh())),
    {C3,null} = merge([C1,C2]),
    ?assertEqual([{<<"1">>,{1,1}},{<<"2">>,{2,2}},{<<"3">>,{3,3}},{<<"4">>,{4,4}}],
                 lists:sort(C3)).

merge_less_left_test() ->
    C1 = {[],{<<"5">>, {5, 5}}},
    C2 = {[{<<"6">>, {6, 6}}], {<<"7">>, {7, 7}}},
    {C3,null} = merge([C1,C2]),
    ?assertEqual([{<<"5">>, {5, 5}},{<<"6">>, {6, 6}}, {<<"7">>, {7, 7}}],
                 lists:sort(C3)).

merge_less_right_test() ->
    C1 = {[{<<"6">>, {6, 6}}], {<<"7">>, {7, 7}}},
    C2 = {[],{<<"5">>, {5, 5}}},
    {C3,null} = merge([C1,C2]),
    ?assertEqual([{<<"5">>, {5, 5}},{<<"6">>, {6, 6}}, {<<"7">>, {7, 7}}],
                 lists:sort(C3)).

merge_same_id_test() ->
    C1 = {[{<<"1">>, {1, 2}}],{<<"2">>,{1,4}}},
    C2 = {[{<<"1">>, {1, 3}}],{<<"3">>,{1,5}}},
    {C3,null} = merge([C1,C2]),
    ?assertEqual([{<<"1">>, {1, 3}},{<<"2">>,{1,4}},{<<"3">>,{1,5}}],
                 lists:sort(C3)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for SYNC

%% TODO





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for UPDATE

%% TODO




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for EQUAL

%% TODO




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for others fucntions

%% TODO


-endif.



