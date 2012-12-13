
%%-------------------------------------------------------------------
%%
%% File:      dottedvv.erl
%%
%% @author    Ricardo Tomé Gonçalves <tome.wave@gmail.com>
%%
%% @copyright 2012 Ricardo Tomé Gonçalves 
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
%%  A simple Erlang implementation of Dotted Version Vectors.
%%  Some functions were adapted from the vclock.erl file (for Version Vectors) of Basho's Riak.
%% @end  
%%
%% @reference Dotted Version Vectors: Logical Clocks for Optimistic Replication
%% URL: http://arxiv.org/abs/1011.5808
%%
%%-------------------------------------------------------------------

-module(dottedvv).

-author('Ricardo Tome Goncalves <tome@di.uminho.pt>').

-export([fresh/0,strict_descends/2,descends/2,sync/2,update/3,equal/2,increment/2,merge/1]).

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

update(Sc, Sr, Id) -> update2(merge(Sc), Sr, Id).
update2({}, {}, Id) -> {[], {Id, {1 , new_timestamp()}}};
update2({}, Sr, Id) -> 
    {Max, _TS2} = max_counter(Id, Sr),
    Dot = {Id, {Max + 1 , new_timestamp()}},
    {[], Dot};
update2(Sc, Sr, Id) ->
    {Sc2, null} = Sc,
    {Max, _TS2} = max_counter(Id, Sr),
    Dot = {Id, {Max + 1 , new_timestamp()}},
    {Sc2, Dot}.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Increment DottedVV at Node.
-spec increment(id(), dottedvv()) -> dottedvv().
increment(Id, C) ->
    update(C, C, Id).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Return true if Va is a direct descendant of Vb, else false --> S1 >= S2
-spec descends(dottedvv(), dottedvv()) -> boolean().
descends(A, B) -> equal(A, B) orelse strict_descends(A, B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Return true if Va is a direct descendant of Vb, else false --> S1 > S2
-spec strict_descends(dottedvv(), dottedvv()) -> boolean().
strict_descends([{}], _) -> false;
strict_descends({}, _) -> false;
strict_descends(_, [{}]) -> true;
strict_descends(_, {}) -> true;
strict_descends(A, B) -> (equal(A, B) == false) andalso descends2(A, B).

 % test if both have a valid dot to compare
descends2({V,{I,{C,TA}}}, {V,{I,{C,TB}}}) -> (TA > TB);
descends2({VA,_DA}, {_VB,{IB,{CB,_TB}}}) ->
    case lists:keyfind(IB, 1, VA) of
        {_, {CA, _TA}} ->    
            (CA >= CB); % orelse ((CA =:= CB) and (TA > TB));
        false -> false %% they are not equal, as it was tested in strict_descends
    end;
descends2(A, B) -> 
    {VA, null} = merge(A),
    {VB, null} = merge(B),
    descends3(VA, VB).

descends3(A, A) -> true;
descends3(_, []) -> true;
descends3(Va, Vb) ->
    [{NodeB, {CtrB, _T}}|RestB] = Vb,
    case lists:keyfind(NodeB, 1, Va) of
        false ->
            false;
        {_, {CtrA, _TSA}} ->
            (CtrA >= CtrB) andalso descends3(Va,RestB)
        end.


            


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Merges the set of clocks, removing redundant information (old entries)
% We assume that all clocks togethor have no "holes"
-spec merge([dottedvv()]) -> dottedvv() | {}.
merge([]) -> {};
merge({}) -> {};
merge([{}|S]) -> merge(S);
merge(S={_,_}) -> merge_dot(S);
merge([S={_,_}]) -> merge_dot(S);
merge(S) -> 
    [First|Rest] = lists:flatten(S),
    merge(Rest, sort_and_merge_dot(First)).

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
%%%%%%%%%%%%%%%%%%%% sync(S, {u}) -> S
% @doc  Takes two clock sets and returns a clock set. 
%       It returns a set of concurrent clocks, 
%       each belonging to one of the sets, and that 
%       together cover both sets while discarding obsolete knowledge.
-spec sync([dottedvv()], [dottedvv()]) -> [dottedvv()].
sync({}, S) -> S;
sync(S, {}) -> S;
sync(S1={_,_}, S2) -> sync(S2, [S1]);
sync(S1, S2={_,_}) -> sync(S1, [S2]);
sync(S1, S2) ->
    NotLeq = [X || X <- S2, lists:all(fun (Y) -> not descends(Y, X) end, S1)],
    NotLess = [X || X <- S1, lists:all(fun (Y) -> not strict_descends(Y, X) end, NotLeq)],
    lists:reverse(NotLeq, NotLess).




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
equal([], B) -> equal({}, B);
equal(A, []) -> equal(A, {});
equal([A], B) -> equal(A, B);
equal(A, [B]) -> equal(A, B);
equal({}, {}) -> true;
equal({}, _) -> false;
equal(_, {}) -> false;
equal({SA,DA}, {SB,DB}) -> DA =:= DB andalso lists:sort(SA) =:= lists:sort(SB);
equal([_], {_,_}) -> false;
equal({_,_}, [_]) -> false;
equal(A, B) -> 
    contains(A, B) andalso contains(B, A).


contains([], _) -> true;
contains({_,_}, []) -> false;
contains({SA,DA}, {SB,DB}) -> DA =:= DB andalso lists:sort(SA) =:= lists:sort(SB);
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
    true = strict_descends(A1,A),
    true = strict_descends(B1,B),
    false = strict_descends(A1,B1),
    A2 = increment(a, A1),
    C = merge([A2, B1]),
    C1 = increment(c, C),
    false = equal(C1, A2),
    true = strict_descends(C1, A2),
    true = strict_descends(C1, B1),
    false = strict_descends(B1, C1),
    false = strict_descends(B1, A1),
    ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for UPDATE

%% NOTE: increment(id,X) = update(X,X,id)
update_test() ->
    C0 = fresh(),
    C1 = increment(a, C0),
    {[], {a,{1,_}}} = C1, 
    C2 = increment(a, C1),
    {[{a,{1,_}}], {a,{2,_}}} = C2,
    C3 = increment(a, C2),
    {[{a,{2,_}}], {a,{3,_}}} = C3,
    C7 = increment(a, C3),
    {[{a,{3,_}}], {a,{4,_}}} = C7,
    C8a = increment(b, C7),
    {[{a,{4,_}}], {b,{1,_}}} = C8a,
    C8b = update(C7, C8a, b),
    {[{a,{4,_}}], {b,{2,_}}} = C8b,
    C8z = update({}, C8a, b),
    {[], {b,{2,_}}} = C8z,
    C8c = update(C7, [C8a, C8b], b),
    {[{a,{4,_}}], {b,{3,_}}} = C8c,
    C8d = update(C7, [C8a, C8b, C8c], b),
    {[{a,{4,_}}], {b,{4,_}}} = C8d,
    C7a = update(C7, [C8a, C8b, C8c, C8d], a),
    {[{a,{4,_}}], {a,{5,_}}} = C7a,
    C8e = update(C7a, [C8a, C8b, C8c, C8d], b),
    {[{a,{5,_}}], {b,{5,_}}} = C8e,
    C8f = update(C8e, [C8a, C8b, C8c, C8d, C8e], b),
    {[{a,{5,_}},{b,{5,_}}], {b,{6,_}}} = C8f,
    C9 = update(C8f, [C8a, C8b, C8c, C8d, C8f], a),
    {[{a,{5,_}},{b,{6,_}}], {a,{6,_}}} = C9, 
    ok.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for DESCENDS

descends_test() ->
    C1 = {[], {a,{1,1}}},
    C2 = {[], {a,{1,2}}},
    false = equal(C1, C2),
    false = strict_descends(C1, C2),
    true = strict_descends(C2, C1),
    C3 = increment(a, C2),
    C4 = increment(a, C3),
    C5 = increment(a, C4),
    true = strict_descends(C5, C3),
    true = strict_descends(C5, C4),
    false = strict_descends(C5, C5),
    true = descends(C5, C5),
    false = strict_descends(C4, C5),
    false = strict_descends(C1, C5),
    C6 = increment(b, C5),
    C7 = increment(a, C6),
    C8a = increment(b, C7),
    C8b = update(C7, C8a, b),
    true = strict_descends(C8a, C3),
    true = strict_descends(C8b, C5),
    true = strict_descends(C8a, C6),
    true = strict_descends(C8b, C7),
    false = strict_descends(C8a, C8a),
    true = descends(C8a, C8a),
    false = strict_descends(C8b, C8b),
    true = descends(C8b, C8b),
%   io:format("~nCa ~p~nCb ~p~n",[C8a,C8b]),
    false = strict_descends(C8a, C8b),
    false = strict_descends(C8b, C8a),
    false = strict_descends(C1, C8a),
    false = strict_descends(C4, C8b),
    false = strict_descends(C5, C8a),
    false = strict_descends(C6, C8b),
    false = strict_descends(C7, C8a),
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


sync_test() ->
    C1 = {[], {a,{1,1}}},
    C2 = {[], {a,{1,2}}},
    C3 = increment(a, C2),
    C4 = increment(a, C3),
    C5 = increment(a, C4),
    C6 = increment(b, C5),
    C7 = increment(a, C6),
    C8a = increment(b, C7),
    C8b = update(C7, C8a, b),
    C8c = update(C7, [C8a, C8b], b),
    C8d = update(C7, [C8a, C8b, C8c], a),
    C9 = update([C8a, C8b, C8c, C8d], [C8a, C8b, C8c, C8d], c),
    [C2] = sync(C1, C2),
    [C4] = sync(C3, C4),
    [C6] = sync(C3, C6),
    [C7] = sync(C6, C7),
    [C7] = sync(C7, C7),
    [C8a] = sync(C7, C8a),
    [C8b] = sync(C7, C8b),
    [C8c] = sync(C7, C8c),
    [C8a,C8b] = lists:sort(sync(C8a, C8b)),
    [C8a,C8b,C8c] = lists:sort(sync([C8c, C8a],C8b)),
    Res = lists:sort(sync([C8d,C8a,C8b],C8c)),
    Res = lists:sort([C8a,C8b,C8c,C8d]),
    [C9] = sync([C8d,C8a,C8b,C8c],C9),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for EQUAL

%% TODO

equal_test() ->
    C1 = {[], {a,{1,1}}},
    C2 = {[], {a,{1,2}}},
    C3 = increment(a, C2),
    C4 = increment(a, C3),
    C5 = increment(b, C4),
    C6 = increment(b, C5),
    C6b = increment(b, C5),
    false = equal(C1,C2),
    true = equal(C1,C1),
    false = equal(C2,C1),
    false = equal(C3,C2),
    false = equal(C4,C3),
    false = equal(C5,C4),
    false = equal(C6,C5),
    true = equal(C6,C6b),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doc Unit Tests for others fucntions

%% TODO


-endif.



