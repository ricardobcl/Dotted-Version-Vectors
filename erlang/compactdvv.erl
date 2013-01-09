
%%%-------------------------------------------------------------------
%%%
%%% File:      compactdvv.erl
%%%
%%% @author    Ricardo Tomé Gonçalves <tome.wave@gmail.com>
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%% @doc  
%%%  An Erlang implementation of the *compact* Dotted Version Vectors.
%%%  Some functions were adapted from the vclock.erl file (for Version Vectors) of Basho's Riak.
%%% @end  
%%%
%%% @reference Dotted Version Vectors: Logical Clocks for Optimistic Replication
%%% URL: http://arxiv.org/abs/1011.5808
%%%
%%%-------------------------------------------------------------------

-module(compactdvv).

-export([   new/0,
            new/1,
            new/2,
            sync/1,
            sync/2,
            syncupdate/4,
            syncupdate/3,
            join/1,
            lww/1,
            lww/2,
            strict_descendant/2,
            value_count/1,
            equal/2,
            get_last/1,
            get_last_value/1,
            get_values/1,
            set_value/2,
            map_values/2
        ]).
-export_type([clock/0, base/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% STRUCTURE
-type clock() :: [base()].
-type base() :: {id(), counter(), list()}.
-type id() :: any().
-type counter() :: non_neg_integer().

-spec new() -> clock().
new() -> new([]).

-spec new(list()) -> clock().
new(L) -> [{null, 0, L}].

-spec new(id(), list()) -> clock().
new(I,L) -> [{I, 0, L}].

-spec syncupdate(clock(), clock(), id(), any()) -> clock().
syncupdate(Cc,Cr,R,V) -> event(sync(join(Cc),Cr),R,V).


-spec syncupdate(clock(), id(), any()) -> clock().
syncupdate(C,R,V) -> event(join(C),R,V).


-spec sync(clock(), clock()) -> clock().
sync(A,B) -> 
    Eq = [ {R1,max(N1,N2),merge(N1,L1,N2,L2)} || {R2,N2,L2} <- B, {R1,N1,L1} <- A, R1 =:= R2],
    K = proplists:get_keys(Eq),
    R = Eq ++ sync_aux(K,A) ++ sync_aux(K,B),
    %% order (descending) clocks by the number of values, so the 1st has the most values
    lists:sort(fun compare_values_length/2, R). 

-spec compare_values_length(base(), base()) -> boolean().
compare_values_length({I1,_,L1},{I2,_,L2}) -> 
    if 
        length(L1) < length(L2) -> false;
        length(L1) > length(L2) -> true;
        length(L1) == length(L2) -> I1 < I2
    end.


sync_aux([],Acc) -> Acc;
sync_aux([H|T],Acc) ->
    sync_aux(T, proplists:delete(H,Acc)).

-spec sync([clock()]) -> clock().
sync(L) -> lists:foldl(fun sync/2, [], L).

-spec merge(counter(), list(), counter(), list()) -> list().
merge(M1,L1,M2,L2) ->
    case M1 + length(L1) >= M2 + length(L2) of
        true -> lists:nthtail(max(M1,M2) - M1, L1);
        false -> lists:nthtail(max(M1,M2) - M2, L2)
    end.

-spec join(clock()) -> clock().
join(C) -> [ {R,N+length(L),[]} || {R,N,L} <- C].

-spec event(clock(), id(), any()) -> clock().
event(C,I,V) -> 
    case lists:keytake(I,1,C) of
        false -> [{I,0,[V]} | C];
        {value, {I,N,L}, C2} -> [{I,N,L++[V]} | C2]
    end.

-spec lww(clock()) -> any().
lww(C) ->
    {I,_,V} = hd(C),
    C2 = join(C),
    case lists:keytake(I,1,C2) of
        %% it should find the value!
        {value, {I,N,_}, C3} -> [{I,N-1,[lists:last(V)]} | C3]
    end.

-spec lww(fun ((A,A) -> boolean()), clock()) -> clock().
lww(F,C) -> 
    {I,_,V} = 
        hd(lists:sort(fun({_,_,V1},{_,_,V2}) -> F(lists:last(V1),lists:last(V2)) end, C)),
    C2 = join(C),
    case lists:keytake(I,1,C2) of
        %% it should find the value!
        {value, {I,N,_}, C3} -> [{I,N-1,[lists:last(V)]} | C3]
    end.

-spec value_count(clock()) -> non_neg_integer().
value_count([]) -> 0;
value_count(C) -> lists:sum([length(L) || {_,_,L} <- C]).


-spec equal(clock(), clock()) -> boolean().
equal(A,B) -> 
    A2 = lists:map(fun ({I,C,L}) -> {I,C,length(L)} end, A),
    B2 = lists:map(fun ({I,C,L}) -> {I,C,length(L)} end, B),
    lists:usort(A2) == lists:usort(B2).

%% maybe optimize by comparing with the "dot"
-spec strict_descendant(clock(), clock()) -> boolean().
strict_descendant(A,B) -> 
    A2 = lists:usort(lists:map(fun ({I,C,L}) -> {I,C+length(L)} end, A)),
    B2 = lists:usort(lists:map(fun ({I,C,L}) -> {I,C+length(L)} end, B)),
    LA = length(A2),
    LB = length(B2),
    (LA >= LB) andalso strict_descendant2(A2,B2,LA,LB,false).

strict_descendant2(_,[],_,0,Flag) -> Flag;
strict_descendant2(_,_,LA,LB,_) when LB > LA -> false;
strict_descendant2([{I,CA}|TA],[{I,CB}|TB],LA,LB,Flag) ->
    if
        (CB>CA) -> 
            false;
        (CB=:=CA) ->  
            if
                (LA > LB) ->
                    strict_descendant2(TA,TB,LA-1,LB-1,true);
                (LA =< LB) ->
                    strict_descendant2(TA,TB,LA-1,LB-1,Flag)
            end;
        (CB<CA) ->
            strict_descendant2(TA,TB,LA-1,LB-1,true)
    end;
strict_descendant2([_|TA],B,LA,LB,Flag) ->
    strict_descendant2(TA,B,LA-1,LB,Flag).

-spec get_last_value(clock()) -> any().
get_last_value(C) ->
    {_,_,L} = hd(C),
    lists:last(L).

-spec get_last(clock()) -> base().
get_last(C) -> hd(C).

-spec get_values(clock()) -> [any()].
get_values(C) ->
    lists:append([L || {_,_,L} <- C]).

-spec set_value(clock(),any()) -> clock().
set_value(Clock,V) ->
    [{I,C,[]}|T] = join(Clock),
    [{I,C-1,[V]}|T].

-spec map_values(fun ((any()) -> any()), clock()) -> clock().
map_values(F,Cl) -> [ {I,C,lists:map(F,V)} || {I,C,V} <- Cl].



%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

example_test() ->
    A = new(),
    B = new(),
    ?assertEqual(A,B),
    ok.

syncupdate_test() ->
    A = new(a,[v1]),
    A1 = syncupdate(A,A,a,v2),
    ?assertEqual([{a,1,[v2]}], A1),
    A2 = syncupdate(A1,b,v3),
    ?assertEqual([{b,0,[v3]},{a,2,[]}], A2),
    A2b = syncupdate(A1,A2,c,v3),
    A3 = syncupdate(A2b,a,v4),
    ?assertEqual([{a,2,[v4]},{c,1,[]},{b,1,[]}], A3),
    ok.

join_test() ->
    A = new(),
    B = new(a,[a,b]),
    ?assertEqual(join(A), [{null,0,[]}]),
    ?assertEqual(join(B), [{a,2,[]}]),
    ok.

event_test() ->
    A = new(a,[v1]),
    ?assertEqual(event(A,a,v2), [{a,0,[v1,v2]}]),
    ?assertEqual(event(A,b,v2), [{b,0,[v2]},{a,0,[v1]}]),
    ok.

sync_test() ->
    X = [{x,1,[]}],
    A = new(a,[v1]),
    Y = new(b,[v2]),
    A1 = syncupdate(A,a,v2),
    R1 = sync(A,A1),
    R2 = sync(A1,A),
    ?assertEqual(R1,R2),
    A2a = syncupdate(A1,b,v3),
    A2b = syncupdate(A1,c,v3),
    R3 = sync(A2b,A2a),
    ?assertEqual(lists:sort(R3), [{a,2,[]},{b,0,[v3]},{c,0,[v3]}]),
    ?assertEqual(lists:sort(R3), lists:sort(sync([A2b,A2a]))),
    ?assertEqual(sync(X,A), [{a,0,[v1]},{x,1,[]}]), %% first clock MUST have a value!
    ?assertEqual(sync(X,A), sync(A,X)),
    %% order by id when length of values is the same
    ?assertEqual(sync(A,Y), [{a,0,[v1]},{b,0,[v2]}]),
    ?assertEqual(sync(Y,A), sync(A,Y)), 

    ok.

lww_test() ->
    ?assertEqual(v5, get_last_value([{a,1,[v0,v5]},{b,1,[]},{c,1,[v7]}])),
    ?assertEqual([{a,2,[v5]},{b,1,[]},{c,2,[]}], lww([{a,1,[v0,v5]},{b,1,[]},{c,1,[v7]}])),
    ok.

value_count_test() ->
    ?assertEqual(3, value_count([{a,2,[v0,v5]},{b,0,[]},{c,0,[v3]}])),
    ok.

strict_descendant_test() ->
    A = new(a,[v1]),
    B = syncupdate(A,a,v2),
    B2 = syncupdate(A,b,v2),
    B3 = syncupdate(A,z,v2),
    C = syncupdate(B,A,c,v3),
    D = syncupdate(C,B2,d,v5),
    ?assert(strict_descendant(B,A)),
    ?assert(strict_descendant(C,A)),
    ?assert(strict_descendant(C,B)),
    ?assert(strict_descendant(D,B)),
    ?assert(strict_descendant(D,B2)),
    ?assert(strict_descendant(D,A)),
    ?assertNot(strict_descendant(C,B2)),
    ?assertNot(strict_descendant(B2,B)),
    ?assertNot(strict_descendant(B,B2)),
    ?assertNot(strict_descendant(A,A)),
    ?assertNot(strict_descendant(C,C)),
    ?assertNot(strict_descendant(B2,D)),
    ?assertNot(strict_descendant(D,B3)),
    ok.

equal_test() ->
    A = [{a,2,[v0,v5]},{b,0,[]},{c,0,[v3]}],
    B = [{b,0,[]},{a,2,[v0,v555]},{c,0,[v3]}],
    C = [{a,2,[v0,v5]},{b,0,[]}],
    % ignore the values' contents
    ?assert(equal(A,B)),
    ?assert(equal(B,A)),
    ?assertNot(equal(A,C)),
    ?assertNot(equal(B,C)),
    ok.

get_set_test() ->
    A = [{a,2,[v0,v5]},{b,0,[]},{c,0,[v3]}],
    ?assertEqual(get_last(A), {a,2,[v0,v5]}),
    ?assertEqual(get_last_value(A), v5),
    ?assertEqual(get_values(A), [v0,v5,v3]),
    ?assertEqual(set_value(A,v9), [{a,3,[v9]},{b,0,[]},{c,1,[]}]),
    ?assertEqual(set_value([{null,0,[v1]}],v9), [{null,0,[v9]}]),
    ok.

map_values_test() ->
    A = [{a,2,[5,0]},{b,0,[]},{c,0,[2]}],
    ?assertEqual(map_values(fun (X) -> X*X end,A), [{a,2,[25,0]},{b,0,[]},{c,0,[4]}]),
    ok.


-endif.
