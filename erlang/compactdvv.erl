
%%%-------------------------------------------------------------------
%%%
%%% File:      compactdvv.erl
%%%
%%% @author    Ricardo Tomé Gonçalves <tome.wave@gmail.com>
%%% @author    Paulo Sérgio Almeida <pssalmeida@gmail.com>
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
%%% @reference
%%% <a href="http://arxiv.org/abs/1011.5808">
%%% Dotted Version Vectors: Logical Clocks for Optimistic Replication
%%% </a>
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(compactdvv).

-export([new/2,
         sync/1,
         sync/2,
         syncupdate/4,
         syncupdate/3,
         join/1,
         lww/2,
         strict_descendant/2,
         value_count/1,
         equal/2,
         get_last_value/2,
         get_values/1,
         set_value/3,
         map_values/2
        ]).

-export_type([clock/0, entry/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% STRUCTURE
-type clock() :: [entry()].
-type entry() :: {id(), counter(), list()}.
-type id() :: any().
-type value() :: any().
-type counter() :: non_neg_integer().

-spec new(id(), value()) -> clock().
new(I, V) -> [{I, 0, [V]}].

-spec syncupdate(clock(), clock(), id(), value()) -> clock().
syncupdate(Cc, Cr, R, V) -> event(sync(join(Cc), Cr), R, V).


-spec syncupdate(clock(), id(), value()) -> clock().
syncupdate(Cc, R, V) -> event(join(Cc), R, V).


-spec sync(C1::clock(), C2::clock()) -> clock().
sync([], C) -> C;
sync(C, []) -> C;
sync([{I1, N1, L1}=H1 | T1]=C1, [{I2, N2, L2}=H2 | T2]=C2) -> 
    if
      I1 < I2 -> [H1 | sync(T1, C2)];
      I1 > I2 -> [H2 | sync(T2, C1)];
      true    -> [merge(I1, N1, L1, N2, L2) | sync(T1, T2)]
    end.

merge(I, N1, L1, N2, L2) ->
    N = max(N1, N2),
    LL1 = length(L1),
    LL2 = length(L2),
    if
      N1 + LL1 >= N2 + LL2 -> {I, N, lists:nthtail(N - N1, L1)};
      true                 -> {I, N, lists:nthtail(N - N2, L2)}
    end.


-spec sync([clock()]) -> clock().
sync(L) -> lists:foldl(fun sync/2, [], L).


-spec join(clock()) -> clock().
join(C) -> [{R, N+length(L), []} || {R, N, L} <- C].


-spec event(C::clock(), I::id(), V::value()) -> clock().
event([], I, V) -> [{I, 0, [V]}];
event([{I, N, L} | T], I, V) -> [{I, N, L++[V]} | T];
event([{I1, _, _} | _]=C, I, V) when I1 > I -> [{I, 0, [V]} | C];
event([H | T], I, V) -> [H | event(T, I, V)].


-spec lww(After::fun((value(),value()) -> boolean()), clock()) -> clock().
lww(_, []) -> [];
lww(F, C) -> 
    {I, V} = find_entry(F, C),
    join_and_replace(I, V, C).

find_entry(F, [{_, _, []} | T]) -> find_entry(F, T);
find_entry(F, [{I, _, L} | T]) -> find_entry(F, I, lists:last(L), T).

find_entry(_, I, V, []) -> {I, V};
find_entry(F, I, V, [{_, _, []} | T]) -> find_entry(F, I, V, T);
find_entry(F, I, V, [{I1, _, L1} | T]) ->
  V1 = lists:last(L1),
  case F(V, V1) of
      true  -> find_entry(F, I, V, T);
      false -> find_entry(F, I1, V1, T)
  end.

join_and_replace(Ir, V, C) -> 
    [if
       I == Ir -> {I, N + length(L) - 1, [V]};
       true    -> {I, N + length(L), []}
     end
     || {I, N, L} <- C].


-spec value_count(clock()) -> non_neg_integer().
value_count(C) -> lists:sum([length(L) || {_,_,L} <- C]).


-spec equal(C1::clock(), C2::clock()) -> boolean().
equal([], []) -> true;
equal([{I, C, L1} | T1], [{I, C, L2} | T2]) when length(L1) == length(L2) -> equal(T1, T2);
equal(_, _) -> false.


-spec strict_descendant(C1::clock(), C2::clock()) -> boolean().
strict_descendant(A, B) -> greater(A, B, false).

greater([], [], Strict) -> Strict;
greater([_|_], [], _) -> true;
greater([], [_|_], _) -> false;
greater([{I, N1, L1} | T1], [{I, N2, L2} | T2], Strict) ->
   Diff = (N1 + length(L1)) - (N2 + length(L2)),
   if
     Diff == 0 -> greater(T1, T2, Strict);
     Diff >  0 -> greater(T1, T2, true);
     Diff <  0 -> false
   end;
greater([{I1, _, _} | T1], [{I2, _, _} | _]=C2, _) when I1 < I2 -> greater(T1, C2, true);
greater(_, _, _) -> false.


-spec get_last_value(After::fun((value(),value()) -> boolean()), C::clock()) -> value().
get_last_value(F, C) ->
    {_, V} = find_entry(F, C),
    V.


-spec get_values(clock()) -> [value()].
get_values(C) ->
    lists:append([L || {_,_,L} <- C]).


-spec set_value(After::fun((value(),value()) -> boolean()), V::value(), C::clock()) -> clock().
set_value(F, V, C) ->
    {I, _} = find_entry(F, C),
    join_and_replace(I, V, C).


-spec map_values(fun((value()) -> value()), clock()) -> clock().
map_values(F, C) -> [ {I, N, lists:map(F, V)} || {I, N, V} <- C].



%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

%example_test() ->
%    A = new(null,[]),
%    B = new(null,[]),
%    ?assertEqual(A,B),
%    ok.

syncupdate_test() ->
    A = new(a,v1),
    A1 = syncupdate(A,A,a,v2),
    ?assertEqual([{a,1,[v2]}], A1),
    A2 = syncupdate(A1,b,v3),
    ?assertEqual([{a,2,[]}, {b,0,[v3]}], A2),
    A2b = syncupdate(A1,A2,c,v3),
    A3 = syncupdate(A2b,a,v4),
    ?assertEqual([{a,2,[v4]}, {b,1,[]}, {c,1,[]}], A3),
    ok.

join_test() ->
    A = new(a,v),
    ?assertEqual(join(A), [{a,1,[]}]),
    B = new(b,v),
    C = syncupdate(B, B, b, v1),
    ?assertEqual(join(C), [{b,2,[]}]),
    D = sync(A, C),
    ?assertEqual(join(D), [{a,1,[]}, {b,2,[]}]),
    ok.

event_test() ->
    A = new(a,v1),
    ?assertEqual(event(A,a,v2), [{a,0,[v1,v2]}]),
    ?assertEqual(event(A,b,v2), [{a,0,[v1]}, {b,0,[v2]}]),
    ok.

sync_test() ->
    X = [{x,1,[]}],
    A = new(a,v1),
    Y = new(b,v2),
    A1 = syncupdate(A,a,v2),
    R1 = sync(A,A1),
    R2 = sync(A1,A),
    ?assertEqual(R1,R2),
    A2a = syncupdate(A1,b,v3),
    A2b = syncupdate(A1,c,v3),
    R3 = sync(A2b,A2a),
    ?assertEqual(R3, [{a,2,[]}, {b,0,[v3]}, {c,0,[v3]}]),
    ?assertEqual(R3, sync([A2b,A2a])),
    ?assertEqual(sync(X,A), [{a,0,[v1]},{x,1,[]}]), %% first clock MUST have a value!
    ?assertEqual(sync(X,A), sync(A,X)),
    ?assertEqual(sync(X,A), sync([A,X])),
    %% order by id when length of values is the same
    ?assertEqual(sync(A,Y), [{a,0,[v1]},{b,0,[v2]}]),
    ?assertEqual(sync(Y,A), sync(A,Y)), 
    ?assertEqual(sync(Y,A), sync([A,Y])), 
    ?assertEqual(sync(A,X), sync([X,A])),
    ok.

%lww_test() ->
%    ?assertEqual(v5, get_last_value([{a,1,[v0,v5]},{b,1,[]},{c,1,[v7]}])),
%    ?assertEqual([{a,2,[v5]},{b,1,[]},{c,2,[]}], lww([{a,1,[v0,v5]},{b,1,[]},{c,1,[v7]}])),
%    ok.

value_count_test() ->
    ?assertEqual(3, value_count([{a,2,[v0,v5]},{b,0,[]},{c,0,[v3]}])),
    ok.

strict_descendant_test() ->
    A = new(a,v1),
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
    B = [{a,2,[v0,v555]}, {b,0,[]}, {c,0,[v3]}],
    C = [{a,2,[v0,v5]},{b,0,[]}],
    % ignore the values' contents
    ?assert(equal(A,B)),
    ?assert(equal(B,A)),
    ?assertNot(equal(A,C)),
    ?assertNot(equal(B,C)),
    ok.

get_set_test() ->
    A = [{a,2,[v0,v5]},{b,0,[]},{c,0,[v3]}],
%    ?assertEqual(get_last(A), {a,2,[v0,v5]}),
%    ?assertEqual(get_last_value(A), v5),
    ?assertEqual(get_values(A), [v0,v5,v3]),
%    ?assertEqual(set_value(A,v9), [{a,3,[v9]},{b,0,[]},{c,1,[]}]),
%    ?assertEqual(set_value([{null,0,[v1]}],v9), [{null,0,[v9]}]),
    ok.

map_values_test() ->
    A = [{a,2,[5,0]},{b,0,[]},{c,0,[2]}],
    ?assertEqual(map_values(fun (X) -> X*X end,A), [{a,2,[25,0]},{b,0,[]},{c,0,[4]}]),
    ok.


-endif.
