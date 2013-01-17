
%%%-------------------------------------------------------------------
%%%
%%% File:      dvvset.erl
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
%%% An Erlang implementation of *compact* Dotted Version Vectors, which
%%% provides a container for a set of concurrent values (siblings) with causal
%%% order information.
%%% Some functions were adapted from the vclock.erl file (for Version Vectors) of Basho's Riak.
%%% @end  
%%%
%%% @reference
%%% <a href="http://arxiv.org/abs/1011.5808">
%%% Dotted Version Vectors: Logical Clocks for Optimistic Replication
%%% </a>
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(dvvset).

-export([new/2,
         sync/1,
         sync/2,
         join/1,
         update/3,
         update/4,
         size/1,
         ids/1,
         values/1,
         equal/2,
         strict_descendant/2,
         map/2,
         last/2,
         lww/2,
         set_value/3
        ]).

-export_type([clock/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% STRUCTURE
-type clock() :: [entry()].
-type entry() :: {id(), counter(), [value()]}.
-type id() :: any().
-type value() :: any().
-type counter() :: non_neg_integer().

-spec new(id(), value()) -> clock().
new(I, V) -> [{I, 1, [V]}].

-spec sync([clock()]) -> clock().
sync(L) -> lists:foldl(fun sync/2, [], L).

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
    LL1 = length(L1),
    LL2 = length(L2),
    if
      N1 >= N2 ->
          if
            N1 - LL1 >=  N2 - LL2 -> {I, N1, L1};
            true                  -> {I, N1, lists:sublist(L1, N1 - N2 + LL2)}
          end;
      true     ->
          if
            N2 - LL2 >=  N1 - LL1 -> {I, N2, L2};
            true                  -> {I, N2, lists:sublist(L2, N2 - N1 + LL1)}
          end
    end.

-spec join(clock()) -> clock().
join(C) -> [{I, N, []} || {I, N, _} <- C].

-spec update(clock(), id(), value()) -> clock().
update(Cc, I, V) -> event(join(Cc), I, V).

-spec update(clock(), clock(), id(), value()) -> clock().
update(Cc, Cs, I, V) -> event(sync(join(Cc), Cs), I, V).

event([], I, V) -> [{I, 1, [V]}];
event([{I, N, L} | T], I, V) -> [{I, N+1, [V | L]} | T];
event([{I1, _, _} | _]=C, I, V) when I1 > I -> [{I, 1, [V]} | C];
event([H | T], I, V) -> [H | event(T, I, V)].

-spec size(clock()) -> non_neg_integer().
size(C) -> lists:sum([length(L) || {_,_,L} <- C]).

-spec ids(clock()) -> [id()].
ids(C) -> ([I || {I,_,_} <- C]).

-spec values(clock()) -> [value()].
values(C) -> lists:append([L || {_,_,L} <- C]).

-spec equal(C1::clock(), C2::clock()) -> boolean().
equal([], []) -> true;
equal([{I, C, L1} | T1], [{I, C, L2} | T2]) when length(L1) == length(L2) -> equal(T1, T2);
equal(_, _) -> false.

-spec strict_descendant(C1::clock(), C2::clock()) -> boolean().
strict_descendant(A, B) -> greater(A, B, false).

greater([], [], Strict) -> Strict;
greater([_|_], [], _) -> true;
greater([], [_|_], _) -> false;
greater([{I, N1, _} | T1], [{I, N2, _} | T2], Strict) ->
   if
     N1 == N2 -> greater(T1, T2, Strict);
     N1 >  N2 -> greater(T1, T2, true);
     N1 <  N2 -> false
   end;
greater([{I1, _, _} | T1], [{I2, _, _} | _]=C2, _) when I1 < I2 -> greater(T1, C2, true);
greater(_, _, _) -> false.

-spec map(fun((value()) -> value()), clock()) -> clock().
map(F, C) -> [ {I, N, lists:map(F, V)} || {I, N, V} <- C].

-spec last(After::fun((value(),value()) -> boolean()), C::clock()) -> value().
last(F, C) ->
    {_, V} = find_entry(F, C),
    V.

-spec lww(After::fun((value(),value()) -> boolean()), clock()) -> clock().
lww(_, []) -> [];
lww(F, C) -> 
    {I, V} = find_entry(F, C),
    join_and_replace(I, V, C).

-spec set_value(After::fun((value(),value()) -> boolean()), V::value(), C::clock()) -> clock().
set_value(F, V, C) ->
    {I, _} = find_entry(F, C),
    join_and_replace(I, V, C).

find_entry(F, [{_, _, []} | T]) -> find_entry(F, T);
find_entry(F, [{I, _, [V|_]} | T]) -> find_entry(F, I, V, T).

find_entry(_, I, V, []) -> {I, V};
find_entry(F, I, V, [{_, _, []} | T]) -> find_entry(F, I, V, T);
find_entry(F, I, V, [{I1, _, [V1|_]} | T]) ->
    case F(V, V1) of
        true  -> find_entry(F, I, V, T);
        false -> find_entry(F, I1, V1, T)
    end.

join_and_replace(Ir, V, C) -> 
    [if
       I == Ir -> {I, N, [V]};
       true    -> {I, N, []}
     end
     || {I, N, _} <- C].

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

%example_test() ->
%    A = new(null,[]),
%    B = new(null,[]),
%    ?assertEqual(A,B),
%    ok.

update_test() ->
    A = new(a,v1),
    A1 = update(A,A,a,v2),
    ?assertEqual([{a,2,[v2]}], A1),
    A2 = update(A1,b,v3),
    ?assertEqual([{a,2,[]}, {b,1,[v3]}], A2),
    A2b = update(A1,A2,c,v3),
    A3 = update(A2b,a,v4),
    ?assertEqual([{a,3,[v4]}, {b,1,[]}, {c,1,[]}], A3),
    ok.

join_test() ->
    A = new(a,v),
    ?assertEqual(join(A), [{a,1,[]}]),
    B = new(b,v),
    C = update(B, B, b, v1),
    ?assertEqual(join(C), [{b,2,[]}]),
    D = sync(A, C),
    ?assertEqual(join(D), [{a,1,[]}, {b,2,[]}]),
    ok.

event_test() ->
    A = new(a,v1),
    ?assertEqual(event(A,a,v2), [{a,2,[v2,v1]}]),
    ?assertEqual(event(A,b,v2), [{a,1,[v1]}, {b,1,[v2]}]),
    ok.

sync_test() ->
    X = [{x,1,[]}],
    A = new(a,v1),
    Y = new(b,v2),
    A1 = update(A,a,v2),
    R1 = sync(A,A1),
    R2 = sync(A1,A),
    ?assertEqual(R1,R2),
    A2a = update(A1,b,v3),
    A2b = update(A1,c,v3),
    R3 = sync(A2b,A2a),
    ?assertEqual(R3, [{a,2,[]}, {b,1,[v3]}, {c,1,[v3]}]),
    ?assertEqual(R3, sync([A2b,A2a])),
    ?assertEqual(sync(X,A), [{a,1,[v1]},{x,1,[]}]), %% first clock MUST have a value!
    ?assertEqual(sync(X,A), sync(A,X)),
    ?assertEqual(sync(X,A), sync([A,X])),
    %% order by id when length of values is the same
    ?assertEqual(sync(A,Y), [{a,1,[v1]},{b,1,[v2]}]),
    ?assertEqual(sync(Y,A), sync(A,Y)), 
    ?assertEqual(sync(Y,A), sync([A,Y])), 
    ?assertEqual(sync(A,X), sync([X,A])),
    ok.

%lww_test() ->
%    ?assertEqual(v5, last([{a,1,[v0,v5]},{b,1,[]},{c,1,[v7]}])),
%    ?assertEqual([{a,2,[v5]},{b,1,[]},{c,2,[]}], lww([{a,1,[v0,v5]},{b,1,[]},{c,1,[v7]}])),
%    ok.

size_test() ->
    ?assertEqual(3, ?MODULE:size([{a,4,[v0,v5]},{b,0,[]},{c,1,[v3]}])),
    ok.

strict_descendant_test() ->
    A = new(a,v1),
    B = update(A,a,v2),
    B2 = update(A,b,v2),
    B3 = update(A,z,v2),
    C = update(B,A,c,v3),
    D = update(C,B2,d,v5),
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
    A = [{a,4,[v0,v5]},{b,0,[]},{c,1,[v3]}],
    B = [{a,4,[v0,v555]}, {b,0,[]}, {c,1,[v3]}],
    C = [{a,4,[v0,v5]},{b,0,[]}],
    % ignore the values' contents
    ?assert(equal(A,B)),
    ?assert(equal(B,A)),
    ?assertNot(equal(A,C)),
    ?assertNot(equal(B,C)),
    ok.

get_set_test() ->
    A = [{a,4,[v0,v5]},{b,0,[]},{c,1,[v3]}],
%    ?assertEqual(get_last(A), {a,4,[v0,v5]}),
%    ?assertEqual(last(A), v5),
    ?assertEqual(values(A), [v0,v5,v3]),
%    ?assertEqual(set_value(A,v9), [{a,4,[v9]},{b,0,[]},{c,1,[]}]),
%    ?assertEqual(set_value([{null,1,[v1]}],v9), [{null,1,[v9]}]),
    ok.

map_test() ->
    A = [{a,4,[5,0]},{b,0,[]},{c,1,[2]}],
    ?assertEqual(map(fun (X) -> X*X end,A), [{a,4,[25,0]},{b,0,[]},{c,1,[4]}]),
    ok.

-endif.
