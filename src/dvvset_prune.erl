
%%%-------------------------------------------------------------------
%%%
%%% File:      dvvset_prune.erl
%%%
%%% @title Dotted Version Vector Set - with Pruning
%%% @author    Ricardo Tomé Gonçalves <tome.wave@gmail.com>
%%% @author    Paulo Sérgio Almeida <pssalmeida@gmail.com>
%%%
%%% @copyright The MIT License (MIT)
%%% Copyright (C) 2013
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @doc  
%%% Equal to dvvset.erl but with logical timestamps to prune older entries.
%%%
%%% For further reading, visit the <a href="https://github.com/ricardobcl/Dotted-Version-Vectors">github page</a>.
%%% @end  
%%%
%%% @reference
%%% <a href="http://arxiv.org/abs/1011.5808">
%%% Dotted Version Vectors: Logical Clocks for Optimistic Replication
%%% </a>
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(dvvset_prune).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1,
         new/2,
         new_list/1,
         new_list/2,
         sync/1,
         join/1,
         update/2,
         update/3,
         size/1,
         ids/1,
         values/1,
         equal/2,
         less/2,
         map/2,
         last/2,
         lww/2,
         reconcile/2,
         prune/2,
         update_time/2
        ]).

-export_type([clock/0, vector/0, id/0, value/0, logical_time/0]).

% % @doc
%% STRUCTURE details:
%%      * entries() are sorted by id()
%%      * each counter() also includes the number of values in that id()
%%      * the values in each triple of entries() are causally ordered and each new value goes to the head of the list

-type clock()         :: {entries(), values()}.
-type vector()        :: [{id(), counter(), logical_time()}].
-type entries()       :: [{id(), counter(), values(), logical_time()}].
-type id()            :: any().
-type values()        :: [value()].
-type value()         :: any().
-type counter()       :: non_neg_integer().
-type logical_time()  :: pos_integer().


%% @doc Constructs a new clock set without causal history,
%% and receives one value that goes to the anonymous list.
-spec new(value()) -> clock().
new(V) -> {[], [V]}.

%% @doc Same as new/1, but receives a list of values, instead of a single value.
-spec new_list([value()]) -> clock().
new_list(Vs) when is_list(Vs) -> {[], Vs};
new_list(V) -> {[], [V]}.

%% @doc Constructs a new clock set with the causal history
%% of the given version vector / vector clock,
%% and receives one value that goes to the anonymous list.
%% The version vector SHOULD BE the output of join/1.
-spec new(vector(), value()) -> clock().
new(VV, V) ->
    VVS = lists:sort(VV), % defense against non-order preserving serialization
    {[{I, N, [], T} || {I, N, T} <- VVS], [V]}.

%% @doc Same as new/2, but receives a list of values, instead of a single value.
-spec new_list(vector(), [value()]) -> clock().
new_list(VV, Vs) when is_list(Vs) ->
    VVS = lists:sort(VV), % defense against non-order preserving serialization
    {[{I, N, [], T} || {I, N, T} <- VVS], Vs};
new_list(VV, V) -> new_list(VV, [V]).

%% @doc Synchronizes a list of clocks using sync/2.
%% It discards (causally) outdated values, 
%% while merging all causal histories.
-spec sync([clock()]) -> clock().
sync(L) -> lists:foldl(fun sync/2, {}, L).

%% Private function
-spec sync(clock(), clock()) -> clock().
sync({}, C) -> C;
sync(C ,{}) -> C;
sync(C1={E1,V1},C2={E2,V2}) ->
    V = case less(C1,C2) of
        true  -> V2; % C1 < C2 => return V2
        false ->
            case less(C2,C1) of
                true  -> V1; % C2 < C1 => return V1
                false -> % keep all unique anonymous values and sync entries()
                    sets:to_list(sets:from_list(V1++V2))
            end
    end,
    {sync2(E1,E2),V}.

%% Private function
-spec sync2(entries(), entries()) -> entries().
sync2([], C) -> C;
sync2(C, []) -> C;
sync2([{I1, N1, L1, LT1}=H1 | T1]=C1, [{I2, N2, L2, LT2}=H2 | T2]=C2) ->
    if
      I1 < I2 -> [H1 | sync2(T1, C2)];
      I1 > I2 -> [H2 | sync2(T2, C1)];
      true    -> [merge(I1, N1, L1, N2, L2, max(LT1,LT2)) | sync2(T1, T2)]
    end.

%% Private function
-spec merge(id(), counter(), values(), counter(), values(), logical_time()) 
    -> {id(), counter(), values(), logical_time()}.
merge(I, N1, L1, N2, L2, LT) ->
    LL1 = length(L1),
    LL2 = length(L2),
    case N1 >= N2 of
        true ->
          case N1 - LL1 >=  N2 - LL2 of 
            true  -> {I, N1, L1, LT};
            false -> {I, N1, lists:sublist(L1, N1 - N2 + LL2), LT}
          end;
        false ->
          case N2 - LL2 >=  N1 - LL1 of 
            true  -> {I, N2, L2, LT};
            false -> {I, N2, lists:sublist(L2, N2 - N1 + LL1), LT}
          end
    end.

%% @doc Receives a clock and an ID. The entry with that ID (if it exists)
%% will have its LT set equal to the maximum LT in the entire clock.
-spec update_time(clock(), id()) -> clock().
update_time({C,A}, I) ->
    LT = max_LT(C),
    C2 = [if I == Ir -> {Ir, N, V, LT} ;
             true    -> {Ir, N, V, OldLT} end
            || {Ir, N, V, OldLT} <- C],
    {C2,A}.

%% @doc Return a version vector that represents the causal history.
-spec join(clock()) -> vector().
join({C,_}) -> [{I, N, T} || {I, N, _, T} <- C].

%% @doc Advances the causal history with the given id.
%% The new value is the *anonymous dot* of the clock.
%% The client clock SHOULD BE a direct result of new/2.
-spec update(clock(), id()) -> clock().
update({C,[V]}, I) ->
    LT = max_LT(C),
    {event(C, I, V, LT+1), []}.

%% @doc Advances the causal history of the
%% first clock with the given id, while synchronizing
%% with the second clock, thus the new clock is
%% causally newer than both clocks in the argument.
%% The new value is the *anonymous dot* of the clock.
%% The first clock SHOULD BE a direct result of new/2,
%% which is intended to be the client clock with
%% the new value in the *anonymous dot* while
%% the second clock is from the local server.
-spec update(clock(), clock(), id()) -> clock().
update({Cc,[V]}, Cr, I) ->
    %% Sync both clocks without the new value
    {C,Vs} = sync({Cc,[]}, Cr),
    LT = max_LT(C),
    %% We create a new event on the synced causal history,
    %% with the id I and the new value.
    %% The anonymous values that were synced still remain.
    {event(C, I, V, LT+1), Vs}.

%% @doc Gets the maximum logical time available in the dvvset.
-spec max_LT(entries()) -> logical_time().
max_LT(C) ->
  L = [LT || {_,_,_,LT} <- C],
  case L of
    [] -> 0;
    _  -> lists:max(L)
  end.

%% Private function
-spec event(vector(), id(), value(), logical_time()) -> entries().
event([], I, V, LT) ->
    [{I, 1, [V], LT}];
event([{I, N, L, _} | T], I, V, LT) ->
    [{I, N+1, [V | L], LT} | T];
event([{I1, _, _, _} | _]=C, I, V, LT) when I1 > I ->
    [{I, 1, [V], LT} | C];
event([H | T], I, V, LT) ->
    [H | event(T, I, V, LT)].

%% @doc Returns the total number of values in this clock set.
-spec size(clock()) -> non_neg_integer().
size({C,Vs}) -> lists:sum([length(L) || {_,_,L,_} <- C]) + length(Vs).

%% @doc Returns all the ids used in this clock set.
-spec ids(clock()) -> [id()].
ids({C,_}) -> ([I || {I,_,_,_} <- C]).

%% @doc Returns all the values used in this clock set,
%% including the anonymous values.
-spec values(clock()) -> [value()].
values({C,Vs}) -> Vs ++ lists:append([L || {_,_,L,_} <- C]).

%% @doc Compares the equality of both clocks, regarding
%% only the causal histories, thus ignoring the values.
-spec equal(clock() | vector(), clock() | vector()) -> boolean().
equal({C1,_},{C2,_}) -> equal2(C1,C2); % DVVSet
equal(C1,C2) when is_list(C1) and is_list(C2) -> equal2(C1,C2). %vector clocks

%% Private function
-spec equal2(vector(), vector()) -> boolean().
equal2([], []) -> true;
equal2([{I, C, L1, LT} | T1], [{I, C, L2, LT} | T2]) 
    when length(L1) =:= length(L2) -> 
    equal2(T1, T2);
equal2(_, _) -> false.

%% @doc Returns True if the first clock is causally older than
%% the second clock, thus values on the first clock are outdated.
%% Returns False otherwise.
-spec less(clock(), clock()) -> boolean().
less({C1,_}, {C2,_}) -> greater(C2, C1, false).

%% Private function
-spec greater(vector(), vector(), boolean()) -> boolean().
greater([], [], Strict) -> Strict;
greater([_|_], [], _) -> true;
greater([], [_|_], _) -> false;
greater([{I, N1, _, _} | T1], [{I, N2, _, _} | T2], Strict) ->
   if
     N1 == N2 -> greater(T1, T2, Strict);
     N1 >  N2 -> greater(T1, T2, true);
     N1 <  N2 -> false
   end;
greater([{I1, _, _, _} | T1], [{I2, _, _, _} | _]=C2, _) when I1 < I2 -> greater(T1, C2, true);
greater(_, _, _) -> false.

%% @doc Maps (applies) a function on all values in this clock set,
%% returning the same clock set with the updated values.
-spec map(fun((value()) -> value()), clock()) -> clock().
map(F, {C,Vs}) -> 
    {[ {I, N, lists:map(F, V), T} || {I, N, V, T} <- C], lists:map(F, Vs)}.


%% @doc Return a clock with the same causal history, but with only one
%% value in the anonymous placeholder. This value is the result of
%% the function F, which takes all values and returns a single new value.
-spec reconcile(Winner::fun(([value()]) -> value()), clock()) -> clock().
reconcile(F, C) ->
    V = F(values(C)),
    new(join(C),V).

%% @doc Returns the latest value in the clock set,
%% according to function F(A,B), which returns *true* if 
%% A compares less than or equal to B, false otherwise.
-spec last(LessOrEqual::fun((value(),value()) -> boolean()), clock()) -> value().
last(F, C) ->
   {_ ,_ , V2} = find_entry(F, C),
   V2.

%% @doc Return a clock with the same causal history, but with only one
%% value in its original position. This value is the newest value
%% in the given clock, according to function F(A,B), which returns *true*
%% if A compares less than or equal to B, false otherwise.
-spec lww(LessOrEqual::fun((value(),value()) -> boolean()), clock()) -> clock().
lww(F, C={E,_}) ->
    case find_entry(F, C) of
        {id, I, V}      -> {join_and_replace(I, V, E),[]};
        {anonym, _, V}  -> new(join(C),V)
    end.

%% find_entry/2 - Private function
-spec find_entry(fun((value(),value()) -> boolean()), clock()) -> {id | anonym, id(), value()}.
find_entry(F, {[], [V|T]}) -> find_entry(F, null, V, {[],T}, anonym);
find_entry(F, {[{_, _, [], _} | T], Vs}) -> find_entry(F, {T,Vs});
find_entry(F, {[{I, _, [V|_], _} | T], Vs}) -> find_entry(F, I, V, {T,Vs}, id).

%% find_entry/5 - Private function
find_entry(F, I, V, C, Flag) ->
    Fun = fun (A,B) ->
        case F(A,B) of
            false -> {left,A}; % A is newer than B
            true  -> {right,B} % A is older than B
        end
    end,
    find_entry2(Fun, I, V, C, Flag).

%% find_entry2/5 - Private function
find_entry2(_, I, V, {[], []}, anonym) -> {anonym, I , V};
find_entry2(_, I, V, {[], []}, id) -> {id, I, V};
find_entry2(F, I, V, {[], [V1 | T]}, Flag) ->
    case F(V, V1) of
        {left,V2}  -> find_entry2(F, I, V2, {[],T}, Flag);
        {right,V2} -> find_entry2(F, I, V2, {[],T}, anonym)
    end;
find_entry2(F, I, V, {[{_, _, [], _} | T], Vs}, Flag) -> find_entry2(F, I, V, {T, Vs}, Flag);
find_entry2(F, I, V, {[{I1, _, [V1|_], _} | T], Vs}, Flag) -> 
    case F(V, V1) of
        {left,V2}  -> find_entry2(F, I, V2, {T, Vs}, Flag);
        {right,V2} -> find_entry2(F, I1, V2, {T, Vs}, Flag)
    end.

%% Private function
join_and_replace(Ir, V, C) -> 
    [if
       I == Ir -> {I, N, [V], T};
       true    -> {I, N, [], T}
     end
     || {I, N, _, T} <- C].


%% @doc Possibly shrink the size of the clock by one, depending on current size.
%% Max can optimistically be set to replication factor + 1
-spec prune(C::clock(), Max::integer()) -> clock().
prune({C,A}, Max) when Max >= length(C) -> 
    {C,A};
prune({C,A}, Max) when Max < length(C) ->
    C2 = lists:sort(fun prune_leq/2, C),
    C3 = case hd(C2) of
            {_,_,[],_} -> tl(C2);
            _          -> C2
        end,
    {lists:sort(C3), A}.


prune_leq({_,_,A,_}, {_,_,[],_})
    % Give priority to entries w/o values.
    when A =/= [] -> false;
prune_leq({_,_,[],_}, {_,_,A,_})
    % give priority to entries w/o values.
    when A =/= [] -> true;
prune_leq({I1,_,_,T1}, {I2,_,_,T2}) ->
    % This sort need to be deterministic, to avoid spurious merge conflicts later.
    % We achieve this by using the ID as secondary key.
    {T1,I1} < {T2,I2}.



%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

join_test() ->
    A  = new(v1),
    A1 = update(A ,a),
    B  = new(join(A1), v2),
    B1 = update(B, A1, b),
    ?assertEqual( []                , join(A)  ),
    ?assertEqual( [{a,1,1}]         , join(A1) ),
    ?assertEqual( [{a,1,1},{b,1,2}] , join(B1) ),
    ok.

update_test() ->
    A0 = update(new(v1), a),
    A1 = update(new(join(A0),v2), A0, a),
    A2 = update(new(join(A1),v3), A1, b),
    A3 = update(new(join(A0),v4), A1, b),
    A4 = update(new(join(A0),v5), A1, a),
    ?assertEqual( {[{a,1,[v1],1}],[]}               , A0  ),
    ?assertEqual( {[{a,2,[v2],2}],[]}               , A1  ),
    ?assertEqual( {[{a,2,[],2}, {b,1,[v3],3}],[]}   , A2  ),
    ?assertEqual( {[{a,2,[v2],2}, {b,1,[v4],3}],[]} , A3  ),
    ?assertEqual( {[{a,3,[v5,v2],3}],[]}            , A4  ),
    ok.

sync_test() ->
    A   = update(new(v1),a),
    A1  = update(new(join(A),v2), a),
    A3  = update(new(join(A1),v3), b),
    A4  = update(new(join(A1),v3), c),
    X   = {[{x,1,[],1}],[]},
    Y   = update(new(v2),b),
    W   = {[{a,1,[],3}],[]},
    Z   = {[{a,2,[v2,v1],2}],[]},
    F   = fun (L,R) -> L>R end,
    ?assertEqual( {[{a,2,[v2],3}],[]}                           , sync([W,Z])   ),
    ?assertEqual( {[{a,2,[],2}, {b,1,[v3],3}, {c,1,[v3],3}],[]} , sync([A4,A3]) ),
    ?assertEqual( {[{a,1,[v1],1},{x,1,[],1}],[]}                , sync([X,A])   ),
    ?assertEqual( {[{a,1,[v1],1},{b,1,[v2],1}],[]}              , sync([A,Y])   ),
    ?assertEqual( sync([W,Z])     , sync([Z,W])                                 ),
    ?assertEqual( sync([A,A1])    , sync([A1,A])                                ),
    ?assertEqual( sync([A4,A3])   , sync([A3,A4])                               ),
    ?assertEqual( sync([X,A])     , sync([A,X])                                 ),
    ?assertEqual( sync([X,A])     , sync([A,X])                                 ),
    ?assertEqual( sync([Y,A])     , sync([A,Y])                                 ),
    ?assertEqual( sync([Y,A])     , sync([A,Y])                                 ),
    ?assertEqual( sync([A,X])     , sync([X,A])                                 ),
    ?assertEqual( lww(F,A4)       , sync([A4,lww(F,A4)])                        ),
    ok.

sync_update_test() ->
    A0  = update(new(v1), a),               % Mary writes v1 w/o VV
    VV1 = join(A0),                         % Peter reads v1 with version vector (VV)
    A1  = update(new(v2), A0, a),           % Mary writes v2 w/o VV
    A2  = update(new(VV1,v3), A1, a),       % Peter writes v3 with VV from v1
    ?assertEqual( [{a,1,1}]                 , VV1 ),
    ?assertEqual( {[{a,1,[v1],1}],[]}       , A0  ),
    ?assertEqual( {[{a,2,[v2,v1],2}],[]}    , A1  ),
    % now A2 should only have v2 and v3, since v3 was causally newer than v1
    ?assertEqual( {[{a,3,[v3,v2],3}],[]}    , A2  ),
    ok.

event_test() ->
    {A,_} = update(new(v1),a),
    ?assertEqual( [{a,2,[v2,v1],2}]             , event(A,a,v2,2) ),
    ?assertEqual( [{a,1,[v1],1}, {b,1,[v2],4}]  , event(A,b,v2,4) ),
    ok.

lww_last_test() ->
    F  = fun (A,B) -> A =< B end,
    F2 = fun ({_,TS1}, {_,TS2}) -> TS1 =< TS2 end,
    X  = {[{a,4,[5,2],1},{b,1,[],1},{c,1,[3],1}],[]},
    Y  = {[{a,4,[5,2],1},{b,1,[],1},{c,1,[3],1}],[10,0]},
    Z  = {[{a,4,[5,2],1}, {b,1,[1],1}], [3]},
    A  = {[{a,4,[{5, 1002345}, {7, 1002340}],1}, {b,1,[{4, 1001340}],1}], [{2, 1001140}]},
    ?assertEqual( last(F,X) , 5                                          ),
    ?assertEqual( last(F,Y) , 10                                         ),
    ?assertEqual( lww(F,X)  , {[{a,4,[5],1},{b,1,[],1},{c,1,[],1}],[]}   ),
    ?assertEqual( lww(F,Y)  , {[{a,4,[],1},{b,1,[],1},{c,1,[],1}],[10]}  ),
    ?assertEqual( lww(F,Z)  , {[{a,4,[5],1},{b,1,[],1}],[]}              ),
    ?assertEqual( lww(F2,A) , {[{a,4,[{5, 1002345}],1}, {b,1,[],1}], []} ),
    ok.

reconcile_test() ->
    F1 = fun (L) -> lists:sum(L) end,
    F2 = fun (L) -> hd(lists:sort(L)) end,
    X  = {[{a,4,[5,2],1},{b,1,[],1},{c,1,[3],1}],[]},
    Y  = {[{a,4,[5,2],1},{b,1,[],1},{c,1,[3],1}],[10,0]},
    ?assertEqual( reconcile(F1,X) , {[{a,4,[],1},{b,1,[],1},{c,1,[],1}],[10]} ),
    ?assertEqual( reconcile(F1,Y) , {[{a,4,[],1},{b,1,[],1},{c,1,[],1}],[20]} ),
    ?assertEqual( reconcile(F2,X) , {[{a,4,[],1},{b,1,[],1},{c,1,[],1}],[2]}  ),
    ?assertEqual( reconcile(F2,Y) , {[{a,4,[],1},{b,1,[],1},{c,1,[],1}],[0]}  ),
    ok.

less_test() ->
    A  = update(new(v1),a),
    B  = update(new(join(A),v2), a),
    B2 = update(new(join(A),v2), b),
    B3 = update(new(join(A),v2), z),
    C  = update(new(join(B),v3), A, c),
    D  = update(new(join(C),v4), B2, d),
    ?assert(    less(A,B)  ),
    ?assert(    less(A,C)  ),
    ?assert(    less(B,C)  ),
    ?assert(    less(B,D)  ),
    ?assert(    less(B2,D) ),
    ?assert(    less(A,D)  ),
    ?assertNot( less(B2,C) ),
    ?assertNot( less(B,B2) ),
    ?assertNot( less(B2,B) ),
    ?assertNot( less(A,A)  ),
    ?assertNot( less(C,C)  ),
    ?assertNot( less(D,B2) ),
    ?assertNot( less(B3,D) ),
    ok.

equal_test() ->
    A = {[{a,4,[v5,v0],1},{b,0,[],1},{c,1,[v3],1}], [v0]},
    B = {[{a,4,[v555,v0],1}, {b,0,[],1}, {c,1,[v3],1}], []},
    C = {[{a,4,[v5,v0],1},{b,0,[],1}], [v6,v1]},
    % compare only the causal history
    ?assert(    equal(A,B) ),
    ?assert(    equal(B,A) ),
    ?assertNot( equal(A,C) ),
    ?assertNot( equal(B,C) ),
    ok.

size_test() ->
    ?assertEqual( 1 , ?MODULE:size(new(v1))                                               ),
    ?assertEqual( 5 , ?MODULE:size({[{a,4,[v5,v0],1},{b,0,[],1},{c,1,[v3],1}],[v4,v1]})   ),
    ok.

ids_values_test() ->
    A = {[{a,4,[v0,v5],1},{b,0,[],1},{c,1,[v3],1}], [v1]},
    B = {[{a,4,[v0,v555],1}, {b,0,[],1}, {c,1,[v3],1}], []},
    C = {[{a,4,[],1},{b,0,[],1}], [v1,v6]},
    ?assertEqual( ids(A)                , [a,b,c]       ),
    ?assertEqual( ids(B)                , [a,b,c]       ),
    ?assertEqual( ids(C)                , [a,b]         ),
    ?assertEqual( lists:sort(values(A)) , [v0,v1,v3,v5] ),
    ?assertEqual( lists:sort(values(B)) , [v0,v3,v555]  ),
    ?assertEqual( lists:sort(values(C)) , [v1,v6]       ),
    ok.

map_test() ->
    A = {[{a,4,[],1},{b,0,[],1},{c,1,[],1}],[10]},
    B = {[{a,4,[5,0],1},{b,0,[],1},{c,1,[2],1}],[20,10]},
    F = fun (X) -> X*2 end,
    ?assertEqual( map(F,A) , {[{a,4,[],1},{b,0,[],1},{c,1,[],1}],[20]}            ),
    ?assertEqual( map(F,B) , {[{a,4,[10,0],1},{b,0,[],1},{c,1,[4],1}],[40,20]}    ),
    ok.

prune_test() ->
    A = {[{a,4,[v1],1},{b,0,[v2],5},{c,1,[],2}],[v10]},
    B = {[{a,4,[v1],1},{b,0,[v2],5},{c,1,[v3],2}],[]},
    ?assertEqual( {[{a,4,[v1],1},{b,0,[v2],5},{c,1,[],2}],[v10]} , prune(A,10)),
    ?assertEqual( {[{a,4,[v1],1},{b,0,[v2],5},{c,1,[],2}],[v10]} , prune(A,3)),
    ?assertEqual( {[{a,4,[v1],1},{b,0,[v2],5}],[v10]}            , prune(A,2)),
    ?assertEqual( {[{a,4,[v1],1},{b,0,[v2],5}],[v10]}            , prune(A,1)),
    ?assertEqual( {[{a,4,[v1],1},{b,0,[v2],5}],[v10]}            , prune(A,0)),
    ?assertEqual( {[{a,4,[v1],1},{b,0,[v2],5},{c,1,[v3],2}],[]}  , prune(B,2)),
    ok.

update_time_test() ->
    A = {[{a,4,[v1],1},{b,0,[v2],5},{c,1,[],2}],[v10]},
    % Should update id "c" with the maximum logical time (5 in this case).
    ?assertEqual( {[{a,4,[v1],1},{b,0,[v2],5},{c,1,[],5}],[v10]} , update_time(A,c)),
    % Should return the clock w/o modifications, since z does not exist in the dvvset.
    ?assertEqual( A , update_time(A,z)),
    ok.

-endif.
