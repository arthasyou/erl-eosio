%%----------------------------------------------------
%% @doc  列表增强函数
%% 
%% @author QingXuan 
%% @end
%%---------------------------------------------------- 
-module(lists2).
-export([
         shuffle/1,
         unique/1,
         for/3,
         for/4,
         sum/1,
         list_to_atom/1
        ]).

-export([to_json/1, is_proplist/1]).

%% @spec shuffle(L) -> NewList
%% @doc 打乱数组顺序 
shuffle(L) when is_list(L) ->
    List1 = [{rand:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
    [E || {_, E} <- List2].

%% 移除数组中重复的值
%% @return list()
unique([]) -> [];
unique([A]) -> [A];
unique([A, A]) -> [A];
unique([A, B]) -> [A, B];
unique([A | L]) ->
    unique(L, [A]).
%%
unique([], L2) -> lists:reverse(L2);
unique([A | L], L2) ->
    case lists:member(A, L2) of
        true  -> unique(L, L2);
        false -> unique(L, [A | L2])
    end.

%% for循环
for(Min, Max, _F) when Min>Max ->
    error;
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% 带返回状态的for循环
%% @return Acc
for(Max, Min, _F, Acc) when Min < Max -> Acc;
for(Max, Max, F, Acc) -> F(Max, Acc);
for(I, Max, F, Acc)   -> Acc1 = F(I, Acc), for(I+1, Max, F, Acc1).

sum([]) ->
    0;
sum([H|T]) ->
    H + sum(T).

list_to_atom(List) when is_list(List) ->
    case catch(list_to_existing_atom(List)) of
        {'EXIT', _} -> 
            erlang:list_to_atom(List);
        Atom -> 
            Atom
    end.


to_json(List) ->
    {obj, do_to_json(List)}.
do_to_json([{K, V} | T]) ->
    case is_proplist(V) of
        true ->
            [{K, to_json(V)} | do_to_json(T)];
        false ->
            [{K, V} | do_to_json(T)]
    end;
do_to_json([]) ->
    [].

is_proplist([{_, _} | T]) ->
    is_proplist(T);
is_proplist([]) ->
    true;
is_proplist(_) ->
    false.