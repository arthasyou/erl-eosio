%%----------------------------------------------------
%% @doc proplist的优化版本
%% 
%% <p>优化proplists的get_value/2</p>
%% 
%% <p>lists:key*()函数集里面只有lists:keyfind/3和lists:keymember/3是`bif'</p>
%% <p>lists:keydelete/3`不是bif'，性能跟proplists:delete差不多，所以不需要优化</p>
%%
%% @type proplist() = [{Key::term(), Value::term()}]
%% 
%% @author QingXuan 
%% @end
%%---------------------------------------------------- 
-module(proplists2).
-include_lib("eunit/include/eunit.hrl").
-export([
        get_value/2
        ,get_value/3
        ,is_defined/2
        ,delete_one/2
]).
-compile(export_all).

%% @spec get_value(Key::term(), proplist()) -> term() | undefined
%% @doc 获取一个值，如不存在则返回undefiend
get_value(_, []) ->
    undefined;
get_value(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            undefined;
        KV ->
            erlang:error({bad_arg, KV})
   end.

%% @spec get_value(Key::term(), proplist(), Default::term()) -> term()
%% @doc 获取一个值，Default为默认值
get_value(_, [], Default) ->
    Default;
get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            Default;
        KV ->
            erlang:error({bad_arg, KV})
   end.

%% @spec is_defined(Key::term(), proplist()) -> true | false
%% @doc 检查一个键是否定义在proplist中
is_defined(_, []) ->
    false;
is_defined(Key, List) ->
    lists:keymember(Key, 1, List).            

%% @spec delete_one(Key::term(), proplist()) -> proplist()
%% @doc 删除proplist中的第一个{Key, _}，proplists:delete/2是删除所有{Key, _}
delete_one(_, []) ->
    [];
delete_one(Key, List) ->
    lists:keydelete(Key, 1, List).

%% 单元测试-----------------------------
basic_test_() ->  
    [
        ?_assert(get_value(bbb, []) == proplists:get_value(bbb, [])),  
        ?_assert(get_value(bbb, [{aaa, 1}, {bbb, 2}]) == proplists:get_value(bbb, [{aaa, 1}, {bbb, 2}])),  
        ?_assert(get_value(bbb, [{aaa, 1}, {bbb, 2}, {bbb, 3}]) == proplists:get_value(bbb, [{aaa, 1}, {bbb, 2}, {bbb, 3}])),  
        ?_assertException(error, {bad_arg, {aaa, 1, x}}, get_value(aaa, [{aaa, 1, x}])),  
        ?_assert(get_value(bbb, [], default) == proplists:get_value(bbb, [], default)),  
        ?_assert(get_value(bbb, [{aaa, 1}, {bbb, 2}], default) == proplists:get_value(bbb, [{aaa, 1}, {bbb, 2}], default)),  
        ?_assert(is_defined(bbb, [{aaa, 1}, {bbb, 2}]) == proplists:is_defined(bbb, [{aaa, 1}, {bbb, 2}])),  
        ?_assert(is_defined(ccc, [{aaa, 1}, {bbb, 2}]) == proplists:is_defined(ccc, [{aaa, 1}, {bbb, 2}])),
        ?_assert(delete_one(aaa, [{aaa, 1}, {bbb, 2}]) == proplists:delete(aaa, [{aaa, 1}, {bbb, 2}])),
        ?_assertNot(delete_one(bbb, [{aaa, 1}, {bbb, 2}, {bbb, 3}]) == proplists:delete(bbb, [{aaa, 1}, {bbb, 2}, {bbb, 3}]))
    ].  

%% 测试用
%% ---------------------------
get_values([], _L) ->
    {};
get_values(Keys, L) ->
    get_values(Keys, L, erlang:make_tuple(length(Keys), undefined)).

get_values(_Keys, [], R) ->
    R;
get_values(Keys, [H|T], R) ->
    get_values(Keys, T, get_values(Keys, 1, H, R)).

get_values([], _N, _KV, R) ->
    R;
get_values([H|_T], N, {H, V}, R) ->
    erlang:setelement(N, R, V);
get_values([_H|T], N, {K, V}, R) ->
    get_values(T, N+1, {K, V}, R).

%% ---------------------------
get_values2([], _L) ->
    [];
get_values2(Keys, L) ->
    get_values2(Keys, L, []).

get_values2([], _L, R) ->
    R;
get_values2([H|T], L, R) ->
    case lists:keyfind(H, 1, L) of
        false -> 
            [undefined|get_values2(T, L, R)];
        {_, V} ->
            [V|get_values2(T, L, R)]
    end.
    
%% proplists2:perf(1000000).
%% "get_values" [total: 936(1014)ms avg: 0.936(1.014)us]
%% "get_values2" [total: 562(547)ms avg: 0.562(0.547)us]
%% ========================================================================
%%           get_values =    936.00ms [  100.00%]   1014.00ms [  100.00%]
%%          get_values2 =    562.00ms [   60.04%]    547.00ms [   53.94%]
%% ========================================================================
perf(N) ->
    ptester:run(N, [
        {"get_values", fun(_)->
                    get_values([b,c,a], [{a,1},{b,2},{c,3}])
        end},
        {"get_values2", fun(_)->
                    get_values2([b,c,a], [{a,1},{b,2},{c,3}])
        end}
    ]).
