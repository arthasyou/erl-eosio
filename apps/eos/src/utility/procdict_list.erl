%%%----------------------------------------------------------------------
%%%
%%% @doc 使用进程字典的列表操作
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(procdict_list).

-export([get_list/1, set_list/2, erase_list/1, list_add/2,list_add_tail/2, list_delete/2,
         list_keydelete/3, list_keyfind/3, list_keyupdate/4, list_keystore/4, 
         list_sort/2, list_member/2, list_len/1,
         get_ordset/1, set_ordset/2, erase_ordset/1, ordset_add/2, ordset_delete/2]).

%% @doc 获取list
get_list(Type) ->
    case erlang:get(Type) of
        undefined ->
            [];
        L ->
            L
    end.

%% @doc 设置list数据
set_list(Type, List) when is_list(List) ->
    erlang:put(Type, List),
    ok.

%% @doc 清除list,返回原有数据
erase_list(Type) ->
    erlang:erase(Type).

%% @doc list中增加一项
list_add(Type, Elem) ->
    set_list(Type, [Elem | get_list(Type)]),
    ok.

%% @doc list中增加一项从尾部添加
list_add_tail(Type, Elem) ->
    set_list(Type, lists:append(get_list(Type), [Elem])).

%% @doc list中删除一项(非key方式)
list_delete(Type, Elem) ->
    List2 = lists:delete(Elem, get_list(Type)),
    set_list(Type, List2),
    ok.

%% @doc list中删除一项
list_keydelete(Type, Key, Pos) ->
    case lists:keytake(Key, Pos, get_list(Type)) of
        {value, V, List2} ->
            set_list(Type, List2),
            V;
        false ->
            false
    end.

%% @doc 排序
list_sort(Type, Fun) ->
    set_list(Type, lists:sort(Fun, get_list(Type))),
    ok.

%% @doc 长度
list_len(Type) ->
	length(get_list(Type)).

%% @doc 元素是否存在
list_member(Type, Elem) ->
    lists:member(Elem, get_list(Type)).

%% @doc list中查找一项
list_keyfind(Type, Key, Pos) ->
    lists:keyfind(Key, Pos, get_list(Type)).

%% @doc list中更新
list_keyupdate(Type, Key, Pos, NewVal) ->
    List2 = lists:keyreplace(Key, Pos, get_list(Type), NewVal),
    set_list(Type, List2).

%% @doc list中存储，参考lists:keystore/3
list_keystore(Type, Key, Pos, NewVal) ->
    List2 = lists:keystore(Key, Pos, get_list(Type), NewVal),
    set_list(Type, List2).

%% 封装在进程词典中维护ordsets结构
%% @doc 获取ordset
get_ordset(Type) ->
    case erlang:get(Type) of
        undefined ->
            ordsets:new();
        S ->
            S
    end.

%% @doc 设置ordset数据
set_ordset(Type, Ordset) ->
    erlang:put(Type, Ordset),
    ok.

%% @doc 清除ordset
erase_ordset(Type) ->
    erlang:erase(Type),
    ok.

%% @doc ordset中增加一项
ordset_add(Type, Elem) ->
    set_ordset(Type, ordsets:add_element(Elem, get_ordset(Type))),
    ok.

%% @doc ordset中删除一项(非key方式)
ordset_delete(Type, Elem) ->
    set_ordset(Type, ordsets:del_element(Elem, get_ordset(Type))),
    ok.


