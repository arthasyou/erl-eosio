%% -------------------------------------------------------
%% @doc 计数器
%%
%% -------------------------------------------------------
-module(counter).

-export([create/0, init/1, init/2]).
-export([up/1, info/1, info/0]).

-define(ets_name, sys_counter).

create() ->
    ensure_ets().

%% -> ok | ignore
init(Key) ->
    init(Key, 1).

init(Key, InitVal) when is_integer(InitVal) ->	
    ensure_ets(),
    case ets:lookup(?ets_name, Key) of
        [] -> 
            ets:insert(?ets_name, {Key, InitVal}),
            ok;
        _ ->
            ignore
    end.

up(Key) ->
    ets:update_counter(?ets_name, Key, 1).

info(Key) ->
    case ets:lookup(?ets_name, Key) of
		[{_, Value} | _] ->
			Value;
		[] ->
			undefined
	end.
		

info() ->
    ets:tab2list(?ets_name).


%% -----------------------------------
%% 内部私有函数
%%
ensure_ets() ->
    case ets:info(?ets_name) of
        undefined ->			
            ets:new(?ets_name, [named_table, public, set, {write_concurrency, true}]);
        _ ->			
            ignore
    end.

