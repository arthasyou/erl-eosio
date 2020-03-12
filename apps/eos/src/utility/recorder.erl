%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2015 2:14 PM
%%%-------------------------------------------------------------------

-module(recorder).
-author("ysx").

-define(ETS_NAME, sys_recorder).

%% API
-export([create/0, delete/1, init/2, lookup/1]).

create() ->
    ensure_ets().

delete(Key) ->
    ets:delete(?ETS_NAME, Key).

%% -> ok | ignore
init(Key, InitVal) ->
    ensure_ets(),
    case ets:lookup(?ETS_NAME, Key) of
        [] ->
            ets:insert(?ETS_NAME, {Key, InitVal}),
            ok;
        _ ->
            ignore
    end.

lookup(Key) ->
    case ets:lookup(?ETS_NAME, Key) of
        [{_, Value} | _] ->
            Value;
        [] ->
            undefined
    end.

%% Internal
ensure_ets() ->
    case ets:info(?ETS_NAME) of
        undefined ->
            ets:new(?ETS_NAME, [named_table, public, set, {write_concurrency, true}]);
        _ ->
            ignore
    end.