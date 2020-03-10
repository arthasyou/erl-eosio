%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2019 16:51
%%%-------------------------------------------------------------------
-module(db_sup).
-author("ysx").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, SqlPool} = application:get_env(eos, sqlPool),
    Name = proplists:get_value(name, SqlPool),
    PoolArgs = proplists:get_value(poolConf, SqlPool),
    WorkerArgs = proplists:get_value(sqlConf, SqlPool),
    Mysql = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
%%    {ok, {{one_for_all, 5, 10}, [Mysql]}}.

    SupFlags = #{strategy => one_for_all, intensity => 5, period => 10},

    ChildSpecs = [Mysql],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

