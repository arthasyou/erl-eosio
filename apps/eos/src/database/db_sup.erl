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
    {ok, Pools} = application:get_env(eos, pools),
    PoolSpecs = lists:map(fun({Name, PoolArgs, WorkerArgs}) ->
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),

    SupFlags = #{strategy => one_for_all, intensity => 5, period => 10},

    ChildSpecs = PoolSpecs,
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



