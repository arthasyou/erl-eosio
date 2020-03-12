%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2019 16:56
%%%-------------------------------------------------------------------
-module(eos_sup).
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
    case ets:info('monitor_symbol_eos') of
        undefined -> ets:new('monitor_symbol_eos',[set,public,named_table,{keypos,2}]);
        _ -> nil
    end,
    case ets:info('eos_work_id_to_pid') of
        undefined -> ets:new('eos_work_id_to_pid',[set,public,named_table,{keypos,1}]);
        _ -> nil
    end,
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    init_eosio(),
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},
    EDDW = #{
        id => eos_do_data_work,
        start => {eos_do_data_work, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [eos_do_data_work]
    },
    ESW = #{
        id => eos_sup_work,
        start => {eos_sup_work, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [eos_sup_work]
    },
    ChildSpecs = [EDDW, ESW],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_eosio() ->
    {ok, List} = application:get_env(eos, eosio),
    lists:foreach(fun({Key, Val}) ->
        recorder:init(Key, Val)
    end, List).


