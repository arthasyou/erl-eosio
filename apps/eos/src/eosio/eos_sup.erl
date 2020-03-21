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
-include("error.hrl").

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
    init_error(),
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_eosio() ->
    {ok, List} = application:get_env(eos, eosio),
    lists:foreach(fun({Key, Val}) ->
        recorder:init(Key, Val)
    end, List).

init_error() ->
    {ok, ConnectErr} = re:compile("Failed to coonect to nodeos"),
    {ok, NetErr} = re:compile("network usage limit imposed"),
    {ok, BalanceErr} = re:compile("overdrawn balance"),
    {ok, AccErr} = re:compile("account does not exist"),
    {ok, CpuErr} = re:compile("CPU usage limit imposed"),
    {ok, PrecErr} = re:compile("symbol precision mismatch"),
    {ok, RamErr} = re:compile("insufficient ram"),

    ErrorList = [
        {?ERR_CONNECTION, ConnectErr},
        {?ERR_NET, NetErr},
        {?ERR_BALANCE, BalanceErr},
        {?ERR_ACC, AccErr},
        {?ERR_CPU, CpuErr},
        {?ERR_PRECISION, PrecErr},
        {?ERR_RAM, RamErr}],
    recorder:init(eosio_error, ErrorList).



