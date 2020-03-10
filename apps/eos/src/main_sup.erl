%%%-------------------------------------------------------------------
%% @doc frame top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(main_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},

    DbSup = #{
        id => db_sup,
        start => {db_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [db_sup]
    },
    EosSup = #{
        id => eos_sup,
        start => {eos_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [eos_sup]
    },
    EosUrlSup = #{
        id => eos_url_sup,
        start => {eos_url_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [eos_url_sup]
    },
    ChildSpecs = [EosSup, EosUrlSup, DbSup],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
