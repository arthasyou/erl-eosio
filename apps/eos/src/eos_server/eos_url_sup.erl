%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2019 15:06
%%%-------------------------------------------------------------------
-module(eos_url_sup).
-author("ysx").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    UrlList = data_eos_url:list(),
    Fun2 = fun(UId)->
        _UInfo = data_eos_url:get(UId),
        Name = list_to_atom(lists:concat([eos_work,"_",UId])),
        eos_url_sup:start_child([Name,UId])
           end,
    lists:foreach(Fun2, UrlList),
    {ok, Pid}.

start_child(Args) ->
    supervisor:start_child(?SERVER, Args).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 10},
    Server = #{
        id => eos_work,
        start => {eos_work, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [eos_work]
    },
    ChildSpecs = [Server],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
