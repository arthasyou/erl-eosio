%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% mapreduce
%%%
%%% @end
%%% Created : 14. Jul 2016 12:05 PM
%%%-------------------------------------------------------------------
-module(pmap).
-author("ysx").

%% API
-export([pmap/2]).



pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) ->
        spawn(fun() -> do_fun(S, F, I) end)
    end, L),
    gather(Pids).


gather([H | T]) ->
    receive
        {H, Result} ->
            [Result | gather(T)]
    end;
gather([]) ->
    [].

do_fun(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.

