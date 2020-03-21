%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 15:27
%%%-------------------------------------------------------------------
-module(e_port).
-author("ysx").

-include("error.hrl").

%% ==================================================
%% API
%% ==================================================
-export([exec/1, exec_json/1]).

exec(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, in, eof, stderr_to_stdout, exit_status]),
    get_data(Port, []).

exec_json(Cmd) ->
    case exec(Cmd) of
        {0, Str} ->
            {ok, jsx:decode(list_to_binary(Str), [return_maps])};
        {_, Error} ->
            lager:error("eosio error: ~p~n", [Error]),
            Reason = format_error(Error),
            {error, Reason}
    end.


%% ==================================================
%% Internal
%% ==================================================

get_data(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Sofar|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    true
            end,
            receive
                {'EXIT',  Port,  _} ->
                    ok
            after 1 ->              % force context switch
                ok
            end,
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, lists:flatten(Sofar)}
    end.

format_error(Error) ->
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

    check_error(Error, ErrorList).

check_error(_Error, []) ->
    1;
check_error(Error, [{ErrorID, EStr}|T]) ->
    case re:run(Error, EStr) of
        nomatch ->
            check_error(Error, T);
        _ ->
            ErrorID
    end.

