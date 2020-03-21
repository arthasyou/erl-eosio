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
            after 1 ->
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
    ErrorList = recorder:lookup(eosio_error),
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

