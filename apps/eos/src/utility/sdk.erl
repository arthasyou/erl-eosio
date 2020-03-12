%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jan 2016 2:09 PM
%%%-------------------------------------------------------------------
-module(sdk).
-author("ysx").

%% API
-export([http/3, http/4, http/5, http/6]).

-export([format_http_get_params/1, format_xml_params/1]).

http(Method, Url, Params) ->
    {Headers, ContentType} =
    case Method of
        get ->
            {[{"charset", "utf-8"}, {"content-type", "application/x-www-form-urlencoded"}], []};
        post ->
            {[{"content-type", "ext/xml;charset=utf-8"}], "application/x-www-form-urlencoded;charset=utf-8"}
%%            {[{"content-type", "ext/xml;charset=utf-8"}], "ext/xml;charset=utf-8"}
    end,
    http(Method, Url, Headers, ContentType, Params, [{timeout, 5000}]).

http(Method, Url, Headers, Params) ->
    http(Method, Url, Headers, Params, [], []).

http(Method, Url, Headers, ContentType, Params) ->
    http(Method, Url, Headers, ContentType, Params, []).

http(Method, Url, Headers, ContentType, Params, HttpOps) ->
    case http_request(Method, Url, Params, Headers, ContentType, HttpOps) of
        {ok, {{_Version, _Flag, _ReasonPhrase}, _ReturnHeaders, Result}} ->
            {ok, Result};
%%            case Flag of
%%                200 ->
%%                    {ok, Result};
%%                Error ->
%%                    ?ERROR_MSG("http error:~p~n", [Error]),
%%                    {error, 1}
%%            end;
        {error, Reason} ->
            {error, Reason}
    end.

format_http_get_params(PropList) ->
    string:join([atom_to_list(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- PropList], "&").
format_xml_params(PropList) ->
    Body = string:join(["<" ++ atom_to_list(Key) ++ "><![CDATA[" ++ format_val(Val) ++ "]]></" ++ atom_to_list(Key) ++ ">"
        || {Key, Val} <- PropList], "\n"),
    "<xml>\n"++Body++"\n</xml>".


%% Internal
http_request(Method, Url, Params, Headers, ContentType, HttpOps) ->
    case Method of
        get ->
            Arg = Url ++ "?" ++ Params,
            http_get(Arg, Headers);
        post ->
            http_post(Url, Headers, ContentType, Params, HttpOps)
    end.

http_get(Arg, Headers) ->
    httpc:request(get, {Arg, Headers}, [{timeout, 5000}], []).

http_post(Url, Headers, ContentType, Body, HttpOps) ->
    httpc:request(post, {Url, Headers, ContentType, Body}, HttpOps, []).

format_val(Val) when is_list(Val) ->
    Val;
format_val(Val) when is_binary(Val) ->
    binary_to_list(Val);
format_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
format_val(Val) when is_float(Val) ->
    float_to_list(Val).

