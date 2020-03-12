%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2019 14:40
%%%-------------------------------------------------------------------
-module(toppage_h).
-author("ysx").

-record(state, {}).

%% ==================================================
%% API
%% ==================================================
-export([init/2, content_types_provided/2, html/2, json/2, text/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([handle/2]).

init(Req, Opts) ->
%%    {ok, Req, #state{}}.
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [
        <<"GET">>,
        <<"HEAD">>,
        <<"POST">>,
        <<"PUT">>,
        <<"PATCH">>,
        <<"DELETE">>,
        <<"OPTIONS">>
    ],
    {Methods, Req, State}.


handle(Req, State=#state{}) ->
    lager:info("going here~p~n", [11111]),
    HasBody = cowboy_req:has_body(Req),
    lager:info("HasBody", [HasBody]),
    AA = cowboy_req:parse_qs(Req),
    lager:error("Headers~p~n", [AA]),
    {ok, Req1} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], [<<"REST Hello World as text!">>]),
    {ok, Req1, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"text/html">>, html},
        {<<"application/json">>, json},
        {<<"text/plain">>, text}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, html},
        {<<"application/json">>, json},
        {<<"text/plain">>, text}
    ], Req, State}.

html(Req, State) ->
    HasBody = cowboy_req:has_body(Req),
    lager:info("HasBody~p~n", [HasBody]),
    AA = cowboy_req:parse_qs(Req),
    lager:error("Headers~p~n", [AA]),
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.

json(Req, State) ->
    Path = cowboy_req:path(Req),
    {ok, RawBody, _Req1} = cowboy_req:read_body(Req),
    DataIn = jsx:decode(RawBody, [return_maps]),
    Jsx = web_routes:routing(Path, DataIn),
    DataOut = jsx:encode(Jsx),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, DataOut, Req),
    {stop, Req, State}.

text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.



%% ==================================================
%% Internal
%% ==================================================