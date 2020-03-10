%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 17:26
%%%-------------------------------------------------------------------
-module(web_callback).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([create_key/0]).

create_key() ->
    case eos:create_key() of
        {true, Publickey, PrivateKeyEncode} ->
            #{flag => success, data => #{
                publicKey => Publickey,
                privateKey => PrivateKeyEncode
            }};
        _ ->
            #{flag => fail, reasonn => "sys_err"}
    end.

%% ==================================================
%% Internal
%% ==================================================