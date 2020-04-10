%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2019 15:07
%%%-------------------------------------------------------------------
-module(web_routes).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([routing/2]).

routing(Path, DataIn) ->
    case Path of
        <<"/create_account">> ->
            web_callback:create_account(DataIn);
        <<"/get_account">> ->
            web_callback:get_account(DataIn);
        <<"/transfer">> ->
            web_callback:transfer(DataIn);
        <<"/get_currency">> ->
            web_callback:get_currency(DataIn);
        <<"/delegate_bw">> ->
            web_callback:delegate_bw(DataIn);
        <<"/un_delegate_bw">> ->
            web_callback:un_delegate_bw(DataIn);
        <<"/get_transaction">> ->
            web_callback:get_transaction(DataIn);
        <<"/buy_ram">> ->
            web_callback:buy_ram(DataIn);
        <<"/sell_ram">> ->
            web_callback:sell_ram(DataIn);
        <<"/list_bw">> ->
            web_callback:list_bw(DataIn);
        <<"/get_account_and_assets">> ->
            web_callback:get_account_and_assets(DataIn);        
        _ ->
            other()
    end.


%% ==================================================
%% Internal
%% ==================================================
other() ->
    #{error => "unknow path"}.