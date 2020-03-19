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
-export([create_account/1, get_account/1]).
-export([transfer/1, get_currency/1]).
-export([delegate_bw/1, un_delegate_bw/1]).

create_account(DataIn) ->
    lager:info("data:~p~n", [DataIn]),
    #{<<"account">> := Account} = DataIn,
    case eosio:create_account(binary_to_list(Account)) of
        error ->
            #{flag => fail, reason => <<"sys_err">>};
        Data ->
            #{flag => success, data => Data}
    end.

get_account(DataIn) ->
    #{<<"account">> := Account} = DataIn,
    case eosio:get_account(binary_to_list(Account)) of
        error ->
            #{flag => fail, reason => <<"sys_err">>};
        Data ->
            #{flag => success, data => Data}
    end.

transfer(DataIn) ->
    #{
        <<"from">> := From,
        <<"to">> := To,
        <<"symbol">> := Symbol,
        <<"quantity">> := Quantity,
        <<"memo">> := Memo
    } = DataIn,
    case eosio:transfer(
        binary_to_list(From),
        binary_to_list(To),
        binary_to_list(Symbol),
        binary_to_list(Quantity),
        binary_to_list(Memo)) of
        error ->
            #{flag => fail, reason => <<"sys_err">>};
        Data ->
            #{flag => success, data => Data}
    end.

get_currency(DataIn) ->
    #{
        <<"account">> := Account,
        <<"symbol">> := Symbol
    } = DataIn,
    case eosio:get_currency(binary_to_list(Account), binary_to_list(Symbol)) of
        error ->
            #{flag => fail, reason => <<"sys_err">>};
        Data ->
            #{flag => success, data => Data}
    end.

delegate_bw(DataIn) ->
    #{
        <<"account">> := Account,
        <<"cpu">> := Cpu,
        <<"net">> := Net
    } = DataIn,
    case eosio:delegate_bw(binary_to_list(Account), binary_to_list(Cpu), binary_to_list(Net)) of
        error ->
            #{flag => fail, reason => <<"sys_err">>};
        Data ->
            #{flag => success, data => Data}
    end.

un_delegate_bw(DataIn) ->
    #{
        <<"account">> := Account,
        <<"cpu">> := Cpu,
        <<"net">> := Net
    } = DataIn,
    case eosio:un_delegate_bw(binary_to_list(Account), binary_to_list(Cpu), binary_to_list(Net)) of
        error ->
            #{flag => fail, reason => <<"sys_err">>};
        Data ->
            #{flag => success, data => Data}
    end.


%% ==================================================
%% Internal
%% ==================================================