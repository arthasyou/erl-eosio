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
-export([buy_ram/1, sell_ram/1]).
-export([list_bw/1]).
-export([get_transaction/1]).
-export([get_account_and_assets/1]).

create_account(DataIn) ->
    #{<<"account">> := Account} = DataIn,
    Reply = eosio:create_account(binary_to_list(Account)),
    reply(Reply).

get_account(DataIn) ->
    #{<<"account">> := Account} = DataIn,
    Reply = eosio:get_account(binary_to_list(Account)),
    reply(Reply).

transfer(DataIn) ->
    #{
        <<"from">> := From,
        <<"to">> := To,
        <<"symbol">> := Symbol,
        <<"quantity">> := Quantity,
        <<"memo">> := Memo
    } = DataIn,
    Reply = eosio:transfer(
        binary_to_list(From),
        binary_to_list(To),
        binary_to_list(Symbol),
        binary_to_list(Quantity),
        binary_to_list(Memo)),
    reply(Reply).

get_currency(DataIn) ->
    #{
        <<"account">> := Account,
        <<"symbol">> := Symbol
    } = DataIn,
    Reply = eosio:get_currency(binary_to_list(Account), binary_to_list(Symbol)),
    reply(Reply).

delegate_bw(DataIn) ->
    #{
        <<"account">> := Account,
        <<"cpu">> := Cpu,
        <<"net">> := Net
    } = DataIn,
    Reply = eosio:delegate_bw(binary_to_list(Account), binary_to_list(Cpu), binary_to_list(Net)),
    reply(Reply).

un_delegate_bw(DataIn) ->
    #{
        <<"account">> := Account,
        <<"cpu">> := Cpu,
        <<"net">> := Net
    } = DataIn,
    Reply = eosio:un_delegate_bw(binary_to_list(Account), binary_to_list(Cpu), binary_to_list(Net)),
    reply(Reply).

buy_ram(DataIn) ->
    #{
        <<"account">> := Account,
        <<"bytes">> := Bytes
    } = DataIn,
    Reply = eosio:buy_ram(binary_to_list(Account), binary_to_list(Bytes)),
    reply(Reply).

sell_ram(DataIn) ->
    #{
        <<"account">> := Account,
        <<"bytes">> := Bytes
    } = DataIn,
    Reply = eosio:sell_ram(binary_to_list(Account), binary_to_list(Bytes)),
    reply(Reply).

list_bw(DataIn) ->
    #{
        <<"account">> := Account
    } = DataIn,
    Reply = eosio:list_bw(binary_to_list(Account)),
    reply(Reply).

get_transaction(DataIn) ->
    #{
        <<"id">> := ID
    } = DataIn,
    Reply = eosio:get_transaction(binary_to_list(ID)),
    reply(Reply).

get_account_and_assets(DataIn) ->
    #{
        <<"account">> := AccountBin
    } = DataIn,
    Account = binary_to_list(AccountBin),

    Reply =
    case eosio:get_account(Account) of
        {ok, R1} ->
            case eosio:get_currency(Account, "DC") of
                {ok, R2} ->
                    #{<<"assets">> := Assets} = R1,
                    {ok, R1#{<<"assets">> => Assets++R2}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end,
    reply(Reply).
 

%% ==================================================
%% Internal
%% ==================================================
reply(Reply) ->
    case Reply of
        {ok, Data} ->
            #{flag => success, data => Data};
        {error, Reason} ->
            lager:info("Reson: ~p~n", [Reply]),
            #{flag => fail, reason => Reason}
    end.
