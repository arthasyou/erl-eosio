%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 15:57
%%%-------------------------------------------------------------------
-module(eosio).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([wallet_unlock/0]).
-export([get_account/1, create_account/1]).
-export([transfer/4, get_currency/2]).
-export([delegate_bw/3, un_delegate_bw/3]).


wallet_unlock() ->
    case wallet_list() of
        {0, Str} ->
            case lists:member($*, Str) of
                true ->
                    locked;
                false ->
                    Key = recorder:lookup(eos_wallet_key),
                    Cmd = "cleos wallet unlock --password " ++ Key,
                    e_port:exec(Cmd)
            end;
        {1, Err} ->
            Err
    end.


wallet_list() ->
    Cmd = "cleos wallet list",
    e_port:exec(Cmd).



get_account(Account) ->
    Url = recorder:lookup(eos_url),
    Cmd = lists:concat(["cleos -u ", Url, " get account ", Account, " -j"]),
    e_port:exec_json(Cmd).

create_account(Account) ->
    Url = recorder:lookup(eos_url),
    PublicKey = recorder:lookup(eos_public_key),
    Creator = recorder:lookup(eos_main_account),
    Cmd = lists:concat(["cleos -u ", Url, " system newaccount --stake-net '0 EOS' --stake-cpu '0 EOS' --buy-ram-kbytes 3 ",
        Creator, " ", Account, " ", PublicKey, " ", PublicKey, " -j"]),
    wallet_unlock(),
    e_port:exec_json(Cmd).

transfer(From, To, Quantity, Memo) ->
    Url = recorder:lookup(eos_url),
    Contract = recorder:lookup(eos_contract),
    Chars = io_lib:format("cleos -u ~p push action ~p transfer '[ ~p, ~p, ~p, ~p ]' -p ",
        [Url, Contract, From, To, Quantity, Memo]),
    Cmd = lists:flatten(Chars)++From++"@active -j",
    wallet_unlock(),
    e_port:exec_json(Cmd).

get_currency(Account, Symbol) ->
    Url = recorder:lookup(eos_url),
    Contract = recorder:lookup(eos_contract),
    Cmd = lists:concat(["cleos -u ", Url, " get currency balance ", Contract, " ", Account, " ", Symbol, " -j"]),
    e_port:exec_json(Cmd).

delegate_bw(Account, Cpu, Net) ->
    Url = recorder:lookup(eos_url),
    MainAccount = recorder:lookup(eos_main_account),
    Str = lists:concat(["cleos -u ", Url, " system delegatebw ", MainAccount, " ", Account, " ~p ~p"]),
    Chars = io_lib:format(Str, [Net, Cpu]),
    Cmd = lists:flatten(Chars)++" -j",
    wallet_unlock(),
    e_port:exec_json(Cmd).

un_delegate_bw(Account, Cpu, Net) ->
    Url = recorder:lookup(eos_url),
    MainAccount = recorder:lookup(eos_main_account),
    Str = lists:concat(["cleos -u ", Url, " system undelegatebw ", MainAccount, " ", Account, " ~p ~p"]),
    Chars = io_lib:format(Str, [Net, Cpu]),
    Cmd = lists:flatten(Chars)++" -j",
    wallet_unlock(),
    e_port:exec_json(Cmd).



%% ==================================================
%% Internal
%% ==================================================

