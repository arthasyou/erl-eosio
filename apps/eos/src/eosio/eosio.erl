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
-export([get_account/1, create_account/1, create_account_with_public_key/2]).
-export([transfer/5, get_currency/2]).
-export([delegate_bw/3, un_delegate_bw/3]).
-export([buy_ram/2, sell_ram/2]).
-export([list_bw/1, get_transaction/1]).


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
        Creator, " ", Account, " ", PublicKey, " ", PublicKey]),
    wallet_unlock(),
    e_port:exec_json(Cmd).


create_account_with_public_key(Account, PublicKey) ->
    Url = recorder:lookup(eos_url),
    Creator = recorder:lookup(eos_main_account),
    Cmd = lists:concat(["cleos -u ", Url, " system newaccount --stake-net '0 EOS' --stake-cpu '0 EOS' --buy-ram-kbytes 3 ",
        Creator, " ", Account, " ", PublicKey, " ", PublicKey]),
    wallet_unlock(),
    e_port:exec_json(Cmd).


transfer(From, To, Symbol, Quantity, Memo) ->
    Url = recorder:lookup(eos_url),
    Amount = Quantity ++ " " ++ Symbol,
    Chars =
    case Symbol of
        "EOS" ->
            Temp = lists:concat(["cleos -u ~p transfer ", From, " ", To, " ~p ~p -p "]),
            io_lib:format(Temp, [Url, Amount, Memo]);
        _ ->
            Contract = recorder:lookup(eos_contract),
            io_lib:format("cleos -u ~p push action ~p transfer '[ ~p, ~p, ~p, ~p ]' -p ",
                [Url, Contract, From, To, Amount, Memo])
    end,
    Cmd = lists:flatten(Chars)++From++"@active -j",
    wallet_unlock(),
    case e_port:exec_json(Cmd) of
        {ok, Reply} ->
            Data = format_transaction_id(Reply),
            {ok, Data};
        Error ->
            Error
    end.


get_currency(Account, Symbol) ->
    Url = recorder:lookup(eos_url),
    Contract = recorder:lookup(eos_contract),
    Cmd = lists:concat(["cleos -u ", Url, " get currency balance ", Contract, " ", Account, " ", Symbol, " -j"]),
    e_port:exec_json(Cmd).


delegate_bw(Account, Cpu, Net) ->
    Url = recorder:lookup(eos_url),
    MainAccount = recorder:lookup(eos_main_account),
    Str = lists:concat(["cleos -u ", Url, " system delegatebw ", MainAccount, " ", Account, " ~p ~p"]),
    CpuStr = Cpu ++" " ++ "EOS",
    NetStr = Net ++" " ++ "EOS",
    Chars = io_lib:format(Str, [NetStr, CpuStr]),
    Cmd = lists:flatten(Chars)++" -j",
    wallet_unlock(),
    case e_port:exec_json(Cmd) of
        {ok, Reply} ->
            Data = format_transaction_id(Reply),
            {ok, Data};
        Error ->
            Error
    end.


un_delegate_bw(Account, Cpu, Net) ->
    Url = recorder:lookup(eos_url),
    MainAccount = recorder:lookup(eos_main_account),
    Str = lists:concat(["cleos -u ", Url, " system undelegatebw ", MainAccount, " ", Account, " ~p ~p"]),
    CpuStr = Cpu ++" " ++ "EOS",
    NetStr = Net ++" " ++ "EOS",
    Chars = io_lib:format(Str, [NetStr, CpuStr]),
    Cmd = lists:flatten(Chars)++" -j",
    wallet_unlock(),
    case e_port:exec_json(Cmd) of
        {ok, Reply} ->
            Data = format_transaction_id(Reply),
            {ok, Data};
        Error ->
            Error
    end.


buy_ram(Account, Bytes) ->
    Url = recorder:lookup(eos_url),
    MainAccount = recorder:lookup(eos_main_account),
    Cmd = lists:concat(["cleos -u ", Url, " system buyram ", MainAccount, " ", Account, " --bytes ", Bytes, " -j"]),
    wallet_unlock(),
    case e_port:exec_json(Cmd) of
        {ok, Reply} ->
            Data = format_transaction_id(Reply),
            {ok, Data};
        Error ->
            Error
    end.


sell_ram(Account, Bytes) ->
    Url = recorder:lookup(eos_url),
    Cmd = lists:concat(["cleos -u ", Url, " system sellram ", Account, " ", Bytes, " -j"]),
    wallet_unlock(),
    case e_port:exec_json(Cmd) of
        {ok, Reply} ->
            Data = format_transaction_id(Reply),
            {ok, Data};
        Error ->
            Error
    end.


list_bw(Account) ->
    Url = recorder:lookup(eos_url),
    Cmd = lists:concat(["cleos -u ", Url, " system listbw ", Account, " -j"]),
    wallet_unlock(),
    e_port:exec_json(Cmd).


get_transaction(TransactionID) ->
    Url = recorder:lookup(eos_query_url),
    Cmd = lists:concat(["cleos -u ", Url, " get transaction ", TransactionID]),
    wallet_unlock(),
    case e_port:exec_json(Cmd) of
        {ok, Reply} ->
            Data = format_transaction_reply(Reply),
            {ok, Data};
        Error ->
            Error
    end.




%% ==================================================
%% Internal
%% ==================================================

format_transaction_id(Reply) ->
    transaction,
    #{<<"transaction_id">> := TransactionID} = Reply,
    #{<<"transaction_id">> => TransactionID}.

format_transaction_reply(Reply) ->
    #{
        <<"block_num">> := BlockNum,
        <<"traces">> := Traces
    } = Reply,
    [#{<<"act">> := Act}|_] = Traces,
    #{<<"data">> := Data, <<"name">> := Name} = Act,
    #{
        <<"block_num">> => BlockNum,
        <<"action">> => Data,
        <<"name">> => Name
    }.
