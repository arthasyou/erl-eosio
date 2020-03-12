%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2019 09:49
%%%-------------------------------------------------------------------
-module(eos).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([init/1]).
-export([create_key/0, create_account/2]).
-export([unlock/0, unlock_user/0, import_user_wallet/1]).
-export([add_cpu_net/3, buy_ram/2, buy_money/3]).
-export([transfer/5, account/1, get_money_list/2]).


init(PublicKey) ->
    eos_lib:my_exec({import,default,PublicKey}).

create_key() ->
    eos_lib:my_exec(create_key).

create_account(Account, PublicKey) ->
    eos_lib:my_exec({create_account, Account, PublicKey}).

unlock() ->
    eos_lib:my_exec(unlock).

unlock_user() ->
    eos_lib:my_exec(unlock_user).

import_user_wallet(PublicKey) ->
    eos_lib:my_exec({import,user_wallet,PublicKey}).


%% ==================================================
%% system
add_cpu_net(Account, CpuStr, NetStr)  ->
    eos_lib:my_exec(add_cpu_net,{Account,CpuStr,NetStr}).

buy_ram(Account, RamStr) ->
    eos_lib:my_exec(buy_ram,{Account, RamStr}).

buy_money(Account,MoneyStr,Type) ->
    eos_lib:my_exec(buy_money,{Account,MoneyStr,Type}).

transfer(AddressFrom,AddressTo,MoneyStr,OrderId,Type) ->
    eos_lib:my_exec(to_transfer,{AddressFrom,AddressTo,MoneyStr,OrderId,Type}).

account(Account) ->
    eos_lib:my_exec(account,Account).

get_money_list(Account, MoneySymbolList) ->
    eos_lib:my_exec(get_money_list,MoneySymbolList,Account).


%% ==================================================
%% Internal
%% ==================================================