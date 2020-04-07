%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2020 14:28
%%%-------------------------------------------------------------------
-author("ysx").

-ifndef(ERROR_HRL).
-define(ERROR_HRL, true).

-define(ERR_CONNECTION, 70001). %% 连接失败
-define(ERR_BALANCE, 70002). %% 账户余额不足
-define(ERR_RAM, 70003). %% EOS内存不足
-define(ERR_CPU, 70004). %% EOSCPU不足
-define(ERR_NET, 70005). %% EOS网络不足
-define(ERR_ACC, 70006). %% EOS账号不存在
-define(ERR_PRECISION, 70006). %% 货币数量最小保留4位小数
-define(ERR_NAME, 70007). %% 名字以存在
-define(ERR_TRANSACTION, 70008). %% transaction_id查不到

-endif.