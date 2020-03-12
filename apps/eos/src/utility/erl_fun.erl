%% @author Shawn
%% 用来解析字符串erlang代码
%% 字符串由策划配置


-module(erl_fun).

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_fun_expr/1]).

%% String -> Fun 
%% String = "fun(X) -> X + 1 end."
parse_fun_expr(S) ->
	{ok, Ts, _} = erl_scan:string(S),
	{ok, Exprs} = erl_parse:parse_exprs(Ts),
	{value, Fun, _} = erl_eval:exprs(Exprs, []),
	Fun.



%% ====================================================================
%% Internal functions
%% ====================================================================


