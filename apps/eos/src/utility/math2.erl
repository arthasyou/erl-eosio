%% @author Administrator

-module(math2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 ceil/1,
		 floor/1,
		 zero_valid/1,
		 one_valid/1
		]).

%% 返回大于于或者等于指定表达式的最大整数
ceil(Value) ->
	Value1 = erlang:round(Value),
	case Value1 >= Value of
		true -> Value1;
		false -> Value1 + 1
	end.

%% 返回小于或者等于指定表达式的最大整数
floor(Value) ->
	Value1 = erlang:round(Value),
	case Value1 =< Value of
		true -> Value1;
		false -> Value1 - 1
	end.

%% 返回不小于0的数值
zero_valid(Value) ->
	case Value < 0 of
		true -> 0;
		false -> Value
	end.

%% 若计算得值小于1则默认为1
one_valid(Value) ->
	case Value < 1 of
		true ->
			1;
		false ->
			% math2:ceil(Value) 
			erlang:round(Value)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


