-module(lib_common).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([term_to_string/1, string_to_term/1, random/2, list_to_string/1,to_utf8_string/1,record_modified/2]).
-export([format_time/1, format_time_for_log/1,datetime_to_timestamp/1,timestamp_to_datetime/1]).
-export([random_select/1,random/1]).
-export([start_children/4,child_spec/7]).
-export([ctimestamp/0, date_num/0,date_num/1,time_num/1, seconds/0, date_to_seconds/1,time/0,week/0,get_diff_days/2,date_time_to_stamp/1,
		 date_tuple/0,seconds_to_localtime/1,seconds_to_date_num/1,week/1]).
-export([ceil/1, floor/1, now/1, sublist/4, to_list/1,to_binary/1,to_tuple/1,to_atom/1,max/2,min/2,
		 to_integer/1,lists_nth/2,random_one/1,random_list_norepeat/2]).
-export([get_ip/1,get_local_node/0,md5/1,get_list_n/2]).
-export([make_query_string/1]).

-define(DIFF_SECONDS_0000_1900, 62167219200).
%%
%% API Functions
%%


ctimestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%% 随机数：[1...Integer]
random(Integer)->
	rand:uniform(Integer).

%% 在一个区间随机取一个数
random(Min, Max) ->
    Diff = Max - Min + 1,
    Random = rand:uniform(Diff),
    Min + Random - 1.

%%log format time
format_time({_,_,_} = ErlangNow) ->
	Time = calendar:now_to_datetime(ErlangNow),
	format_time(Time);
format_time({{N,Y,R},{S,F,M}}) ->
	integer_to_list(N) ++ ":" ++ integer_to_list(Y) ++ ":" ++ integer_to_list(R) ++ ", " ++
	integer_to_list(S) ++ ":" ++ integer_to_list(F) ++ ":" ++ integer_to_list(M).

format_time_for_log({{Y, Mon, D}, {H, Min, S}}) ->
    integer_to_list(Y) ++ "-" ++ integer_to_list(Mon) ++ "-" ++ 
    integer_to_list(D) ++ " " ++ integer_to_list(H) ++ ":" ++
    integer_to_list(Min) ++ ":" ++ integer_to_list(S).

term_to_string(Term) ->
	lists:flatten(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% 原来list里面的元素的顺序会倒序
list_to_string(List) ->
    lists:flatten(do_list_to_string(List, [])).

do_list_to_string([], Str) ->
    ["[", Str, "]"];
do_list_to_string([H | T], Str)->
    Char = integer_to_list(H),
    case Str of 
        [] ->
            do_list_to_string(T, Char);
        _ ->
            NewStr = [Char, ",", Str],
            do_list_to_string(T, NewStr)
    end.




random_select(List) ->
    lists:nth(rand:uniform(length(List)), List).


%% 获取列表的一段
sublist(List, Index, Size, Max) ->
    Length = length(List),
    case Index > Length of 
        true ->
            [];

        false ->
            Min = 
            case Index =< 0 of
                true -> 1;
                false -> Index
            end,
            Len = 
            case (Min + Size - 1) >  Max of 
                true ->
                    Max - Min + 1;
                false ->
                    Size
            end,
            lists:sublist(List, Min, Len)
    end.

%% 取整 大于X的最小整数
ceil(X) ->
    T = trunc(X),
    if 
        X - T == 0 ->
            T;
        true ->
            if
                X > 0 ->
                    T + 1;
                true ->
                    T
            end			
    end.

%% 取整 小于X的最大整数
floor(X) ->
	 T = trunc(X),
	 if
		 X == T -> 
			 T;
		 true ->
			 if
				 X > 0 ->
					 T;
				 true ->
					 T - 1
			 end
	 end.

now(second) ->
	erlang:system_time(second);
now(millisecond) ->
	erlang:system_time(millisecond);
now(microsecond) ->
	erlang:system_time(microsecond);
now(nanosecond) ->
	erlang:system_time(nanosecond);
now(native) ->
	erlang:system_time(native).

datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?DIFF_SECONDS_0000_1900.
timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp) + ?DIFF_SECONDS_0000_1900.

%% 各种转换---list
to_list(Msg) when is_list(Msg) -> 
    Msg;
to_list(Msg) when is_atom(Msg) -> 
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> 
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> 
    integer_to_list(Msg);
to_list(Msg) when is_tuple(Msg) ->
	tuple_to_list(Msg);
to_list(Msg) when is_float(Msg) -> 
    float_to_list(Msg);
to_list(_) ->
    [].
%% 各种转换---binary
to_binary(Msg) when is_binary(Msg) ->
    Msg;
to_binary(Msg) when is_atom(Msg) ->
	list_to_binary(atom_to_list(Msg));
to_binary(Msg) when is_list(Msg) -> 
	list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> 
	list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_tuple(Msg) ->
	list_to_binary(tuple_to_list(Msg));
to_binary(Msg) when is_float(Msg) ->
	list_to_binary(float_to_list(Msg));
to_binary(_Msg) ->
    <<>>.
%% 各种转换---tuple
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) when is_list(T) ->
	list_to_tuple(T);
to_tuple(T) -> {T}.

%% 各种转换---atom
to_atom(Msg) when is_atom(Msg) -> 
    Msg;
to_atom(Msg) when is_binary(Msg) -> 
	list_to_atom1(binary_to_list(Msg));
to_atom(Msg) when is_integer(Msg) ->
	list_to_atom1(integer_to_list(Msg));
to_atom(Msg) when is_tuple(Msg) -> 
	list_to_atom1(tuple_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
	Msg2 = list_to_binary(Msg),
	Msg3 = binary_to_list(Msg2),
    list_to_atom1(Msg3);
to_atom(_) ->
    list_to_atom("").

%% 各种转换---integer
to_integer(Msg) when is_integer(Msg) -> 
    Msg;
to_integer(Msg) when is_binary(Msg) ->
	Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) -> 
    list_to_integer(Msg);
to_integer(_Msg) ->
    0.

%% list_to_existing_atom
list_to_atom1(List)->
	try 
		erlang:list_to_existing_atom(List)
	catch _:_ ->
	 	erlang:list_to_atom(List)
	end.

%%获取数据属于数组中的第几个0是不存在
get_list_n(Tulp,LIst)->
	get_list_n_1(Tulp,LIst,0).
get_list_n_1(_Tulp,[],_N)->
	0;
get_list_n_1(Tulp,[Tulp|_LIst],N)->
	N+1;
get_list_n_1(Tulp,[_|LIst],N)->
	get_list_n_1(Tulp,LIst,N+1).	
	
%% 取最大值
max(Min,Max) when Min =< Max -> Max;
max(Max,Min) when Min < Max -> Max;
max(Max,_Min) -> Max.

%% 取最小值
min(Min,Max) when Min =< Max -> Min;
min(Max,Min) when Min < Max -> Min;
min(_Max,Min) -> Min.

%% 与lists:nth一样，不过多了0判断和N>length(List)情况的判断
lists_nth(0, _) -> 0;
lists_nth(1, [H|_]) -> H;
lists_nth(_, []) -> 0;
lists_nth(N, [_|T]) when N > 1 ->
    lists_nth(N - 1, T).

%% 从列表中随机取出一个数据(等概率)
%% lib_common:random_one([a,b,c,d,e]).
random_one([]) -> null;
random_one(List) ->
	Len = length(List),
	Idx = random(Len),
	lists:nth(Idx, List).

%% 从列表中随机取出count数据(等概率)
%% lib_common:random_list_norepeat([a,b,c,d,e]).
random_list_norepeat(List, Count) ->
	Len =  length(List),
	if
		Len >= Count -> random_list_norepeat(List, Count, []);
		true -> List
	end.
random_list_norepeat([], _, Acc) -> Acc;
random_list_norepeat(_List, 0, Acc) -> Acc;
random_list_norepeat(List, Count, Acc) ->
	case random_one(List) of
		null -> random_list_norepeat(List, Count, Acc);
		Data  ->
			case lists:member(Data, Acc) of
				true	->
					random_list_norepeat(List, Count, Acc);
				false	->
					List2	= lists:delete(Data, List),
					random_list_norepeat(List2, Count - 1, [Data|Acc])
			end
	end.

to_utf8_string(String)
  when is_list(String) ->
    binary_to_list(unicode:characters_to_binary(String,utf8,utf8));
to_utf8_string(String) 
  when is_binary(String) ->
    String;
to_utf8_string(_Other) ->
    [].
record_modified(R1, R2)
  when is_tuple(R1) andalso
       is_tuple(R2) ->
    L1 = tuple_to_list(R1),
    L2 = tuple_to_list(R2),
    record_modified({[], false}, L1, L2).
record_modified({[], InFlag}, [Head | Rest1], [Head | Rest2]) ->
    record_modified({[Head], InFlag}, Rest1, Rest2);
record_modified({ResultList, InFlag}, [Head | Rest1], [Head | Rest2]) ->
    record_modified({[undefined | ResultList], InFlag}, Rest1, Rest2);
record_modified({ResultList, _}, [_H1 | Rest1], [H2 | Rest2]) ->
    record_modified({[H2 | ResultList], true}, Rest1, Rest2);
record_modified({ResultList, true}, [], []) ->
    %% 将最终的结果反转成 record
    list_to_tuple(lists:reverse(ResultList));
record_modified({_, false}, [], []) ->
    %% 没有任何变更
    [];
record_modified(_RList, _L1, _L2) ->
    [].


date_num(Tulp) ->
	{Y, M, D} = Tulp,
	Y * 10000 + M * 100 + D.
time_num(Tulp) ->
	{H, M, S} = Tulp,
	H * 3600 + M * 60 + S.

%% 得到现在日期--20120630
date_num() ->
	{Y, M, D} = date_tuple(),
	Y * 10000 + M * 100 + D.

%% 得到现在日期{年,月,日}
date_tuple() ->
%% 	erlang:date().
	{Date, _Time}	= seconds_to_localtime(seconds()),
	Date.

%% 得到现在时间{时,分,秒}
time() ->
%% 	erlang:time().
	{_Date, Time}	= seconds_to_localtime(seconds()),
	Time.
%% -----------------------------------------------------------------
%% 根据1970年以来的秒数获得日期
%% -----------------------------------------------------------------
%% lib_common:seconds_to_localtime(seconds()).
seconds_to_localtime(Seconds) ->
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1900),
	calendar:universal_time_to_local_time(DateTime).
%% 根据秒数获得日期 --20120630
seconds_to_date_num(Seconds) ->
	{{Y, M, D}, {_, _, _}} = seconds_to_localtime(Seconds),
	Y * 10000 + M * 100 + D.

%% 得到现在时间秒数 
seconds()->
	{MegaSecs, Secs, _}	= os:timestamp(),
	MegaSecs * 1000000 + Secs.

%% 得到今天是星期几
week() ->
	{Y,M,D} = date_tuple(),
	week(Y,M,D).

%% 得到Y年M月D日是星期几
week({Y,M,D}) ->
	week(Y,M,D).

%% 得到Y年M月D日是星期几
week(Y, 1, D) -> week(Y - 1, 13, D);
week(Y, 2, D) -> week(Y - 1, 14, D);
week(Y, M, D) -> ((D + 2 * M + 3 * (M + 1) div 5 + Y + Y div 4 - Y div 100 + Y div 400) rem 7) + 1.

%% -----------------------------------------------------------------
%% 计算相差的天数
%% -----------------------------------------------------------------
get_diff_days(Seconds1, Seconds2) ->
	{{Year1, Month1, Day1}, _} = seconds_to_localtime(Seconds1),
	{{Year2, Month2, Day2}, _} = seconds_to_localtime(Seconds2),
	Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
	Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
	abs(Days2-Days1).

%% 日期转化为当天0点秒数
date_to_seconds(Date) ->
	Y = Date div 10000,
	M = (Date - 10000 * Y) div 100,
	D = (Date - 10000 * Y - 100 * M),
	date_time_to_stamp({Y, M, D, 0, 0, 0}).

%% 通过datetime获取时间戳
date_time_to_stamp(Sec) when is_integer(Sec) ->
    Sec;
date_time_to_stamp({Y, M, D, H, I, S}) ->
	date_time_to_stamp(Y, M, D, H, I, S).
date_time_to_stamp(Y, M, D, H, I, S) ->
	[UniversalTime]	= calendar:local_time_to_universal_time_dst({{Y, M, D}, {H, I, S}}),
	Seconds			= calendar:datetime_to_gregorian_seconds(UniversalTime),
	TimeGMT			= ?DIFF_SECONDS_0000_1900,
	Seconds - TimeGMT.

%% -----------------------------------------------------------------
get_local_node() ->
	erlang:node().

get_ip(Socket) when is_port(Socket) ->
	case inet:peername(Socket) of
		{ok, {Ip, _Port}} ->
			inet:ntoa(Ip);
		_ ->
			integer_to_list(0) ++ "." ++ integer_to_list(0) ++ "." ++ integer_to_list(0) ++ "." ++ integer_to_list(0)
	end;
get_ip(Ip) ->
	inet:ntoa(Ip).


start_children([], _Sup, _Nth, _Len) ->
	ok;
start_children([Mod|Tail], Sup, Nth, Len) ->
	try
		Type		= get_module_type(Mod),
		ChildSpec 	= child_spec(Mod, Mod, start_link, [], permanent, 1000, Type),
		case supervisor:start_child(Sup, ChildSpec) of
			{ok, _Pid} ->
%%				?DEBUG("ok:server[~p|~p].....[~p/~p]", [Type, Mod, Nth, Len]),
				start_children(Tail, Sup, Nth+1, Len);
			{error, {already_started, _Pid}} ->
				start_children(Tail, Sup, Nth+1, Len);
			{error, Reason} ->
				throw({error, Reason})
		end
	catch
		Error:_Why ->
			Error
	end.

child_spec(ServName, Mod, Fun, Args, Restart, Shutdown, Type) ->
	{ServName,                                          
	 {Mod, Fun, Args},         
	 Restart,                                           
	 Shutdown,                                         
	 Type,                                            
	 [Mod]                                          
	}.

get_module_type(Module) ->
	case module_attributes(Module) of
		[supervisor] -> supervisor;
		[gen_server] -> worker;
		_ -> throw({error, no_find})
	end.

module_attributes(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
            {error, undef};
        {'EXIT', Reason} ->
            {error, Reason};
        TupleList ->
            case lists:keyfind(behaviour, 1, TupleList) of
				{behaviour, Value} ->
					Value;
				_ ->
					 {error, no_find}
			end
    end.
md5(S) -> 
	Md5_bin = erlang:md5(S), 
	Md5_list = binary_to_list(Md5_bin), 
	lists:flatten(list_to_hex(Md5_list)). 
	 
	list_to_hex(L) -> 
	lists:map(fun(X) -> int_to_hex(X) end, L). 
	 
	int_to_hex(N) when N < 256 -> 
	[hex(N div 16), hex(N rem 16)].
	 
	hex(N) when N < 10 -> 
	$0+N; 
	hex(N) when N >= 10, N < 16 -> 
	$a + (N-10).


make_query_string(Params) ->
    Str = [lists:concat([url_encode(thin_to_list(Key)),"=",url_encode(thin_to_list(Value))]) || {Key,Value} <- Params],
    string:join(Str, "&").
thin_to_list(Thin) ->
    if
        erlang:is_atom(Thin) ->
            erlang:atom_to_list(Thin);
        erlang:is_integer(Thin) ->
            erlang:integer_to_list(Thin);
        erlang:is_binary(Thin) ->
            erlang:binary_to_list(Thin);
		erlang:is_float(Thin) ->
			erlang:float_to_list(Thin);
        true ->
            Thin
    end.
url_encode(String) ->
    lists:append([encode_url(Char) || Char <- String]).

encode_url(H) when H >= $0 andalso H =< $9 ->
    [H];
encode_url(H) when H >= $a andalso H =< $z ->
    [H];
encode_url(H) when H >= $A andalso H =< $Z ->
    [H];
encode_url(H) when H =:= $- ->
    [H];
encode_url(H) when H =:= $_ ->
    [H];
encode_url(H) when H =:= $. ->
    [H];
encode_url(H) ->
    [$% | http_util:integer_to_hexlist(H)].