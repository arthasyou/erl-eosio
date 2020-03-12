%% @author Administrator
%% @doc @todo Add description to fish_lib.


-module(eos_lib).
-include("logger.hrl").

-define(EOS_URL,"http://peer1.eoshuobipool.com:8181").
%% =================very=====very=======important=======================================
-define(EOS_KEY_LIST, [{107,81},{109,65},{115,97},{82,77},{68,57},{80,48},{89,85},{113,90},{114,54},{90,104},
			{119,76},{97,66},{54,49},{102,73},{69,117},{110,79},{85,50},{72,98},{51,105},{71,112},
			{121,68},{98,99},{106,109},{66,120},{76,100},{57,84},{52,108},{116,118},{81,103},{86,74},
			{84,87},{122,88},{75,70},{101,71},{117,53},{55,52},{100,82},{79,122},{111,114},{50,69},
			{77,89},{65,83},{73,101},{108,111},{104,110},{70,51},{67,102},{103,116},{99,121},{105,115},
			{53,113},{78,107},{112,75},{83,78},{56,119},{88,56},{74,67},{120,86},{87,72},{49,55},{118,80},{48,106}]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
%% ====================================================================
%% Internal functions
%% ====================================================================
-export([my_exec/1,my_exec/2,my_exec/3]).
-export([encode_str/2,decode_str/2]).
-export([import_user_wallet/0]).
encode_str(Str,rootadmin)->
	BinX0 = list_to_binary(Str),
	encode_str_1(BinX0);
encode_str(_,_)->
	error.
encode_str_1(List) ->
   encode_str_1(List,<<>>).
encode_str_1(<<>>,Str) ->
	Str;
encode_str_1(<<Value:8,Rest/binary>>,Str) when Value =< 255->	
	NewValue = case lists:keyfind(Value, 1, ?EOS_KEY_LIST) of
				   false->
					   Value;
				   {_,V}->
					   V
			   end,
	NewStr = <<NewValue:8,Str/binary>>,
	encode_str_1(Rest,NewStr).
	
decode_str(Str,adminroot)->
	BinX0 = list_to_binary(Str),
	decode_str_1(BinX0);
decode_str(_,_)->
	error.
decode_str_1(List) ->
   decode_str_1(List,<<>>).
decode_str_1(<<>>,Str) ->
	Str1=binary_to_list(Str),
	Str1;

decode_str_1(<<Value:8,Rest/binary>>,Str) when Value =< 255->	
	NewValue = case lists:keyfind(Value, 2, ?EOS_KEY_LIST) of
				   false->
					   Value;
				   {V,_}->
					   V
			   end,
	NewStr = <<NewValue:8,Str/binary>>,
	decode_str_1(Rest,NewStr).

%%====================================================================================================================
%%==========以下是===my_exec/2===========================================================
my_exec(add_cpu_net,{Acc,CpuStr,NetStr})->
	eos_lib:my_exec(unlock),
	Str = lists:concat(["cleos -u ",?EOS_URL," system delegatebw jys2322dceos ",Acc," '",CpuStr,"' '",NetStr,"' -p jys2322dceos"]),
    Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
    case get_data(Port, []) of
		{0, _R}->
				true;
		{1, _R}->
				false;
		Error->
			?ERROR_MSG("====get_data==3=~p====~p====~w=",[Str,Error,Error]),
			false
		
	end;

my_exec(buy_ram,{Acc,RamStr})->
	eos_lib:my_exec(unlock),
	Str = lists:concat(["cleos -u ",?EOS_URL," system buyram jys2322dceos ",Acc," '",RamStr,"' -p jys2322dceos"]),
    Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
    case get_data(Port, []) of
		{0,_}->
				true;
		{1,_}->
				false;
		Error->
			?ERROR_MSG("====get_data==3=~w=====",[Error]),
			false
	end;

my_exec(to_transfer,{AddressFrom,AddressTo,MoneyStr,OrderId,Type})->
	if AddressFrom =:= "system" ->
			Str =
		   	case Type of
				"EOS"->
		 			io_lib:format("cleos -u ~p push action transfer '[ \"jys2322dceos\", ~p, ~p,\"transfer to hs\" ]' -p jys2322dceos", [?EOS_URL,AddressTo,MoneyStr]);
				"DCGEM"->
		  			io_lib:format("cleos -u ~p push action jys2321dceos transfer '[ \"jys2322dceos\", ~p, ~p,\"transfer to hs\" ]' -p jys2322dceos", [?EOS_URL,AddressTo,MoneyStr]);
				"USDX"->
					io_lib:format("cleos -u ~p push action jys51usdxeos transfer '[ \"jys2322dceos\", ~p, ~p,\"transfer to hs\" ]' -p jys2322dceos", [?EOS_URL,AddressTo,MoneyStr])
			end,
		    my_exec(unlock);
		true->
			Str =
			case Type of
				"EOS"->
		 			io_lib:format("cleos -u ~p push action transfer '[ ~p, ~p, ~p, ~p]' -p ~p", [?EOS_URL,AddressFrom,AddressTo,MoneyStr,OrderId,AddressFrom]);
				"DCGEM"->
		  			io_lib:format("cleos -u ~p push action jys2321dceos transfer '[ ~p, ~p, ~p, ~p ]' -p ~p", [?EOS_URL,AddressFrom,AddressTo,MoneyStr,OrderId,AddressFrom]);
				"USDX"->
					io_lib:format("cleos -u ~p push action jys51usdxeos transfer '[ ~p, ~p, ~p, ~p ]' -p ~p", [?EOS_URL,AddressFrom,AddressTo,MoneyStr,OrderId,AddressFrom])
			end,
			my_exec(unlock_user)
	end,
    Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
    case get_data(Port, []) of
		{0,Result}->
			?ERROR_MSG("===buy_money==true===~p===~ts==",[Result,Str]),
			true;
		{1,Error}->
			?ERROR_MSG("===buy_money==false===~p===~ts==",[Error,Str]),
			false;
		Error->
			?ERROR_MSG("===buy_money==error===~p===~ts=",[Error,Str]),
			false
	end;	
	

	
	
my_exec(buy_money,{Account,MoneyStr,Type})->
	Str =
		case Type of
		"EOS"->			
 			io_lib:format("cleos -u ~p push action transfer '[ \"jys2322dceos\", ~p, ~p,\"transfer to hs\" ]' -p jys2322dceos", [?EOS_URL,Account,MoneyStr]);
		"DCGEM"->
  			io_lib:format("cleos -u ~p push action jys2321dceos transfer '[ \"jys2322dceos\", ~p, ~p,\"transfer to hs\" ]' -p jys2322dceos", [?EOS_URL,Account,MoneyStr]);
		"USDX"->
			io_lib:format("cleos -u ~p push action jys51usdxeos transfer '[ \"jys2322dceos\", ~p, ~p,\"transfer to hs\" ]' -p jys2322dceos", [?EOS_URL,Account,MoneyStr])
	end,

	my_exec(unlock),
    Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
    case get_data(Port, []) of
		{0,Result}->
			?ERROR_MSG("===buy_money==true===~p===~ts==",[Result,Str]),
			true;
		{1,Error}->
			?ERROR_MSG("===buy_money==false===~p===~ts==",[Error,Str]),
			false;
		Error->
			?ERROR_MSG("===buy_money==error===~p===~ts=",[Error,Str]),
			false
	end;
	
my_exec(account,Acc) ->
	Str = lists:concat(["cleos -u ",?EOS_URL," get account ",Acc," --json"]),
    Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
    case get_data(Port, []) of
		{0,Result}->
				case catch jsx:decode(Result) of
					Jsx when is_list(Jsx) ->
						#{
							<<"core_liquid_balance">> := Eos,
							<<"ram_quota">> := MemoryQuota,
							<<"ram_usage">> := MemoryUsage,
							<<"net_limit">> := NetLimit,
							<<"cpu_limit">> := CpuLimit
						} = Jsx,

						#{
							<<"used">> := NetLimitUsed,
							<<"max">> := NetLimitMax
						} = NetLimit,
						#{
							<<"used">> := CpuLimitUsed,
							<<"max">> := CpuLimitMax
						} = CpuLimit,
%%						case lists:keyfind(<<"core_liquid_balance">>, 1, KeyList) of
%%							false->
%%								Eos = list_to_binary("0 EOS");
%%							{_,Eos}->
%%								Eos
%%						end,
%%						{_,MemoryQuota} = lists:keyfind("ram_quota", 1, KeyList),
%%						{_,MemoryUsage} = lists:keyfind("ram_usage", 1, KeyList),
						
%%						{_,{obj,NetLimit}} = lists:keyfind("net_limit", 1, KeyList),
%%						{_,NetLimitUsed} = lists:keyfind("used", 1, NetLimit),
%%						{_,NetLimitMax} = lists:keyfind("max", 1, NetLimit),
						
%%						{_,{obj,CpuLimit}} = lists:keyfind("cpu_limit", 1, KeyList),
%%						{_,CpuLimitUsed} = lists:keyfind("used", 1, CpuLimit),
%%						{_,CpuLimitMax} = lists:keyfind("max", 1, CpuLimit),
						{true,[{eos,Eos},{ram_quota,MemoryQuota},{ram_usage,MemoryUsage},
							   {net_used,NetLimitUsed},{net_max,NetLimitMax},
							   {cpu_used,CpuLimitUsed},{cpu_max,CpuLimitMax}
							   ]};
					Error->
						?ERROR_MSG("====make_request===~w=====",[Error]),
						"error false"
				end;
		{1,_}->
				"error"
	end.
%%==========以下是===my_exec/1===========================================================
my_exec(create_key) ->
    Port = open_port({spawn, "cleos create key --to-console"}, [stream, in, eof, hide, exit_status]),
    {0,Result} = get_data(Port, []),
	Result_1 = string:sub_string(Result,14),
	Ln = string:str(Result_1, "\nPublic key: "),
	En = string:rstr(Result_1, "\n"),
	PrivateKey = string:substr(Result_1,1,Ln-1),
	Publickey = string:sub_string(Result_1,Ln+13,En-1),	
	
	PrivateKeyEncode = encode_str(PrivateKey,rootadmin),
%%	?ERROR_MSG("====create_key==1===~ts======~ts=",[PrivateKey,PrivateKeyEncode]),
    {true,Publickey,PrivateKeyEncode};

my_exec({create_account,Acc,Publickey}) ->
	eos_lib:my_exec(unlock),
	Str = lists:concat(["cleos -u ",?EOS_URL," system newaccount --stake-net '0.15 EOS' --stake-cpu '0.15 EOS' --buy-ram-kbytes 8 jys2322dceos ",
						Acc," EOS6URfVfZSosgdGj7op6ZPLhKtXHgbJE66WErWbHaZkVNa4eiAYe ",Publickey]),
%%	?ERROR_MSG("====create_key==1===~ts====",[Str]),
 	Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
	case get_data(Port, []) of
		{0, _Result}->
			true;
		{1, _Result}->
			false;
		Error->
			?ERROR_MSG("====get_data==3=~w=====",[Error]),
			false
	end;
my_exec(unlock)->
	Port = open_port({spawn, "cleos wallet unlock --password PW5J9QTa6KamLAf6GvLCFZ3qkhYcvw7H2SYAS8riHz3qzJjBH2FzR"}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []);

my_exec(unlock_user)->
	Port = open_port({spawn, "cleos wallet unlock -n user_wallet --password PW5KQtA8QiWeiMmLa5JKBdEX6XgZbJPm4ymp7ygKsaKG8fMV6aQNY"}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []);

my_exec({import,default,Pk})->
	Str = lists:concat(["cleos wallet import --private-key ",Pk]),
	Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []);
my_exec({import,user_wallet,Pk})->
	Str = lists:concat(["cleos wallet import --name user_wallet --private-key ",Pk]),
	Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []);
my_exec(_Command) ->
    nil.

%%==========以下是===my_exec/3===========================================================
my_exec(get_money_list,MoneySymbolList,Account)->
	get_money(MoneySymbolList,Account,[]).
%%=====================================================================

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.
%% ====================================================================
%% other functions
%% ====================================================================


get_money([], _Acc, R)->
	R;
get_money([MoneySymbol|MoneySymbolList],Acc,R) when MoneySymbol == "DCGEM"->
	Str = lists:concat(["cleos -u ",?EOS_URL," get currency balance jys2321dceos ",Acc," DCGEM"]),
	case do_open(Str,MoneySymbol) of
		{true,Num}->
			get_money(MoneySymbolList,Acc,[{MoneySymbol,Num}|R]);
		_->
			get_money(MoneySymbolList,Acc,[{MoneySymbol,list_to_binary("0 DCGEM")}|R])
	end;	
get_money([MoneySymbol|MoneySymbolList],Acc,R) when MoneySymbol == "USDX"->
	Str = lists:concat(["cleos -u ",?EOS_URL," get currency balance jys51usdxeos ",Acc," USDX"]),											   
	case do_open(Str,MoneySymbol) of
		{true,Num}->
			get_money(MoneySymbolList,Acc,[{MoneySymbol,Num}|R]);
		_->
			get_money(MoneySymbolList,Acc,[{MoneySymbol,list_to_binary("0 USDX")}|R])
	end.
	
do_open(Str,MoneySymbol)->
 	Port = open_port({spawn, Str}, [stream, in, eof, hide, exit_status]),
	case get_data(Port, []) of
		{0,Result}->
			?ERROR_MSG("====do_open=ok=~p=====",[Result]),
			case Result of
				[]->
					{true,list_to_binary(lists:concat([0," ",MoneySymbol]))};
				_->	
					{true,list_to_binary(string:strip(Result,both,$\n))}				
			end;	
		{1,Result}->
			false;
		Error->
			?ERROR_MSG("====do_open=error=~w=====",[Error]),
			false
	end.

import_user_wallet()->
	Query = db:select(eos_account),
	case db:query(Query) of
		{ok, []} ->
			none;
		{ok, List} ->
			my_exec(unlock),
			my_exec(unlock_user),
			Fun = fun(Item)->
				#{
					<<"privateKey">> := Str,
					<<"type">> := Type
				} = Item,
				Pk = decode_str(Str,adminroot),
				case Type of
					1->
						my_exec({import,default,Pk});
					0->
						my_exec({import,user_wallet,Pk})
				end
			end,
			lists:foreach(Fun, List),
			ok;
		Error ->
			?ERROR_MSG("failed: ~p", [Error]),
			{error, -1, unknown_error}
	end.
	


















