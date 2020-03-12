%% @author ayang
%% @doc @todo Add description to eos_url_work.


-module(eos_do_data_work).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-include("logger.hrl").
-include("eos.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).
%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
    {ok, 0}.
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({do_data, _BH, _Url,Info}, State) ->
	do_check_data(Info),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
 

handle_info(_, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

do_check_data(Data)->
%%	lager:info("~p~n", [Data]),
    #{
        <<"transactions">> := Data1
    } = Data,
    lists:foreach(fun(X) ->
        #{<<"trx">> := Data2} = X,
        case is_map(Data2) of
            true ->
                do_check_data_1(Data2);
            false ->
                ok
        end
    end, Data1).

do_check_data_1(Data) ->
    #{<<"transaction">> := Data1} = Data,
    #{<<"actions">> := Data2} = Data1,
    do_check_data_2(Data2).

do_check_data_2([])->
	nil;
do_check_data_2([RL|L])->
	#{
		<<"account">> := Account,
		<<"name">> := Name
	} = RL,
	case Account of
		Acc when Acc =:= <<"jys51usdxeos">>  orelse Acc =:= <<"jys2321dceos">> ->
			case Name of
				<<"transfer">> ->
					do_check_data_3(RL);
				_ ->
					do_check_data_2(L)
			end;
		_ ->
			do_check_data_2(L)
	end.
	
do_check_data_3(Map)->
	#{
		<<"data">> := Data
	} = Map,
	#{<<"memo">> := Memo} = Data,
	case Memo of
		<<"transfer to hs">>->
			nil;
		_->
			do_check_data_4(Memo)
	end.

do_check_data_4(BStrOrderId)->
	OrderStr = lib_common:to_list(BStrOrderId),
	mod_order_back ! {order_end,OrderStr,0}.
