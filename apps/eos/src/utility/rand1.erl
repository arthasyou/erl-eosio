%%----------------------------------------------------

%% @doc 随机数种子服务器
%% 
%% @end
%%----------------------------------------------------
-module(rand1).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    get_seed/0, range/2, range/1,
    percent/0, permillage/0,
    test/1,
    split/2, split/1,
    take_by_prop/2, take_by_weights_list/1,
    string/1
]).
-record(state, {seed}).

%% --- 系统调用 ---------------------------------

%% --- 对外接口 ---------------------------------
%% @spec get_seed() -> {integer(), integer(), integer()}
%% @doc 取得一个随机数种子
get_seed() ->
    ensure_started(),
    gen_server:call(?MODULE, get_seed).

%% 产生一个介于Min到Max之间的随机整数
range(Same, Same) -> Same;
range(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    case get('#rand_seed') of
        undefined ->
            RandSeed = rand1:get_seed(),
            rand:seed(exs1024, RandSeed),
            put('#rand_seed', RandSeed);
        _ ->
	   skip
    end,
    M = Min - 1,
    rand:uniform(abs(Max - M)) + M.

%% 从一个list中随机取出一项
%% null | term()
range([]) -> null;
range([I]) -> I;
range(List) -> 
    Len = length(List),
    Index = range(1, Len),
    lists:nth(Index, List).

%% 百分概率
%% @return int()
percent() -> 
    range(1, 100).

%% 千分概率
%% @return int()
permillage() -> 
    range(1, 1000).

%% 概率命中
%% @return true | false
test(Rate) ->
    R = round(Rate * 1000000),
    case range(1, 1000000) of
        N when N =< R ->
            true;
        _ ->
            false
    end.

%% 从一个list中随机取出N项,并返回这N项和剩余的项
%% @return {TakenList, RestList}
split(N, List) ->
    split(N, List, []).
%%
split(N, List, List1) when (N > 0 andalso length(List) > 0) ->
    case split(List) of
        {Term, RestList} -> split(N - 1, RestList, [Term | List1]);
        _ -> {List1, List}
    end;
split(_N, List, List1) -> {List1, List}.

%% same as take(List, 1)
%% 从一个list中随机取出一项,并返回这个项,和剩余的项
%% @return {Term, RestList} | null
split([]) -> null;
split(List) -> 
    Len = length(List),
    Index = range(1, Len),
    take_and_rest(List, Index, []).
%%
take_and_rest(List, 1, Rest) ->
    [Term | R] = List,
    {Term, R ++ Rest};
take_and_rest(List, Index, Rest) ->
    [H | R] = List,
    Nrest = [H | Rest],
    take_and_rest(R, Index - 1, Nrest).



%% @doc 从[{Item, Proportion}...]列表中按照概率抽取不重复的多个元组
%% @spec [{Item, Proportion}...], int() -> [{Item, Proportion}...]
take_by_prop(List, N) when is_list(List) andalso N > 0 ->
	Length = length(List),
    if 
        N > Length ->
            [];
        N =:= Length ->
            List;
        N < Length ->
%%             TotalProp = lists:foldl(fun({_Item, Prop}, Sum) ->
%%                         Prop + Sum
%%                 end, 0, List),
            take_by_prop(List, Length, N, []);
        true ->
            []
    end.
%% 内部函数
take_by_prop(List, Length, N, Result) when N > 0 ->
    RandIndex = range(1, Length),	
    Elem = lists:nth(RandIndex, List),
    take_by_prop(lists:delete(Elem, List), Length-1, N-1, [Elem | Result]);
take_by_prop(_List, _Length, N, Result) when N =:= 0 ->
    Result.

%% 内部函数
%% 根据随机数RandIndex遍历数组，找到一个元组
%% take_one_by_prop(List, RandIndex, ArrayNth, LeftValue) ->
%%     {Item, Prop} = lists:nth(ArrayNth, List),
%%     RightValue = LeftValue + Prop,
%%     case RandIndex > LeftValue andalso RandIndex =< RightValue of
%%         true ->
%%             {Item, Prop};
%%         false ->
%%             take_one_by_prop(List, RandIndex, ArrayNth + 1, RightValue)
%%     end.

%% 权重获取列表中的序列
%% List::[{ID, Weights}]
%% ID::integer(), Weights::integer()

take_by_weights_list(List) ->
    {TotalWeights, SerialList} = serialize_weight_list(List),
    select_by_weights(TotalWeights, SerialList).

serialize_weight_list(List) ->
    {T, L} = lists:foldl(fun(X, {TW, LI}) ->
        {ID, Weight} = X,
        Acc = TW + Weight,
        {Acc, [{ID, Acc} | LI]}
    end, {0, []}, List),
    {T, lists:reverse(L)}.

select_by_weights(TotalWeights, List) ->	
	RandNumber = range(1, TotalWeights),
	pick_nth(List, RandNumber).

pick_nth([{Nth, Weights} | T], RandNumber) ->
	case RandNumber =< Weights of
		true ->
			Nth;
		false ->
			pick_nth(T, RandNumber)
	end.

string(Amount) ->
    string(Amount, []).
string(0, R) ->
    R;
string(Amount, R) ->
    Rand = rand1:range(33, 126),
    string(Amount - 1, [Rand | R]).
	


%% --- 服务器内部实现 ---------------------------------
%% @hidden
init([]) ->
    State = #state{},
    {ok, State}.

%% @hidden
%% 返回一个随机数组合做为其它进程的随机数种子
handle_call(get_seed, _From, State) ->
    case State#state.seed of
        undefined ->
            rand:seed(exs1024, erlang:timestamp());
        S ->
            rand:seed(exs1024, S)
    end,
    Seed = {rand:uniform(99999), rand:uniform(999999), rand:uniform(999999)},
    {reply, Seed, State#state{seed = Seed}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------
%% 启动服务器
ensure_started() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

