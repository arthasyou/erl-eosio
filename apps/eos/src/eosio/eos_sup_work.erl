%% @author ayang
%% @doc @todo Add description to eos_url_work.


-module(eos_sup_work).
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
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	erlang:send_after(5000, self(), get_cur_blockheight),
    {ok, 0}.


%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(get_cur_blockheight, State) ->
	List = ets:tab2list('eos_work_id_to_pid'),
	Fun = fun({_,Pid})->
				gen_server:cast(Pid, get_cur_blockheight)  
		end,
	lists:foreach(Fun, List),
	{noreply, State};

handle_info({get_cur_blockheight,BlockHeight}, State) ->
	if State > 0 ->
		  	{noreply, State};
	   true->
			List = ets:tab2list('eos_work_id_to_pid'),
			Fun = fun({_,Pid})->
						gen_server:cast(Pid, {get_block_info,BlockHeight})  
				end,
			lists:foreach(Fun, List),		   
		  	{noreply, BlockHeight}
	end;

handle_info({get_block_info_back,State,Url,Info}, State) ->
	%%把消息转发到其他地方做处理
	gen_server:cast(eos_do_data_work, {do_data,State,Url,Info}),
	List = ets:tab2list('eos_work_id_to_pid'),
	Fun = fun({_,Pid})->
				gen_server:cast(Pid, {get_block_info,State+1})  
		end,
	lists:foreach(Fun, List),		   
  	{noreply, State+1};   

handle_info(_, State) ->
    {noreply, State}.
terminate(Reason, State) ->
    ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


