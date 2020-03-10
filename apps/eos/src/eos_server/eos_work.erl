-module(eos_work).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).
-include("logger.hrl").
-include("eos.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start_link(Name,Id) ->
    gen_server:start_link({local,Name},?MODULE, [Name,Id], []).
%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([Name,Id]) ->
	UInfo = data_eos_url:get(Id),
	Url = UInfo#data_eos_url.url_address,
	put(main_profile,Name),
%%    inets:start(permanent),
%%    ssl:start(permanent),
%%    {ok, _Pid} = inets:start(httpc, [{profile, Name}],stand_alone),
%%    HttpcOptions = [{max_sessions, 5000},
%%                    {max_keep_alive_length, 1},
%%                    {keep_alive_timeout, 0}
%%                   ],
%%    httpc:set_options(HttpcOptions, self()),
	ets:insert('eos_work_id_to_pid', {Id,self()}),
	erlang:send_after(1000, self(), get_block_info),
    {ok, {Url,0,0}}.


%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(get_cur_blockheight, {Url,Height,Height}) ->
	RealURl = lists:concat([Url,"/v1/chain/get_info"]),
	Headers = [{"accept", "application/json"},{"content-type", "application/json"}],
	case ibrowse:send_req(RealURl, Headers, post) of
		{ok, "200", _ReplyHeaders, ReplyBody} ->
			Maps = jsx:decode(list_to_binary(ReplyBody), [return_maps]),
			case maps:is_key(<<"head_block_num">>, Maps) of
				true ->
					#{<<"head_block_num">> := BlockHeight} = Maps,
					eos_sup_work ! {get_cur_blockheight,BlockHeight},
					{noreply, {Url, Height, BlockHeight}};
				false ->
					{noreply, {Url, Height, Height}}
			end;
		_ ->
			{noreply, {Url, Height, Height}}
	end;


handle_cast({get_block_info,BlockHeight}, {Url,OldHeight, _NextHeight}) ->
	{noreply, {Url,OldHeight,BlockHeight}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(get_block_info, {Url, OldHeight, NextHeight}) ->
	erlang:send_after(250, self(), get_block_info),
	if OldHeight >= NextHeight ->
			{noreply, {Url,OldHeight,NextHeight}};
		true->
			RealURl = lists:concat([Url,"/v1/chain/get_block"]),
			Map = #{
				block_num_or_id => NextHeight
			},
			Body = jsx:encode(Map),
			Headers = [{"accept", "application/json"},{"content-type", "application/json"}],
			case ibrowse:send_req(RealURl, Headers, post, Body) of
				{ok, "200", _ReplyHeaders, ReplyBody} ->
					Maps = jsx:decode(list_to_binary(ReplyBody), [return_maps]),
					case maps:is_key(<<"block_num">>, Maps) of
						true ->
							eos_sup_work ! {get_block_info_back, NextHeight, Url, Maps};
						false ->
							ok
					end;
				_ ->
					ok
			end
	end,
	{noreply, {Url, OldHeight, NextHeight}};
handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
