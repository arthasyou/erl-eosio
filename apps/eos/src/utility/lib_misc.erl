-module(lib_misc).

-export([is_remote_process_alive/1]).

is_remote_process_alive(Pid) ->
	rpc:call(node(Pid), erlang, is_process_alive, [Pid]).