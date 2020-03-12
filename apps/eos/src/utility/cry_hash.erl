%% @author Shawn

-module(cry_hash).

%% ====================================================================
%% API functions
%% ====================================================================
-export([md5/1, sha512/1, hmacsha512/2]).

md5(List) ->
  Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(List))]),
  binary_to_list(Bin).

sha512(List) ->
  Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:hash(sha512, List))]),
  binary_to_list(Bin).

hmacsha512(List, Key) ->
    Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:hmac(sha512, Key, List))]),
    binary_to_list(Bin).



%% ====================================================================
%% Internal functions
%% ====================================================================


