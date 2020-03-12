-module(node_hash).

-export([term_to_base64/1,
         base64_to_term/1]).

%% 这个模块摘自ejabberd中的jlib.erl
%% 使用方法
%% 1> node_hash:term_to_base64({'room1@192.168.1.137', "abc"}).
%% <<"g2gCZAATcm9vbTFAMTkyLjE2OC4xLjEzN2sAA2FiYw==">>
%% 2> node_hash:base64_to_term(<<"g2gCZAATcm9vbTFAMTkyLjE2OC4xLjEzN2sAA2FiYw==">>).
%% {term,{'room1@192.168.1.137',"abc"}}


term_to_base64(Term) ->
    encode_base64(term_to_binary(Term)).

base64_to_term(Base64) ->
    case catch binary_to_term(decode_base64(Base64), [safe]) of
        {'EXIT', _} ->
            error;
        Term ->
            {term, Term}
    end.

encode_base64(Data) ->
    encode_base64_bin(Data, <<>>).

encode_base64_bin(<<A:6, B:6, C:6, D:6, Tail/binary>>, Acc) ->
    encode_base64_bin(Tail, <<Acc/binary, (e(A)):8, (e(B)):8, (e(C)):8, (e(D)):8>>);
encode_base64_bin(<<A:6, B:6, C:4>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B)):8, (e(C bsl 2)):8, $=>>;
encode_base64_bin(<<A:6, B:2>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B bsl 4)):8, $=, $=>>;
encode_base64_bin(<<>>, Acc) ->
    Acc.

e(X) when X >= 0, X < 26 -> X + 65;
e(X) when X > 25, X < 52 -> X + 71;
e(X) when X > 51, X < 62 -> X - 4;
e(62) -> $+;
e(63) -> $/;
e(X) -> exit({bad_encode_base64_token, X}).


decode_base64(S) ->
    case catch binary:last(S) of
        C when C == $\n; C == $\s ->
            decode_base64(binary:part(S, 0, byte_size(S) - 1));
        _ ->
            decode_base64_bin(S, <<>>)
    end.

take_without_spaces(Bin, Count) -> 
    take_without_spaces(Bin, Count, <<>>).

take_without_spaces(Bin, 0, Acc) ->
    {Acc, Bin};
take_without_spaces(<<>>, _, Acc) ->
    {Acc, <<>>};
take_without_spaces(<<$\s, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\t, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\n, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\r, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<Char:8, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count-1, <<Acc/binary, Char:8>>).

decode_base64_bin(<<>>, Acc) ->
    Acc;
decode_base64_bin(Bin, Acc) ->
    case take_without_spaces(Bin, 4) of
        {<<A, B, $=, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B) bsr 4):2>>;
        {<<A, B, C, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B)):6, (d(C) bsr 2):4>>;
        {<<A, B, C, D>>, Tail} ->
            Acc2 = <<Acc/binary, (d(A)):6, (d(B)):6, (d(C)):6, (d(D)):6>>,
            decode_base64_bin(Tail, Acc2);
        _ ->
            <<"">>
    end.

d(X) when X >= $A, X =< $Z -> X - 65;
d(X) when X >= $a, X =< $z -> X - 71;
d(X) when X >= $0, X =< $9 -> X + 4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.
