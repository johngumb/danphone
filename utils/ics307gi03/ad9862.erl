-module(ad9862).
-export([test/1]).

-include_lib("stdlib/include/assert.hrl").

bin_to_hex(Bin) when is_binary(Bin) ->
    JoinableLength = byte_size(Bin) - 1,
    << Bytes:JoinableLength/binary, LastNibble1:4, LastNibble2:4 >> = Bin,
    [ "<< ",
      [ [ erlang:integer_to_list(Nibble1, 2), erlang:integer_to_list(Nibble2, 16), ", " ]
        || << Nibble1:4, Nibble2:4 >> <= Bytes ],
      erlang:integer_to_list(LastNibble1, 2),
      erlang:integer_to_list(LastNibble2, 2),
      " >>" ].

set_sd_dac(ValueIn)->
    % <<RnW:1,2n1:1,Address:6,Data0:8,Data1:8>>
    Value=binary:decode_unsigned(binary:encode_unsigned(ValueIn),big),
    io:format("Value ~p~n",[Value]),
    SW = <<Value:12>>,
    <<MSB:8, LSN:4>> = SW,

    Cmd1= <<0:1,0:1,42:6,LSN:4,0:3,0:1>>,
    Cmd2= <<0:1,0:1,43:6,MSB:8>>,
    io:format("Cmd1 ~p~n",[Cmd1]),
    io:format("Cmd2 ~p~n",[Cmd2]),
    <<W1:16>>=Cmd1,
    <<W2:16>>=Cmd2,
    io:format("W1 ~p~n",[integer_to_list(W1,16)]),
    io:format("W2 ~p~n",[integer_to_list(W2,16)]),
    %io:format("~.16B~n",[W1]),
    ok.


test(V) ->
    set_sd_dac(V)
.


