-module(ics307gi03).
-export([main/0]).

% At power-up, the registers are set to:
% ref divide = 5
% VCO divide = 50
% output divide = 10 (CLK1)
% output divide = 2 (CLK2)
% output divide = 2 (CLK3)
% bit 123, 124 = 1
% ICP = 3.75 μA
% R = 16k
% Default programming word is:
% 0x31FFDFFEE3BFFFFFFFFFFFFFFFF055FF2
% 1100011111111111011111111111101110001110111111111111111111111111111111111111111111111111111111111111111111000001010101111111110010

bin_to_hex(Bin) when is_binary(Bin) ->
    JoinableLength = byte_size(Bin) - 1,
    << Bytes:JoinableLength/binary, LastNibble1:4, LastNibble2:4 >> = Bin,
    [ "<< ",
      [ [ erlang:integer_to_list(Nibble1, 2), erlang:integer_to_list(Nibble2, 16), ", " ]
        || << Nibble1:4, Nibble2:4 >> <= Bytes ],
      erlang:integer_to_list(LastNibble1, 2),
      erlang:integer_to_list(LastNibble2, 2),
      " >>" ].

vco_divider(<<_:108, VcoDivider:11, _:13>>) ->
    VcoDivider + 8.

clk_ena(<<_:20,P:1,C2:1,C1:1,_:109>>)->
    io:format("Clocks 1 and 2 ~p ~p~n",[C1,C2]),
    io:format("Powerdown ~p~n",[P]).

o2_div(W= <<X:14/bitstring, V:4/bitstring, L:1/bitstring, Y:113/bitstring>>)->
    io:format("W ~p~n",[W]),
    io:format("X ~p~n",[X]),
    io:format("L ~p~n",[L]),
    io:format("V ~p~n",[V]),
    io:format("Y ~p~n",[Y]),


    <<Vi:4/integer-unsigned-little>> = V,
    VN=((Vi bxor 2#1111) + 2),
    io:format("Vi VN ~p ~p~n",[Vi, VN]),
    case L of 
        <<0:1>> -> VP=VN;
        <<1:1>> -> VP=VN*2
    end,
    io:format("CLK2 Output Divider ~p~n",[VP]).

report_123_124(<<_:8, Clk1:1, Clk2:1, _:122>>)->
    io:format("Bits 123, 124 ~p ~p~n",[Clk1, Clk2]).

main() ->
    
    Progword=16#31FFDFFEE3BFFFFFFFFFFFFFFFF055FF2,
%    Progword=16#F25F05FFFFFFFFFFFFFFFF3BEEFFFD1F3,
    %Progword=16#0803F80000200000000000000001C1FF2,

    io:format("pw ~p~n",[integer_to_list(Progword,2)]),
    Word = <<Progword:132/integer-unsigned-big>>,

    io:format("~p~n",[Word]),

    io:format("VCO Divider ~p~n",[vco_divider(Word)]),

    io:format("Input Divider ~p~n",[input_divider:input_divider(Word)]),

    clk_ena(Word),
    o2_div(Word),
    report_123_124(Word)
.


