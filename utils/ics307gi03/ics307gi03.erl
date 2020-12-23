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
    % VcoDivider is bound to a little endian 11 bit integer in the match.
    % No need to go via bitstrings.
    VcoDivider + 8.

% Table 8 Misc Control Bits
misc_bits(<<0:2, OE3:1, _:4, Clk3Src:1, Clk2Src:1, XtalInput:1,  _:10, P:1, OE2:1, OE1:1, _:109>>)->
    io:format("Clocks 1, 2, 3 enable: ~p ~p ~p ~n",[OE1, OE2, OE3]),
    io:format("XtalInput: ~p~n",[XtalInput]),
    io:format("Clk2Src Clk3Src: ~p ~p~n", [Clk2Src, Clk3Src]),
    io:format("Powerdown ~p~n",[P]).

calc_O2O3(V, L)->
    io:format("L ~p~n",[L]),
    io:format("V ~p~n",[V]),

    VN=((V bxor 2#1111) + 2),
    io:format("V VN ~p ~p~n",[V, VN]),
    case L of 
        0 -> VP=VN;
        1 -> VP=VN*2
    end,
    VP.

% Table 5 Output Divider for Output 1
o1_div(<<_:33, 0:3, L:1, _:95>>)-> % first two rows
    L+2;

o1_div(<<_:33, 8:4, _:95>>)-> % row 3
    4;

o1_div(<<_:34, 2:3, _:95>>)->
    5;

o1_div(<<_:33, 9:4, _:95>>)->
    6;

o1_div(<<_:32, 3:5, _:95>>)->
    7;

o1_div(<<_:32, 2#01011:5, _:95>>)->
    9;

o1_div(<<_:32, 2#10011:5, _:95>>)->
    11;

o1_div(<<_:32, 2#11011:5, _:95>>)->
    13;

o1_div(<<_:29, V:5, 2#100:3, _:95>>)->
    (V bxor 2#11111)+6;

o1_div(<<_:132>>) ->
    error.

% Table 6 Output Divider for Output 2
o2_div(<<_:14, V:4, L:1, _:113>>)->
    calc_O2O3(V, L).

% Table 7 Output Divider for Output 3
o3_div(<<_:10, V:4, _:24, L:1, _:93>>)->
    calc_O2O3(V, L).

main() ->
    
    Progword=16#31FFDFFEE3BFFFFFFFFFFFFFFFF055FF2, % default from spec
%    Progword=16#F25F05FFFFFFFFFFFFFFFF3BEEFFFD1F3,
%    Progword=16#0803F80000200000000000000001C1FF2, % 116 Mhz calculated

    io:format("pw ~p~n",[integer_to_list(Progword,2)]),

    Word = <<Progword:132/integer-unsigned-big>>,

    io:format("~p~n",[Word]),

    InputDivider = input_divider:input_divider(Word),
    VcoDivider = vco_divider(Word),

    io:format("Input Divider ~p~n",[InputDivider]),
    io:format("VCO Divider ~p~n",[VcoDivider]),

    misc_bits(Word),

    Clk1_output_div=o1_div(Word),
    io:format("CLK1 Output Divider ~p~n",[Clk1_output_div]),

    Clk2_output_div=o2_div(Word),
    io:format("CLK2 Output Divider ~p~n",[Clk2_output_div]),

    Clk3_output_div=o2_div(Word),
    io:format("CLK3 Output Divider ~p~n",[Clk3_output_div])
.


