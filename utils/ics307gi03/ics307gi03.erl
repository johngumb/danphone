-module(ics307gi03).
-export([main/0]).

-include_lib("stdlib/include/assert.hrl").

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

% Table 2. VCO Divider
vco_divider(<<_:108, VcoDivider:11, _:13>>) ->
    % VcoDivider is bound to a little endian 11 bit integer in the match.
    % No need to go via bitstrings.
    VcoDivider + 8.

% Table 3. Charge pump current.
charge_pump_current(<<_:3,H:2,_:33,L:3,_:91>>)->
    %Icp = ([128...127]+1)*1.25μA*([~93 ~92 ~91] + 1)

    (H+1)*1.25*((L bxor 2#111) +1).

% continue with low bit of this
return_if_found(NewWord,_Word,_N,_CPI,_CPI)->
    NewWord;

return_if_found(_NewWord,_Word,-1,_ReqCpi,_ActCPI)->
    notfound;

return_if_found(_NewWord,Word,N,ReqCpi,ActCPI)->
    io:format("reqcpi actcpi ~p ~p ~n",[ReqCpi,ActCPI]),
    mod_cpi_h(Word, ReqCpi, N-1).

mod_cpi_h(Word, ReqCpi, N)->
    <<T:3,_:2,M:33,L:3,B:91>> = Word,
    CPI=charge_pump_current(NewWord = <<T:3,N:2,M:33,L:3,B:91>>),
    return_if_found(NewWord,Word,N,ReqCpi,CPI).

mod_cpi(Word, ReqCpi)->
    mod_cpi_h(Word, ReqCpi, 3).
%
% Table 4. Loop filter resistor.
% BUG? Power up value of 16k does not match what this reports
% for the power up word (52k).
%
loop_filter_resistor(<<_:41,0:2,_:89>>)->
    64;
loop_filter_resistor(<<_:41,1:2,_:89>>)->
    52;
loop_filter_resistor(<<_:41,2:2,_:89>>)->
    16;
loop_filter_resistor(<<_:41,3:2,_:89>>)->
    4.

%Nv = 0..3
mod_loop_filter_resistor(<<H:41,V:2,L:89>>, Nv)->
    <<H:41,Nv:2,L:89>>.

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

% Divide Values 38-8232 - needs testing somehow
o1_div(<<_:22, Vh:9, Vl:2, B98:1, 2#101:3, _:95>>)->
    (((Vh + 3) * 2) + (B98 bxor 2#1)) * (1 bsl Vl); % use bsl instead of math:pow

o1_div(<<_:132>>) ->
    error.

calc_O2O3(V, L)->
    VN=((V bxor 2#1111) + 2),
    (L+1) * VN.

% Table 6 Output Divider for Output 2
o2_div(<<_:14, V:4, L:1, _:113>>)->
    calc_O2O3(V, L).

% Table 7 Output Divider for Output 3
o3_div(<<_:10, V:4, _:24, L:1, _:93>>)->
    calc_O2O3(V, L).

% Table 8 Misc Control Bits
misc_bits(<<0:2, OE3:1, MBZ:2, _:2, Clk3Src:1, Clk2Src:1, XtalInput:1,  _:10, P:1, OE2:1, OE1:1, _:109>>)->
    io:format("Clocks 1, 2, 3 enable: ~p ~p ~p ~n",[OE1, OE2, OE3]),
    io:format("XtalInput: ~p~n",[XtalInput]),
    io:format("Clk2Src Clk3Src: ~p ~p~n", [Clk2Src, Clk3Src]),
    io:format("Powerdown ~p~n",[P]),
    io:format("MBZ ~p~n",[MBZ]).

main() ->
    
%    Progword=16#31FFDFFEE3BFFFFFFFFFFFFFFFF055FF2, % default from spec
%    Progword=16#F25F05FFFFFFFFFFFFFFFF3BEEFFFD1F3,
    Progword=16#0803F80000200000000000000001C1FF2, % 116 Mhz calculated

    io:format("pw ~p~n",[integer_to_list(Progword,2)]),

    % report original word
    io:format("~.16B~n",[Progword]),

    Word = <<Progword:132/integer-unsigned-big>>,
    io:format("~p~n",[Word]),


    InputDivider = input_divider:input_divider(Word),
    VcoDivider = vco_divider(Word),

    io:format("Input Divider ~p~n",[InputDivider]),
    io:format("VCO Divider ~p~n",[VcoDivider]),

    CP_current=charge_pump_current(Word),
    io:format("Charge pump current uA ~p~n",[CP_current]),

    LFR=loop_filter_resistor(Word),
    io:format("Loop filter resistor ~pk ~n",[LFR]),

    W2=mod_loop_filter_resistor(Word,2),
    LFR2=loop_filter_resistor(W2),
    io:format("Loop filter resistor after mod ~pk ~n",[LFR2]),

    % report modified word
    <<W2i:132/integer-unsigned-big>>=W2,
    io:format("~.16B~n",[W2i]),

    W3=mod_cpi(Word, 10.0),
    io:format("modded cpi ~p ~n",[W3]),
    <<W3i:132/integer-unsigned-big>>=W3,
    io:format("~.16B~n",[W3i]),

    misc_bits(Word),

    Clk1_output_div=o1_div(Word),
    io:format("CLK1 Output Divider ~p~n",[Clk1_output_div]),

    Clk2_output_div=o2_div(Word),
    io:format("CLK2 Output Divider ~p~n",[Clk2_output_div]),

    Clk3_output_div=o2_div(Word),
    io:format("CLK3 Output Divider ~p~n",[Clk3_output_div]),

    OutputDivider=Clk1_output_div,
    InputFreq = 10, % 10MHz

    Clk1Freq = InputFreq * VcoDivider/(InputDivider * OutputDivider),
    VCOFreq = InputFreq * VcoDivider/InputDivider,
    io:format("CLK Freq ~p~n",[Clk1Freq]),
    io:format("VCO Freq ~p~n",[VCOFreq]),

    ok = ?assert(VCOFreq>100.0),
    ok = ?assert(VCOFreq<730.0)
.


