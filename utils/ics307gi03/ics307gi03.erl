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

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

hexlify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

% Table 2. VCO Divider
vco_divider(<<_:108, VcoDivider:11, _:13>>) ->
    % VcoDivider is bound to a little endian 11 bit integer in the match.
    % No need to go via bitstrings.
    VcoDivider + 8.

mod_vco_divider(<<H:108,_:11,L:13>>, NewDiv)->
    %BNewDiv=binary:decode_unsigned(binary:encode_unsigned(NewDiv),big),
    NW= <<H:108,(NewDiv-8):11,L:13>>,
    io:format("mod_vco_divider ~p: ~p ~n", [NewDiv, NW]),
    NW.

% Table 3. Charge pump current.
charge_pump_current(<<_:3,H:2,_:33,L:3,_:91>>)->
    %Icp = ([128...127]+1)*1.25μA*([~93 ~92 ~91] + 1)

    (H+1)*1.25*((L bxor 2#111) +1).

return_if_found(NewWord,_Word,N,L,_CPI,_CPI)->
    %io:format("N ~p L ~p ~n", [N, L]),
    NewWord;

return_if_found(_NewWord,_Word,-1,-1,_ReqCpi,_ActCPI)->
    notfound;

return_if_found(_NewWord,Word,-1,L,ReqCpi,ActCPI)->
    %io:format("reqcpi actcpi ~p ~p ~n",[ReqCpi,ActCPI]),
    mod_cpi_h(Word, ReqCpi, 3, L-1);

return_if_found(_NewWord,Word,N,L,ReqCpi,ActCPI)->
    %io:format("reqcpi actcpi ~p ~p ~n",[ReqCpi,ActCPI]),
    mod_cpi_h(Word, ReqCpi, N-1, L).

mod_cpi_h(Word, ReqCpi, N, L)->
    <<T:3,_:2,M:33,_:3,B:91>> = Word,
    CPI=charge_pump_current(NewWord = <<T:3,N:2,M:33,L:3,B:91>>),
    return_if_found(NewWord,Word,N,L,ReqCpi,CPI).


mod_cpi(Word, ReqCpi)->
    mod_cpi_h(Word, ReqCpi, 3, 7).
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
mod_loop_filter_resistor(<<H:41,_V:2,L:89>>, Nv)->
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

return_or_continue(_W, NW, _ReqDiv, _ReqDiv, _V, _Limit, _TestFun)->
    NW;

return_or_continue(_W, _NW, _, _, _Limit, _Limit, _TestFun) ->
    notfound;

return_or_continue(W, _NW, ReqDiv, _ActDiv, V, _Limit, TestFun)->
    TestFun(W, ReqDiv, V+1).

mod_o1div_worker(W, ReqDiv, V)->
    <<H:22, _:15, L:95>> = W,
    ActDiv = o1_div(NW= <<H:22, V:15, L:95>>),
    return_or_continue(W, NW, ReqDiv, ActDiv, V, 32768, fun mod_o1div_worker/3).

mod_o1div(W, ReqDiv)->
    mod_o1div_worker(W, ReqDiv, 0).

calc_O2O3(V, L)->
    VN=((V bxor 2#1111) + 2),
    (L+1) * VN.

% Table 6 Output Divider for Output 2
o2_div(<<_:14, V:4, L:1, _:113>>)->
    calc_O2O3(V, L).

% Table 7 Output Divider for Output 3
o3_div(<<_:10, V:4, _:24, L:1, _:93>>)->
    calc_O2O3(V, L).

mod_o2div_worker(W, ReqDiv, V)->
    <<H:14, _:5, L:113>> = W,
    ActDiv = o2_div(NW= <<H:14, V:5, L:113>>),
    return_or_continue(W, NW, ReqDiv, ActDiv, V, _Limit=32, fun mod_o2div_worker/3).

mod_o2div(W, ReqDiv)->
    mod_o2div_worker(W, ReqDiv, 0).

mod_o3div_worker(W, ReqDiv, V)->
    <<H:10, _:4, M:24, _:1, L:93>> = W,
    <<_:3, VU:4, VL:1>> = <<V>>,
    ActDiv = o3_div(NW= <<H:10, VU:4, M:24, VL:1, L:93>>),
    return_or_continue(W, NW, ReqDiv, ActDiv, V, _Limit=32, fun mod_o3div_worker/3).

mod_o3div(W, ReqDiv)->
    mod_o3div_worker(W, ReqDiv, 0).

% Table 8 Misc Control Bits
report_misc_bits(<<0:2, OE3:1, _ICP:2, 0:2, Clk3Src:1, Clk2Src:1, XtalInput:1, _ODiv:9, P:1, OE2:1, OE1:1, _:110>>)->
    io:format("Clocks 1, 2, 3 enable: ~p ~p ~p ~n",[OE1, OE2, OE3]),
    io:format("XtalInput: ~p~n",[XtalInput]),
    io:format("Clk2Src Clk3Src: ~p ~p~n", [Clk2Src, Clk3Src]),
    io:format("Powerdown ~p~n",[P]).

mod_misc_bits(<<0:2, _OE3:1, ICP:2, 0:2, Clk3Src:1, Clk2Src:1, XtalInput:1, ODiv:9, P:1, _OE2:1, _OE1:1, L:110>>, NOE1, NOE2, NOE3)->
    <<0:2, NOE3:1, ICP:2, 0:2, Clk3Src:1, Clk2Src:1, XtalInput:1, ODiv:9, P:1, NOE2:1, NOE1:1, L:110>>.

report_input_divider(W)->
    InputDivider = input_divider:input_divider(W),
    io:format("Input Divider ~p~n",[InputDivider]),    
    InputDivider.

report_vco_divider(W)->
    VcoDivider = vco_divider(W),
    io:format("VCO Divider ~p~n",[VcoDivider]),
    VcoDivider.

report_charge_pump_current(W)->
    CP_current=charge_pump_current(W),
    io:format("Charge pump current uA ~p~n",[CP_current]),
    CP_current.

report_loop_filter_resistor(W)->
    LFR=loop_filter_resistor(W),
    io:format("Loop filter resistor ~pk ~n",[LFR]),
    LFR.

report_clk1_output_divider(W)->
    OutputDivider1=o1_div(W),
    io:format("CLK1 Output Divider ~p~n",[OutputDivider1]),
    OutputDivider1.

report_clk2_output_divider(W)->
    OutputDivider2=o2_div(W),
    io:format("CLK2 Output Divider ~p~n",[OutputDivider2]),
    OutputDivider2.

report_clk3_output_divider(W)->
    OutputDivider3=o3_div(W),
    io:format("CLK3 Output Divider ~p~n",[OutputDivider3]),
    OutputDivider3.

fmt_word_worker(<<>>)->
    io:format("~n"),
    ok;

fmt_word_worker(<<B,Rest/bitstring>>)->
    io:format("0x~2.16.0B, ",[B]),
    fmt_word_worker(Rest).

fmt_word(W)->
    <<Wi:132>> =W,
    PaddedW= <<0:4,Wi:132>>,
    fmt_word_worker(PaddedW).

report_word(W)->
    <<Wi:132>>=W,
    %io:format("~p~n",[W]),
    io:format("+++++ Progword ~.16B ++++~n", [Wi]),

    fmt_word(W),

    InputDivider=report_input_divider(W),
    VcoDivider=report_vco_divider(W),
    OutputDivider1 = report_clk1_output_divider(W),
    OutputDivider2 = report_clk2_output_divider(W),
    OutputDivider3 = report_clk3_output_divider(W),

    InputFreq = 10, % 10MHz

    VCOFreq = InputFreq * VcoDivider/InputDivider,
    Clk1Freq = VCOFreq/OutputDivider1,
    Clk2Freq = VCOFreq/OutputDivider2,
    Clk3Freq = VCOFreq/OutputDivider3,

    io:format("VCO Freq ~p~n",[VCOFreq]),
    io:format("CLK1 Freq ~p~n",[Clk1Freq]),
    io:format("CLK2 Freq ~p~n",[Clk2Freq]),
    io:format("CLK3 Freq ~p~n",[Clk3Freq]),

    ok = ?assert(VCOFreq >= 100.0),
    ok = ?assert(VCOFreq =< 730.0),

    report_loop_filter_resistor(W),
    report_charge_pump_current(W),
    report_misc_bits(W),
    io:format("----- Progword ~.16B ----~n~n", [Wi])
.

main() ->
    
%    Progword=16#31FFDFFEE3BFFFFFFFFFFFFFFFF055FF2, % default from spec
%    Progword=16#F25F05FFFFFFFFFFFFFFFF3BEEFFFD1F3,
    Progword=16#0803F80000200000000000000001C1FF2, % 116 Mhz calculated

%    Progword=16#1003DC06E1200000000000000000D9FF2, % clk1 23.2, clk2 116 calculated

    io:format("pw ~p~n",[integer_to_list(Progword,2)]),

    % report original word
    io:format("~.16B~n",[Progword]),

    OrigWord = <<Progword:132>>,

    report_word(OrigWord),

    %% io:format("~p~n",[OrigWord]),




    %% WW1=mod_vco_divider(OrigWord, 232),
    %% W2=mod_loop_filter_resistor(Word,2),

    %% % report modified word
    %% <<W2i:132/integer-unsigned-big>>=W2,
    %% io:format("~.16B~n",[W2i]),

    %% NewCpi=26.25,
    %% W3=mod_cpi(Word, NewCpi),
    %% io:format("modded cpi for ~p: ~p ~n",[NewCpi, W3]),
    %% <<W3i:132/integer-unsigned-big>>=W3,
    %% io:format("~.16B~n",[W3i]),

    %% misc_bits(Word),
    XW=mod_misc_bits(OrigWord,1,1,0),
    report_word(XW),
    


    %% NV=20,
    C1W=mod_o1div(XW, 20),
    report_word(C1W),
    %% io:format("CLK1 Output Divider modded ~p ~p ~n",[NV, C1W]),


    C3W=mod_o3div(C1W, 4),
    report_word(C3W),




    ad9862:test(1 + 128 + 256 + 512)
.


