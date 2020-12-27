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

% Table 2. VCO Divider
vco_divider(<<_:108, VcoDivider:11, _:13>>) ->
    % VcoDivider is bound to a little endian 11 bit integer in the match.
    % No need to go via bitstrings.
    VcoDivider + 8.

mod_vco_divider(<<H:108,_:11,L:13>>, NewDiv)->
    %BNewDiv=binary:decode_unsigned(binary:encode_unsigned(NewDiv),big),
    NW= <<H:108,(NewDiv-8):11/integer-unsigned-big,L:13>>,
    io:format("mod_vco_divider ~p: ~p ~n", [NW]),
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

test(V) ->
    set_sd_dac(V)
.


