-module(input_divider).
-export([input_divider/1]).

input_divider(<<_:130, 2#0:2>>) ->
    1;

input_divider(<<_:130, 2#1:2>>) ->
    2;

input_divider(<<_:126, 2#111010:6>>) ->
    3;

input_divider(<<_:126, 2#110110:6>>) ->
    4;

input_divider(<<_:126, 2#110010:6>>) ->
    5;

input_divider(<<_:126, 2#101110:6>>) ->
    6;

input_divider(<<_:126, 2#101010:6>>) ->
    7;

input_divider(<<_:126, 2#100110:6>>) ->
    8;

input_divider(<<_:126, 2#100010:6>>) ->
    9;

input_divider(<<_:126, 2#011110:6>>) ->
    10;

input_divider(<<_:126, 2#011010:6>>) ->
    11;

input_divider(<<_:126, 2#010110:6>>) ->
    12;

input_divider(<<_:126, 2#010010:6>>) ->
    13;

input_divider(<<_:126, 2#001110:6>>) ->
    14;

input_divider(<<_:126, 2#001010:6>>) ->
    15;

input_divider(<<_:126, 2#000110:6>>) ->
    16;

input_divider(<<_:126, 2#000001:6>>) ->
    17;

input_divider(<<_:119, E:11, 2#11:2>>) ->
    io:format("erk~n"),
    E+8;

input_divider(_) ->
    255.
