%%% -*- erlang-indent-level: 2 -*-
%%%
%%% strtol() for Erlang
%%% Copyright (C) 2018-2024  Mikael Pettersson
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Provides C strto[u]l(3) like functionality in Erlang.  Erlang's built-in
%%% support (erlang:list_to_integer/[12], string:to_integer/1, and
%%% io_lib:fread/[23]) is too Erlang-centric to be a drop-in replacement.
%%% Since Erlang has bignums, this code performs no overflow checks.

-module(strtol).
-export([parse/2]).

-type base() :: 0 | 2..36.

-spec parse(string(), base())
      -> {ok, {integer(), string()}} | {error, no_digits}.
parse(String, Base) ->
  scan_spaces(String, Base).

scan_spaces(String, Base) ->
  case String of
    [C | Rest] ->
      case isspace(C) of
        true -> scan_spaces(Rest, Base);
        false -> scan_sign(C, Rest, Base)
      end;
    [] -> no_digits()
  end.

scan_sign(C, Rest, Base) ->
  case C of
    $- -> scan_base(Rest, Base, _Minus = true);
    $+ -> scan_base(Rest, Base, _Minus = false);
    _  -> scan_base([C | Rest], Base, _Minus = false)
  end.

scan_base(String, Base, Minus) ->
  case Base of
    0 ->
      case String of
        [$0, C | Rest] when C =:= $x; C =:= $X ->
          scan_digits(Rest, _Base = 16, Minus);
        [$0 | Rest] ->
          scan_digits(Rest, _Base = 8, Minus);
        _ ->
          scan_digits(String, _Base = 10, Minus)
      end;
    16 ->
      case String of
        [$0, C | Rest] when C =:= $x; C =:= $X ->
          scan_digits(Rest, Base, Minus);
        _ ->
          scan_digits(String, Base, Minus)
      end;
    _ when Base >= 2, Base =< 36 ->
      scan_digits(String, Base, Minus);
    _ ->
      %% This case is a type error, so deserves an exception not an error return.
      error({invalid_base, Base})
  end.

scan_digits(String, Base, Minus) ->
  case String of
    [C | Rest] ->
      D = digit_value(C),
      if D < Base -> scan_digits(Rest, D, Base, Minus);
         true -> no_digits()
      end;
    [] -> no_digits()
  end.

scan_digits(String, Value, Base, Minus) ->
  case String of
    [C | Rest] ->
      D = digit_value(C),
      if D < Base -> scan_digits(Rest, Value * Base + D, Base, Minus);
         true -> return(String, Value, Minus)
      end;
    [] ->
      return([], Value, Minus)
  end.

return(Rest, Value0, Minus) ->
  Value = if Minus -> -Value0; true -> Value0 end,
  {ok, {Value, Rest}}.

no_digits() ->
  {error, no_digits}.

isspace(C) ->
  case C of
    $\s -> true;
    $\f -> true;
    $\n -> true;
    $\r -> true;
    $\t -> true;
    $\v -> true;
    _   -> false
  end.

digit_value(C) ->
  if C >= $0, C =< $9 -> C - $0;
     C >= $A, C =< $Z -> C - $A + 10;
     C >= $a, C =< $z -> C - $a + 10;
     true -> 255 % out-of-band value >= any valid base
  end.
