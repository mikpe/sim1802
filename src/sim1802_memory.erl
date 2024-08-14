%%% -*- erlang-indent-level: 2 -*-
%%%
%%% CDP1802 memory simulation
%%%
%%% We simulate a 64KB RAM initialized to all bits zero, stored in an ETS table.

-module(sim1802_memory).

-export([ init/0
        , get_byte/1
        , set_byte/2
        , write_protect/1
        , is_write_protected/1
        ]).

-export_type([ address/0
             ]).

-define(UINT16_MAX, ((1 bsl 16) - 1)).
-type address() :: 0..?UINT16_MAX.

-define(ETS, ?MODULE).

%% API =========================================================================

-spec init() -> ok.
init() ->
  ets:new(?ETS, [named_table, public]),
  ok.

-spec get_byte(address()) -> byte().
%% TODO: for OTP >= 26 use ets:lookup_element/4
get_byte(Address) ->
  case ets:lookup(?ETS, Address) of
    [{_Address, Byte}] -> Byte;
    [] -> 0
  end.

-spec set_byte(address(), byte()) -> ok.
set_byte(Address, Byte) ->
  ets:insert(?ETS, {Address, Byte}),
  ok.

-spec write_protect(address()) -> ok.
write_protect(Limit) ->
  ets:insert(?ETS, {write_protect, Limit}),
  ok.

-spec is_write_protected(address()) -> boolean().
is_write_protected(Address) ->
  case ets:lookup(?ETS, write_protect) of
    [{_, Limit}] -> Address < Limit;
    [] -> false
  end.
