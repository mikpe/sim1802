%%% -*- erlang-indent-level: 2 -*-
%%%
%%% I/O for CDP1802 simulator
%%%
%%% This exposes a virtual I/O device containing control logic and an
%%% 8-bit buffer. The interface to the CDP1802 consists of an 8-bit port
%%% and a 1-bit data/command selector:
%%%
%%% - OUT 6 writes a byte to the port and buffers it
%%% - OUT 7 writes a byte to the port and activates the control logic,
%%%   which reads that byte, and may read or write the buffer
%%% - INP 6 reads a byte from the buffer

-module(sim1802_io).

-export([ init/0
        , inp/1
        , out/3
        ]).

-export_type([ portnr/0
             ]).

-type portnr() :: 1..7.

-include("sim1802_io.hrl").

-define(ETS, ?MODULE).

%% API =========================================================================

-spec init() -> ok.
init() ->
  ets:new(?ETS, [named_table, public]),
  ets:insert(?ETS, {buffer, 0}),
  ok.

-spec inp(portnr()) -> byte().
inp(PortNr) ->
  case PortNr of
    ?SIM1802_IO_BUF -> read_buffer();
    _ -> 0
  end.

-spec out(sim1802_core:core(), portnr(), byte()) -> ok.
out(Core, PortNr, Byte) ->
  case PortNr of
    ?SIM1802_IO_BUF -> write_buffer(Byte);
    ?SIM1802_IO_CMD -> write_command(Core, Byte);
    _ -> ok
  end.

%% Internal ====================================================================

read_buffer() ->
  ets:lookup_element(?ETS, buffer, 2).

write_buffer(Byte) ->
  ets:update_element(?ETS, buffer, {2, Byte}),
  ok.

write_command(Core, Command) ->
  case Command of
    ?SIM1802_CMD_HALT ->
      sim1802_core:halt(Core, read_buffer());
    ?SIM1802_CMD_PUTCHAR ->
      file:write(standard_io, [read_buffer()]),
      ok;
    _ ->
      io:format(standard_error, "@ Invalid I/O command 0x~2.16.0B\n", [Command])
  end.
