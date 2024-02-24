%%% -*- erlang-indent-level: 2 -*-
%%%
%%% I/O for CDP1802 simulator

-ifndef(SIM1802_IO_HRL).
-define(SIM1802_IO_HRL, 1).

%% Port numbers to use with INP/OUT
-define(SIM1802_IO_BUF, 6). % buffer (read/write)
-define(SIM1802_IO_CMD, 7). % command (write-only)

%% Commands to pass in data with OUT 7
-define(SIM1802_CMD_HALT,    16#00). % reads buffer
-define(SIM1802_CMD_PUTCHAR, 16#01). % reads buffer

-endif. % SIM1802_IO_HRL
