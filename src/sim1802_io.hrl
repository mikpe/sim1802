%%% -*- erlang-indent-level: 2 -*-
%%%
%%% I/O for CDP1802 simulator

-ifndef(SIM1802_IO_HRL).
-define(SIM1802_IO_HRL, 1).

%% Port numbers to use with INP/OUT
-define(SIM1802_IO_BUF, 6). % buffer (read/write)
-define(SIM1802_IO_CMD, 7). % command (write-only)

%% Miscellaneous commands:
-define(SIM1802_CMD_HALT,                       16#00).

%% Interrupt controller commands:
-define(SIM1802_CMD_INTERRUPT_ACKNOWLEDGE,      16#10).
-define(SIM1802_CMD_INTERRUPT_READ_ENABLED,     16#11).
-define(SIM1802_CMD_INTERRUPT_READ_PENDING,     16#12).
-define(SIM1802_CMD_INTERRUPT_WRITE_ENABLED,    16#13).
-define(SIM1802_CMD_INTERRUPT_WRITE_PENDING,    16#14).

%% Timer commands:
-define(SIM1802_CMD_TIMER_WRITE_CONTROL,        16#80). % 0=Off, 1=0.1Hz, 2=1Hz, 3=10Hz

%% Console commands:
-define(SIM1802_CMD_CONSOLE_PUTCHAR,            16#E0).
-define(SIM1802_CMD_CONSOLE_GETCHAR,            16#E1).

-endif. % SIM1802_IO_HRL
