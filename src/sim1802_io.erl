%%% -*- erlang-indent-level: 2 -*-
%%%
%%% I/O for CDP1802 simulator
%%%
%%% The I/O device consists of:
%%%
%%% - an 8-bit argument/result buffer
%%% - an interrupt controller for 8 IRQs 0..7:
%%%   * an 8-bit IRQ enabled register
%%%   * an 8-bit IRQ pending register
%%% - IRQ 0: timer
%%%   * a 2-bit control register
%%% - IRQs 1-5: reserved
%%% - IRQ 6: console
%%%   * an 8-bit input buffer
%%% - IRQ 7: reserved, could be used for daisy-chaining
%%%
%%% The argument/result buffer is written with OUT 6 and read with INP 6.
%%% Commands are written with OUT 7.
%%%
%%% Interrupts are handled as follows:
%%% 1. Device with IRQ I signals an interrupt request.
%%% 2. The interrupt controller sets bit 2^I in the pending register.
%%% 3. If any bit is set in the bitwise AND of the pending and enabled
%%%    registers, an interrupt request is signalled to the CPU.
%%% 4. The interrupt handler sends an INTERRUPT_ACKNOWLEDGE command.
%%% 5. The lowest set bit in the bitwise AND of the pending and enabled
%%%    registers is written to the result buffer, and cleared in the
%%%    pending and enable registers. If no bit is set, 8 is written to
%%%    the result buffer to signal a spurious interrupt. The interrupt
%%%    request to the CPU is cleared.
%%% 6. The interrupt handler reads the buffer and branches to the
%%%    device-specific handler.
%%% 7. Interrupts from the device may be re-enabled by setting its
%%%    bit in the interrupt enable register.

-module(sim1802_io).

-export([ init/0
        , inp/1
        , out/3
        ]).

%% private: enable reloading code
-export([ timer_disabled_loop/0
        , timer_enabled_loop/1
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
  buffer_init(),
  interrupt_init(),
  timer_init(),
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

-define(buffer, buffer).

buffer_init() ->
  ets:insert(?ETS, {?buffer, 0}),
  ok.

read_buffer() ->
  ets:lookup_element(?ETS, ?buffer, 2).

write_buffer(Byte) ->
  ets:update_element(?ETS, ?buffer, {2, Byte}),
  ok.

write_command(Core, Command) ->
  case Command of
    ?SIM1802_CMD_HALT ->
      sim1802_core:halt(Core, read_buffer());
    ?SIM1802_CMD_INTERRUPT_ACKNOWLEDGE ->
      interrupt_acknowledge();
    ?SIM1802_CMD_INTERRUPT_READ_ENABLED ->
      interrupt_read_enabled();
    ?SIM1802_CMD_INTERRUPT_READ_PENDING ->
      interrupt_read_pending();
    ?SIM1802_CMD_INTERRUPT_WRITE_ENABLED ->
      interrupt_write_enabled();
    ?SIM1802_CMD_INTERRUPT_WRITE_PENDING ->
      interrupt_write_pending();
    ?SIM1802_CMD_TIMER_WRITE_CONTROL ->
      timer_write_control();
    ?SIM1802_CMD_CONSOLE_PUTCHAR ->
      console_putchar();
    _ ->
      io:format(standard_error, "@ Invalid I/O command 0x~2.16.0B\n", [Command])
  end.

%% Interrupt Controller ======================================================

-define(enabled, enabled).
-define(pending, pending).
-define(IRQ_SPURIOUS, 8).

interrupt_init() ->
  ets:insert(?ETS, {?enabled, 0}),
  ets:insert(?ETS, {?pending, 0}),
  ok.

interrupt_acknowledge() ->
  Enabled = read_enabled(),
  Pending = read_pending(),
  case Enabled band Pending of
    0 ->
      write_buffer(?IRQ_SPURIOUS);
    Mask ->
      IRQ = ctz(Mask),
      write_enabled(Enabled band bnot (1 bsl IRQ)),
      write_pending(Pending band bnot (1 bsl IRQ)),
      write_buffer(IRQ),
      sim1802_core:clear_interrupt(),
      check_interrupt()
  end.

%% ctz - count trailing zeros
-spec ctz(non_neg_integer()) -> pos_integer().
ctz(NonZero) ->
  %% Width should be larger in general, this value is optimized for byte-sized inputs.
  ctz(NonZero, _Width = 4, _CTZ = 0).

ctz(_NonZero = 1, _Width, CTZ) -> CTZ;
ctz(NonZero, Width, CTZ) ->
  case NonZero band ((1 bsl Width) - 1) of
    0 -> ctz(NonZero bsr Width, Width, CTZ + Width);
    NonZero2 -> ctz(NonZero2, Width div 2, CTZ)
  end.

interrupt_read_enabled() ->
  write_buffer(read_enabled()).

interrupt_read_pending() ->
  write_buffer(read_pending()).

interrupt_write_enabled() ->
  write_enabled(read_buffer()),
  check_interrupt().

interrupt_write_pending() ->
  write_pending(read_buffer()),
  check_interrupt().

set_interrupt(IRQ) ->
  write_pending(read_pending() bor (1 bsl IRQ)),
  check_interrupt().

check_interrupt() ->
  case read_enabled() band read_pending() of
    0 -> ok;
    _ -> sim1802_core:set_interrupt()
  end.

read_enabled() ->
  ets:lookup_element(?ETS, ?enabled, 2).

read_pending() ->
  ets:lookup_element(?ETS, ?pending, 2).

write_enabled(Byte) ->
  ets:update_element(?ETS, ?enabled, {2, Byte}),
  ok.

write_pending(Byte) ->
  ets:update_element(?ETS, ?pending, {2, Byte}),
  ok.

%% Timer =====================================================================

-define(IRQ_TIMER, 0).
-define(timer_pid, timer_pid).

timer_init() ->
  Pid = spawn_link(
          fun() ->
            register(sim1802_timer, self()),
            ?MODULE:timer_disabled_loop()
          end),
  ets:insert(?ETS, {?timer_pid, Pid}).

timer_disabled_loop() ->
  receive
    {reset, Mode} -> timer_reset(Mode)
  after 60_000 ->
    ?MODULE:timer_disabled_loop()
  end.

timer_enabled_loop(DelayMs) ->
  receive
    {reset, Mode} -> timer_reset(Mode)
  after DelayMs ->
    set_interrupt(?IRQ_TIMER),
    ?MODULE:timer_enabled_loop(DelayMs)
  end.

timer_reset(Mode) ->
  case timer_delay(Mode) of
    infinity -> ?MODULE:timer_disabled_loop();
    DelayMs -> timer_enabled_loop(DelayMs)
  end.

timer_delay(Mode) ->
  case Mode of
    0 -> infinity; % disabled
    1 -> 10_000;   % 0.1Hz
    2 ->  1_000;   % 1Hz
    3 ->    100;   % 10Hz
    _ ->
      io:format(standard_error, "@ Invalid timer mode ~p\n", [Mode]),
      infinity
  end.

timer_write_control() ->
  Byte = read_buffer(),
  Pid = ets:lookup_element(?ETS, ?timer_pid, 2),
  Pid ! {reset, Byte},
  ok.

%% Console =====================================================================

console_putchar() ->
  file:write(standard_io, [read_buffer()]),
  ok.
