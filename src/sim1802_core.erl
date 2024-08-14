%%% -*- erlang-indent-level: 2 -*-
%%%
%%% CDP1802 CPU core simulation with partial CDP1804AC support

-module(sim1802_core).

-export([ clear_ef1/0
        , clear_ef2/0
        , clear_ef3/0
        , clear_ef4/0
        , get_byte/2
        , get_d/1
        , get_df/1
        , get_p/1
        , get_r/2
        , get_word/2
        , get_x/1
        , halt/2
        , init/2
        , reset/1
        , set_ef1/0
        , set_ef2/0
        , set_ef3/0
        , set_ef4/0
        , step/1
        ]).

-export_type([ core/0
             ]).

-type uint1_t() :: 0..1.

-define(UINT4_MAX, ((1 bsl 4) - 1)).
-type uint4_t() :: 0..?UINT4_MAX.

-define(UINT8_MAX, ((1 bsl 8) - 1)).
-type uint8_t() :: 0..?UINT8_MAX.

-define(UINT16_MAX, ((1 bsl 16) - 1)).
-type uint16_t() :: 0..?UINT16_MAX.

-type regs() :: { uint16_t(), uint16_t(), uint16_t(), uint16_t() % R(0)..R(3)
                , uint16_t(), uint16_t(), uint16_t(), uint16_t() % R(4)..R(7)
                , uint16_t(), uint16_t(), uint16_t(), uint16_t() % R(8)..R(11)
                , uint16_t(), uint16_t(), uint16_t(), uint16_t() % R(12)..R(15)
                }.

-record(core,
        %% The I and N registers are excluded since they never live between instructions.
        { r   :: regs()    % R: 16 16-bit scratchpad registers
        , p   :: uint4_t() % P: designates which R-register is program counter
        , x   :: uint4_t() % X: designates which R-register is data/stack pointer
        , d   :: uint8_t() % D: 8-bit accumulator
        , df  :: uint1_t() % Data Flag, signals carry or borrow
        , ie  :: uint1_t() % interrupt enable flip-flop
        , t   :: uint8_t() % temporary holding <X,P> after interrupt
        , q   :: uint1_t() % Programmable flip-flop
        , trc :: boolean() % Trace each instruction being executed
        , symtab :: sim1802_symtab:symtab() % Symbol table from image file
        }).

-type core() :: #core{}.

%% CDP1802 opcodes
-define(OP_IDL,  16#00). % IDLE
-define(OP_LDN,  16#00). % LOAD VIA N, N NOT 0, 0N
-define(OP_INC,  16#10). % INCREMENT REG N, 1N
-define(OP_DEC,  16#20). % DECREMENT REG N, 2N
-define(OP_BR,   16#30). % UNCONDITIONAL SHORT BRANCH
-define(OP_BQ,   16#31). % SHORT BRANCH IF Q=1
-define(OP_BZ,   16#32). % SHORT BRANCH IF D=0
-define(OP_BDF,  16#33). % SHORT BRANCH IF DF=1 (alias: BPZ = SHORT BRANCH IF POS OR ZERO, BGE = SHORT BRANCH IF EQUAL OR GREATER)
-define(OP_B1,   16#34). % SHORT BRANCH IF EF1=1
-define(OP_B2,   16#35). % SHORT BRANCH IF EF2=1
-define(OP_B3,   16#36). % SHORT BRANCH IF EF3=1
-define(OP_B4,   16#37). % SHORT BRANCH IF EF4=1
-define(OP_NBR,  16#38). % NO SHORT BRANCH (alias: SKP = SHORT SKIP)
-define(OP_BNQ,  16#39). % SHORT BRANCH IF Q=0
-define(OP_BNZ,  16#3A). % SHORT BRANCH IF D NOT 0
-define(OP_BNF,  16#3B). % SHORT BRANCH IF DF=0 (alias: BM = SHORT BRANCH IF MINUS, BL = SHORT BRANCH IF LESS)
-define(OP_BN1,  16#3C). % SHORT BRANCH IF EF1=0
-define(OP_BN2,  16#3D). % SHORT BRANCH IF EF2=0
-define(OP_BN3,  16#3E). % SHORT BRANCH IF EF3=0
-define(OP_BN4,  16#3F). % SHORT BRANCH IF EF4=0
-define(OP_LDA,  16#40). % LOAD ADVANCE, 4N
-define(OP_STR,  16#50). % STORE VIA N, 5N
-define(OP_IRX,  16#60). % INCREMENT REG X
-define(OP_OUT,  16#60). % OUTPUT, 6N, N=1-7
-define(OP_68,   16#68). % illegal opcode (1802) or prefix for second opcode (1804/1805/1806)
-define(OP_INP,  16#60). % INPUT, 6N, N=9-F
-define(OP_RET,  16#70). % RETURN
-define(OP_DIS,  16#71). % DISABLE
-define(OP_LDXA, 16#72). % LOAD VIA X AND ADVANCE
-define(OP_STXD, 16#73). % STORE VIA X AND DECREMENT
-define(OP_ADC,  16#74). % ADD WITH CARRY
-define(OP_SDB,  16#75). % SUBTRACT D WITH BORROW
-define(OP_SHRC, 16#76). % SHIFT RIGHT WITH CARRY (alias: RSHR = RING SHIFT RIGHT)
-define(OP_SMB,  16#77). % SUBTRACT MEMORY WITH BORROW
-define(OP_SAV,  16#78). % SAVE
-define(OP_MARK, 16#79). % PUSH X, P TO STACK
-define(OP_REQ,  16#7A). % RESET Q
-define(OP_SEQ,  16#7B). % SET Q
-define(OP_ADCI, 16#7C). % ADD WITH CARRY IMMEDIATE
-define(OP_SDBI, 16#7D). % SUBTRACT D WITH BORROW IMMEDIATE
-define(OP_SHLC, 16#7E). % SHIFT LEFT WITH CARRY (alias: RSHL = RING SHIFT LEFT)
-define(OP_SMBI, 16#7F). % SUBTRACT MEMORY WITH BORROW IMMEDIATE
-define(OP_GLO,  16#80). % GET LOW REG N, 8N
-define(OP_GHI,  16#90). % GET HIGH REG N, 9N
-define(OP_PLO,  16#A0). % PUT LOW REG N, AN
-define(OP_PHI,  16#B0). % PUT HIGH REG N, BN
-define(OP_LBR,  16#C0). % LONG BRANCH
-define(OP_LBQ,  16#C1). % LONG BRANCH IF Q=1
-define(OP_LBZ,  16#C2). % LONG BRANCH IF D=0
-define(OP_LBDF, 16#C3). % LONG BRANCH IF DF=1
-define(OP_NOP,  16#C4). % NO OPERATION
-define(OP_LSNQ, 16#C5). % LONG SKIP IF Q=0
-define(OP_LSNZ, 16#C6). % LONG SKIP IF D NOT 0
-define(OP_LSNF, 16#C7). % LONG SKIP IF DF=0
-define(OP_NLBR, 16#C8). % NO LONG BRANCH (alias: LSKP = LONG SKIP)
-define(OP_LBNQ, 16#C9). % LONG BRANCH IF Q=0
-define(OP_LBNZ, 16#CA). % LONG BRANCH IF D NOT 0
-define(OP_LBNF, 16#CB). % LONG BRANCH IF DF=0
-define(OP_LSIE, 16#CC). % LONG SKIP IF IE=1
-define(OP_LSQ,  16#CD). % LONG SKIP IF Q=1
-define(OP_LSZ,  16#CE). % LONG SKIP IF D=0
-define(OP_LSDF, 16#CF). % LONG SKIP IF DF=1
-define(OP_SEP,  16#D0). % SET P, DN
-define(OP_SEX,  16#E0). % SET X, EN
-define(OP_LDX,  16#F0). % LOAD VIA X
-define(OP_OR,   16#F1). % OR
-define(OP_AND,  16#F2). % AND
-define(OP_XOR,  16#F3). % EXCLUSIVE-OR
-define(OP_ADD,  16#F4). % ADD
-define(OP_SD,   16#F5). % SUBTRACT D
-define(OP_SHR,  16#F6). % SHIFT RIGHT
-define(OP_SM,   16#F7). % SUBTRACT MEMORY
-define(OP_LDI,  16#F8). % LOAD IMMEDIATE
-define(OP_ORI,  16#F9). % OR IMMEDIATE
-define(OP_ANI,  16#FA). % AND IMMEDIATE
-define(OP_XRI,  16#FB). % EXCLUSIVE-OR IMMEDIATE
-define(OP_ADI,  16#FC). % ADD IMMEDIATE
-define(OP_SDI,  16#FD). % SUBTRACT D IMMEDIATE
-define(OP_SHL,  16#FE). % SHIFT LEFT
-define(OP_SMI,  16#FF). % SUBTRACT MEMORY IMMEDIATE

%% CDP1804 opcodes, superset of CDP1802, all prefixed by 16#68.
-define(OP_STPC, 16#00). % 6800: STOP COUNTER
-define(OP_DTC,  16#01). % 6801: DECREMENT COUNTER
-define(OP_SPM2, 16#02). % 6802: SET PULSE WIDTH MODE 2, START
-define(OP_SCM2, 16#03). % 6803: SET COUNTER MODE 2 AND START
-define(OP_SPM1, 16#04). % 6804: SET PULSE WIDTH MODE 1, START
-define(OP_SCM1, 16#05). % 6805: SET COUNTER MODE 1 AND START
-define(OP_LDC,  16#06). % 6808: LOAD COUNTER
-define(OP_STM,  16#07). % 6807: SET TIMER MODE AND START
-define(OP_GEC,  16#08). % 6808: GET COUNTER
-define(OP_ETQ,  16#09). % 6809: ENABLE TOGGLE Q
-define(OP_XIE,  16#0A). % 680A: EXTERNAL INTERRUPT ENABLE
-define(OP_XID,  16#0B). % 680B: EXTERNAL INTERRUPT DISABLE
-define(OP_CIE,  16#0C). % 680C: COUNTER INTERRUPT ENABLE
-define(OP_CID,  16#0D). % 680D: COUNTER INTERRUPT DISABLE
-define(OP_BCI,  16#3E). % 683E: SHORT BRANCH ON COUNTER INTERRUPT
-define(OP_BXI,  16#3F). % 683F: SHORT BRANCH ON EXTERNAL INTERRUPT
-define(OP_RLXA, 16#60). % 686N: REGISTER LOAD VIA X AND ADVANCE
-define(OP_SCAL, 16#80). % 688N: STANDARD CALL
-define(OP_SRET, 16#90). % 689N: STANDARD RETURN
-define(OP_RSXD, 16#A0). % 68AN: REGISTER STORE VIA X AND DECREMENT
-define(OP_RNX,  16#B0). % 68BN: REGISTER N TO REGISTER X COPY
-define(OP_RLDI, 16#C0). % 68CN: REGISTER LOAD IMMEDIATE

%% CDP1804AC opcodes, superset of CDP1804, all prefixed by 16#68.
-define(OP_DBNZ, 16#20). % 682N: DECREMENT REG N AND LONG BRANCH IF NOT EQUAL 0
-define(OP_DADC, 16#74). % 6874: DECIMAL ADD WITH CARRY
-define(OP_DSAV, 16#76). % 6876: SAVE T, D, DF
-define(OP_DSMB, 16#77). % 6877: DECIMAL SUBTRACT MEMORY WITH BORROW
-define(OP_DACI, 16#7C). % 687C: DECIMAL ADD WITH CARRY, IMMEDIATE
-define(OP_DSBI, 16#7F). % 687F: DECIMAL SUBTRACT MEMORY WITH BORROW, IMMEDIATE
-define(OP_DADD, 16#F4). % 68F4: DECIMAL ADD
-define(OP_DSM,  16#F7). % 68F7: DECIMAL SUBTRACT MEMORY
-define(OP_DADI, 16#FC). % 68FC: DECIMAL ADD IMMEDIATE
-define(OP_DSMI, 16#FF). % 68FF: DECIMAL SUBTRACT MEMORY, IMMEDIATE

%% Simulator debugging opcodes, all prefixed by 16#68.
-define(OP_DBG0, 16#10). % 6810: print PC and SP
-define(OP_DBG1, 16#11). % 6811: print PC, SP, r7, r10, r15
-define(OP_DBG2, 16#12). % 6812: print PC, SP, argv[1] (*(void*)(r8+2))
-define(OP_NYI,  16#1F). % 681F: print D and exit 98

%% ETS for recording state of input signals.
-define(SIGNALS_ETS, ?MODULE).
-define(SIGNAL_EF1, ef1).
-define(SIGNAL_EF2, ef2).
-define(SIGNAL_EF3, ef3).
-define(SIGNAL_EF4, ef4).

%% Simulator Control ===========================================================

-spec init(Trace :: boolean(), SymTab :: sim1802_symtab:symtab()) -> core().
init(Trace, SymTab) ->
  init_signals(),
  #core{ r   = erlang:make_tuple(16, 0)
       , p   = 0
       , x   = 0
       , d   = 0
       , df  = 0
       , ie  = 1
       , t   = 0
       , q   = 0
       , trc = Trace
       , symtab = SymTab
       }.

-spec reset(core()) -> core().
reset(Core) ->
  #core{x = X, p = P, r = R} = Core,
  %% See do_step/1. No need for the special post-reset "init" cycle.
  Core#core{ q = 0
           , ie = 1
           , t = (X bsl 4) bor P
           , x = 0
           , p = 0
           , r = setelement(0 + 1, R, 0)
           }.

-spec step(core()) -> {ok, core()} | {error, {core(), any()}}.
step(Core) ->
  try {ok, do_step(Core)}
  catch throw:{Core1, Reason} -> {error, {Core1, Reason}}
  end.

-spec set_ef1() -> ok.
set_ef1() ->
  set_signal(?SIGNAL_EF1).

-spec set_ef2() -> ok.
set_ef2() ->
  set_signal(?SIGNAL_EF2).

-spec set_ef3() -> ok.
set_ef3() ->
  set_signal(?SIGNAL_EF3).

-spec set_ef4() -> ok.
set_ef4() ->
  set_signal(?SIGNAL_EF4).

-spec clear_ef1() -> ok.
clear_ef1() ->
  clear_signal(?SIGNAL_EF1).

-spec clear_ef2() -> ok.
clear_ef2() ->
  clear_signal(?SIGNAL_EF2).

-spec clear_ef3() -> ok.
clear_ef3() ->
  clear_signal(?SIGNAL_EF3).

-spec clear_ef4() -> ok.
clear_ef4() ->
  clear_signal(?SIGNAL_EF4).

%% Traps =======================================================================
%% Traps are generated by throwing {Core, Reason} letting step/1 catch.

-spec halt(core(), byte()) -> no_return().
-compile({no_auto_import, [halt/2]}).
halt(Core, Status) ->
  throw({Core, {halt, Status}}).

%% Instruction sequencing ======================================================

do_step(Core) ->
  Core1 = fetch_and_execute(Core),
  %% Interrupts occur after the S1 (execute) cycle.
  %% This means that the special "init" cycle after reset isn't needed.
  case pred_IE_NZ(Core1) andalso sim1802_io:is_interrupt() of
    true -> interrupt(Core1);
    false -> Core1
  end.

fetch_and_execute(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  Opcode = get_byte(Core, A),
  trace(Core, A, Opcode),
  execute(set_r(Core, P, uint16_inc(A)), Opcode).

trace(#core{trc = false}, _A, _Opcode) -> ok;
trace(#core{symtab = SymTab}, A, Opcode) ->
  Address =
    case sim1802_symtab:resolve(SymTab, A) of
      {Name, Offset} ->
        io_lib:format("~s+~.16.0b (~4.16.0b)", [Name, Offset, A]);
      false ->
        io_lib:format("~4.16.0b", [A])
    end,
  io:format(standard_error, "@ ~s op ~2.16.0b\n", [Address, Opcode]).

execute(Core, Opcode) ->
  case Opcode of
    ?OP_IDL  -> emu_IDL(Core);  % 00

    ?OP_BR   -> emu_BR(Core);   % 30
    ?OP_BQ   -> emu_BQ(Core);   % 31
    ?OP_BZ   -> emu_BZ(Core);   % 32
    ?OP_BDF  -> emu_BDF(Core);  % 33
    ?OP_B1   -> emu_B1(Core);   % 34
    ?OP_B2   -> emu_B2(Core);   % 35
    ?OP_B3   -> emu_B3(Core);   % 36
    ?OP_B4   -> emu_B4(Core);   % 37
    ?OP_NBR  -> emu_NBR(Core);  % 38
    ?OP_BNQ  -> emu_BNQ(Core);  % 39
    ?OP_BNZ  -> emu_BNZ(Core);  % 3A
    ?OP_BNF  -> emu_BNF(Core);  % 3B
    ?OP_BN1  -> emu_BN1(Core);  % 3C
    ?OP_BN2  -> emu_BN2(Core);  % 3D
    ?OP_BN3  -> emu_BN3(Core);  % 3E
    ?OP_BN4  -> emu_BN4(Core);  % 3F

    ?OP_IRX  -> emu_IRX(Core);  % 60
    ?OP_68   -> emu_68(Core);   % 68

    ?OP_RET  -> emu_RET(Core);  % 70
    ?OP_DIS  -> emu_DIS(Core);  % 71
    ?OP_LDXA -> emu_LDXA(Core); % 72
    ?OP_STXD -> emu_STXD(Core); % 73
    ?OP_ADC  -> emu_ADC(Core);  % 74
    ?OP_SDB  -> emu_SDB(Core);  % 75
    ?OP_SHRC -> emu_SHRC(Core); % 76
    ?OP_SMB  -> emu_SMB(Core);  % 77
    ?OP_SAV  -> emu_SAV(Core);  % 78
    ?OP_MARK -> emu_MARK(Core); % 79
    ?OP_REQ  -> emu_REQ(Core);  % 7A
    ?OP_SEQ  -> emu_SEQ(Core);  % 7B
    ?OP_ADCI -> emu_ADCI(Core); % 7C
    ?OP_SDBI -> emu_SDBI(Core); % 7D
    ?OP_SHLC -> emu_SHLC(Core); % 7E
    ?OP_SMBI -> emu_SMBI(Core); % 7F

    ?OP_LBR  -> emu_LBR(Core);  % C0
    ?OP_LBQ  -> emu_LBQ(Core);  % C1
    ?OP_LBZ  -> emu_LBZ(Core);  % C2
    ?OP_LBDF -> emu_LBDF(Core); % C3
    ?OP_NOP  -> emu_NOP(Core);  % C4
    ?OP_LSNQ -> emu_LSNQ(Core); % C5
    ?OP_LSNZ -> emu_LSNZ(Core); % C6
    ?OP_LSNF -> emu_LSNF(Core); % C7
    ?OP_NLBR -> emu_NLBR(Core); % C8
    ?OP_LBNQ -> emu_LBNQ(Core); % C9
    ?OP_LBNZ -> emu_LBNZ(Core); % CA
    ?OP_LBNF -> emu_LBNF(Core); % CB
    ?OP_LSIE -> emu_LSIE(Core); % CC
    ?OP_LSQ  -> emu_LSQ(Core);  % CD
    ?OP_LSZ  -> emu_LSZ(Core);  % CE
    ?OP_LSDF -> emu_LSDF(Core); % CF

    ?OP_LDX  -> emu_LDX(Core);  % F0
    ?OP_OR   -> emu_OR(Core);   % F1
    ?OP_AND  -> emu_AND(Core);  % F2
    ?OP_XOR  -> emu_XOR(Core);  % F3
    ?OP_ADD  -> emu_ADD(Core);  % F4
    ?OP_SD   -> emu_SD(Core);   % F5
    ?OP_SHR  -> emu_SHR(Core);  % F6
    ?OP_SM   -> emu_SM(Core);   % F7
    ?OP_LDI  -> emu_LDI(Core);  % F8
    ?OP_ORI  -> emu_ORI(Core);  % F9
    ?OP_ANI  -> emu_ANI(Core);  % FA
    ?OP_XRI  -> emu_XRI(Core);  % FB
    ?OP_ADI  -> emu_ADI(Core);  % FC
    ?OP_SDI  -> emu_SDI(Core);  % FD
    ?OP_SHL  -> emu_SHL(Core);  % FE
    ?OP_SMI  -> emu_SMI(Core);  % FF

    _ ->
      N = Opcode band 16#0F,
      case Opcode bsr 4 of
        ?OP_LDN bsr 4 -> emu_LDN(Core, N); % 0N, N NOT 0, 00 = IDL handled above
        ?OP_INC bsr 4 -> emu_INC(Core, N); % 1N
        ?OP_DEC bsr 4 -> emu_DEC(Core, N); % 2N
        ?OP_LDA bsr 4 -> emu_LDA(Core, N); % 4N
        ?OP_STR bsr 4 -> emu_STR(Core, N); % 5N
        16#60   bsr 4 -> % 60 and 68 handled above
          case N >= 9 of
            true      -> emu_INP(Core, N); % 6N, N=9-F
            false     -> emu_OUT(Core, N)  % 6N, N=1-7
          end;
        ?OP_GLO bsr 4 -> emu_GLO(Core, N); % 8N
        ?OP_GHI bsr 4 -> emu_GHI(Core, N); % 9N
        ?OP_PLO bsr 4 -> emu_PLO(Core, N); % AN
        ?OP_PHI bsr 4 -> emu_PHI(Core, N); % BN
        ?OP_SEP bsr 4 -> emu_SEP(Core, N); % DN
        ?OP_SEX bsr 4 -> emu_SEX(Core, N)  % EN
      end
  end.

emu_68(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  execute_68(set_r(Core, P, uint16_inc(A)), get_byte(Core, A)).

execute_68(Core, Opcode) ->
  N = Opcode band 16#0F,
  case Opcode bsr 4 of
    ?OP_DBNZ bsr 4 -> emu_DBNZ(Core, N); % 682N
    ?OP_RLXA bsr 4 -> emu_RLXA(Core, N); % 686N
    ?OP_SCAL bsr 4 -> emu_SCAL(Core, N); % 688N
    ?OP_SRET bsr 4 -> emu_SRET(Core, N); % 689N
    ?OP_RSXD bsr 4 -> emu_RSXD(Core, N); % 68AN
    ?OP_RNX  bsr 4 -> emu_RNX (Core, N); % 68BN
    ?OP_RLDI bsr 4 -> emu_RLDI(Core, N); % 68CN
    %% TODO: handle more CDP1804AC extended opcodes
    _ ->
      case Opcode of
        ?OP_DBG0 -> emu_DBG0(Core); % 6810
        ?OP_DBG1 -> emu_DBG1(Core); % 6811
        ?OP_DBG2 -> emu_DBG2(Core); % 6812
        ?OP_NYI  -> emu_NYI(Core);  % 681F
        _ ->
          io:format(standard_error, "@ Invalid opcode 0x68~2.16.0B at 0x~4.16.0B\n",
                    [Opcode, uint16_dec2(get_r(Core, get_p(Core)))]),
          halt(Core, 1)
      end
  end.

emu_DBG0(Core) -> % print PC and SP
  io:format(standard_error, "@ PC 0x~4.16.0B SP 0x~4.16.0B\n",
            [uint16_dec2(get_r(Core, get_p(Core))), get_r(Core, 2)]),
  Core.

emu_DBG1(Core) -> % print PC, SP, r7, r10, r15
  io:format(standard_error, "@ PC 0x~4.16.0B SP 0x~4.16.0B R7 0x~4.16.0B R10 0x~4.16.0B R15 0x~4.16.0B\n",
            [uint16_dec2(get_r(Core, get_p(Core))), get_r(Core, 2), get_r(Core, 7), get_r(Core, 10), get_r(Core, 15)]),
  Core.

emu_DBG2(Core) -> % print PC, SP, argv[1] (*(void*)(r8+2))
  Address = uint16_inc2(get_r(Core, 8)),
  Argv1 = get_word(Core, Address),
  io:format(standard_error, "@ PC 0x~4.16.0B SP 0x~4.16.0B &argv[1] 0x~4.16.0B argv[1] 0x~4.16.0B\n",
            [uint16_dec2(get_r(Core, get_p(Core))), get_r(Core, 2), Address, Argv1]),
  Core.

%% silence "Function emu_NYI/1 has no local return"
-dialyzer({nowarn_function, emu_NYI/1}).
emu_NYI(Core) ->
  D = get_d(Core),
  P = get_p(Core),
  PC = get_r(Core, P),
  io:format(standard_error, "@ PC 0x~4.16.0B NYI 0x~2.16.0B\n", [uint16_dec2(PC), D]),
  halt(Core, 98).

interrupt(Core) ->
  X = get_x(Core),
  P = get_p(Core),
  T = (X bsl 4) bor P,
  set_ie(set_x(set_p(set_t(Core, T), 1), 2), 0).

%% Register Operations =========================================================

emu_INC(Core, N) ->
  set_r(Core, N, uint16_inc(get_r(Core, N))).

emu_DEC(Core, N) ->
  set_r(Core, N, uint16_dec(get_r(Core, N))).

emu_IRX(Core) ->
  X = get_x(Core),
  set_r(Core, X, uint16_inc(get_r(Core, X))).

emu_GLO(Core, N) ->
  set_d(Core, get_low(get_r(Core, N))).

emu_PLO(Core, N) ->
  set_r(Core, N, set_low(get_r(Core, N), get_d(Core))).

emu_GHI(Core, N) ->
  set_d(Core, get_high(get_r(Core, N))).

emu_PHI(Core, N) ->
  set_r(Core, N, set_high(get_r(Core, N), get_d(Core))).

%% Memory Reference ============================================================

emu_LDN(Core, N) ->
  true = N =/= 0, % assert
  A = get_r(Core, N),
  set_d(Core, get_byte(Core, A)).

emu_LDA(Core, N) ->
  A = get_r(Core, N),
  set_r(set_d(Core, get_byte(Core, A)), N, uint16_inc(A)).

emu_LDX(Core) ->
  A = get_r(Core, get_x(Core)),
  set_d(Core, get_byte(Core, A)).

emu_LDXA(Core) -> % Note: exactly like LDA with N=X
  X = get_x(Core),
  A = get_r(Core, X),
  set_r(set_d(Core, get_byte(Core, A)), X, uint16_inc(A)).

emu_LDI(Core) -> % Note: exactly like LDA with N=P
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(set_d(Core, get_byte(Core, A)), P, uint16_inc(A)).

emu_STR(Core, N) ->
  A = get_r(Core, N),
  set_byte(Core, A, get_d(Core)).

emu_STXD(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  set_r(set_byte(Core, A, get_d(Core)), X, uint16_dec(A)).

%% Logic Operations ============================================================

emu_OR(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  set_d(Core, get_d(Core) bor get_byte(Core, A)).

emu_ORI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(set_d(Core, get_d(Core) bor get_byte(Core, A)), P, uint16_inc(A)).

emu_XOR(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  set_d(Core, get_d(Core) bxor get_byte(Core, A)).

emu_XRI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(set_d(Core, get_d(Core) bxor get_byte(Core, A)), P, uint16_inc(A)).

emu_AND(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  set_d(Core, get_d(Core) band get_byte(Core, A)).

emu_ANI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(set_d(Core, get_d(Core) band get_byte(Core, A)), P, uint16_inc(A)).

emu_SHR(Core) ->
  D = get_d(Core),
  set_df(set_d(Core, D bsr 1), D band 1).

emu_SHRC(Core) ->
  D = get_d(Core),
  DF = get_df(Core),
  set_df(set_d(Core, (D bsr 1) bor (DF bsl 7)), D band 1).

emu_SHL(Core) ->
  D = get_d(Core),
  set_df(set_d(Core, (D bsl 1) band 16#FF), D bsr 7).

emu_SHLC(Core) ->
  D = get_d(Core),
  DF = get_df(Core),
  set_df(set_d(Core, ((D bsl 1) band 16#FF) bor DF), D bsr 7).

%% Arithmetic Operations =======================================================

emu_ADD(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  D = get_d(Core),
  {Result, Carry} = uint8_addc(get_byte(Core, A), D),
  set_df(set_d(Core, Result), Carry).

emu_ADI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  D = get_d(Core),
  {Result, Carry} = uint8_addc(get_byte(Core, A), D),
  set_r(set_df(set_d(Core, Result), Carry), P, uint16_inc(A)).

emu_ADC(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  D = get_d(Core),
  DF = get_df(Core),
  {Result, Carry} = uint8_addc(get_byte(Core, A), D, DF),
  set_df(set_d(Core, Result), Carry).

emu_ADCI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  D = get_d(Core),
  DF = get_df(Core),
  {Result, Carry} = uint8_addc(get_byte(Core, A), D, DF),
  set_r(set_df(set_d(Core, Result), Carry), P, uint16_inc(A)).

emu_SD(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  D = get_d(Core),
  {Result, Borrow} = uint8_subc(get_byte(Core, A), D),
  set_df(set_d(Core, Result), Borrow).

emu_SDI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  D = get_d(Core),
  {Result, Borrow} = uint8_subc(get_byte(Core, A), D),
  set_r(set_df(set_d(Core, Result), Borrow), P, uint16_inc(A)).

emu_SDB(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  D = get_d(Core),
  DF = get_df(Core),
  {Result, Borrow} = uint8_subc(get_byte(Core, A), D, DF),
  set_df(set_d(Core, Result), Borrow).

emu_SDBI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  D = get_d(Core),
  DF = get_df(Core),
  {Result, Borrow} = uint8_subc(get_byte(Core, A), D, DF),
  set_r(set_df(set_d(Core, Result), Borrow), P, uint16_inc(A)).

emu_SM(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  D = get_d(Core),
  {Result, Borrow} = uint8_subc(D, get_byte(Core, A)),
  set_df(set_d(Core, Result), Borrow).

emu_SMI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  D = get_d(Core),
  {Result, Borrow} = uint8_subc(D, get_byte(Core, A)),
  set_r(set_df(set_d(Core, Result), Borrow), P, uint16_inc(A)).

emu_SMB(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  D = get_d(Core),
  DF = get_df(Core),
  {Result, Borrow} = uint8_subc(D, get_byte(Core, A), DF),
  set_df(set_d(Core, Result), Borrow).

emu_SMBI(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  D = get_d(Core),
  DF = get_df(Core),
  {Result, Borrow} = uint8_subc(D, get_byte(Core, A), DF),
  set_r(set_df(set_d(Core, Result), Borrow), P, uint16_inc(A)).

%% Short-Branch Operations =====================================================

emu_BR(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(Core, P, set_low(A, get_byte(Core, A))).

emu_NBR(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(Core, P, uint16_inc(A)).

emu_BZ(Core) ->
  short_branch(Core, fun pred_D_Z/1).

emu_BNZ(Core) ->
  short_branch(Core, fun pred_D_NZ/1).

emu_BDF(Core) ->
  short_branch(Core, fun pred_DF_NZ/1).

emu_BNF(Core) ->
  short_branch(Core, fun pred_DF_Z/1).

emu_BQ(Core) ->
  short_branch(Core, fun pred_Q_NZ/1).

emu_BNQ(Core) ->
  short_branch(Core, fun pred_Q_Z/1).

emu_B1(Core) ->
  short_branch(Core, fun pred_EF1_NZ/1).

emu_BN1(Core) ->
  short_branch(Core, fun pred_EF1_Z/1).

emu_B2(Core) ->
  short_branch(Core, fun pred_EF2_NZ/1).

emu_BN2(Core) ->
  short_branch(Core, fun pred_EF2_Z/1).

emu_B3(Core) ->
  short_branch(Core, fun pred_EF3_NZ/1).

emu_BN3(Core) ->
  short_branch(Core, fun pred_EF3_Z/1).

emu_B4(Core) ->
  short_branch(Core, fun pred_EF4_NZ/1).

emu_BN4(Core) ->
  short_branch(Core, fun pred_EF4_Z/1).

short_branch(Core, Pred) ->
  P = get_p(Core),
  A = get_r(Core, P),
  NewA =
    case Pred(Core) of
      true -> set_low(A, get_byte(Core, A));
      false -> uint16_inc(A)
    end,
  set_r(Core, P, NewA).

%% Long-Branch Operations ======================================================

emu_LBR(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(Core, P, get_word(Core, A)).

emu_NLBR(Core) ->
  P = get_p(Core),
  A = get_r(Core, P),
  set_r(Core, P, uint16_inc2(A)).

emu_LBZ(Core) ->
  long_branch(Core, fun pred_D_Z/1).

emu_LBNZ(Core) ->
  long_branch(Core, fun pred_D_NZ/1).

emu_LBDF(Core) ->
  long_branch(Core, fun pred_DF_NZ/1).

emu_LBNF(Core) ->
  long_branch(Core, fun pred_DF_Z/1).

emu_LBQ(Core) ->
  long_branch(Core, fun pred_Q_NZ/1).

emu_LBNQ(Core) ->
  long_branch(Core, fun pred_Q_Z/1).

long_branch(Core, Pred) ->
  P = get_p(Core),
  A = get_r(Core, P),
  NewA =
    case Pred(Core) of
      true -> get_word(Core, A);
      false -> uint16_inc2(A)
    end,
  set_r(Core, P, NewA).

%% Skip Instructions ===========================================================

emu_LSZ(Core) ->
  long_skip(Core, fun pred_D_Z/1).

emu_LSNZ(Core) ->
  long_skip(Core, fun pred_D_NZ/1).

emu_LSDF(Core) ->
  long_skip(Core, fun pred_DF_NZ/1).

emu_LSNF(Core) ->
  long_skip(Core, fun pred_DF_Z/1).

emu_LSQ(Core) ->
  long_skip(Core, fun pred_Q_NZ/1).

emu_LSNQ(Core) ->
  long_skip(Core, fun pred_Q_Z/1).

emu_LSIE(Core) ->
  long_skip(Core, fun pred_IE_NZ/1).

long_skip(Core, Pred) ->
  case Pred(Core) of
    true ->
      P = get_p(Core),
      A = get_r(Core, P),
      set_r(Core, P, uint16_inc2(A));
    false ->
      Core
  end.

%% Control Instructions ========================================================

emu_IDL(Core) ->
  case pred_IE_NZ(Core) of
    true -> sim1802_io:wait_interrupt(), Core;
    false -> halt(Core, deadlock)
  end.

emu_NOP(Core) ->
  Core.

emu_SEP(Core, N) ->
  set_p(Core, N).

emu_SEX(Core, N) ->
  set_x(Core, N).

emu_SEQ(Core) ->
  set_q(Core, 1).

emu_REQ(Core) ->
  set_q(Core, 0).

%% Interrupt and Subroutine Handling ===========================================

emu_SAV(Core) ->
  X = get_x(Core),
  A = get_r(Core, X),
  T = get_t(Core),
  set_byte(Core, A, T).

emu_MARK(Core) ->
  X = get_x(Core),
  P = get_p(Core),
  A = get_r(Core, 2),
  T = (X bsl 4) bor P,
  set_r(set_x(set_byte(set_t(Core, T), A, T), P), 2, uint16_dec(A)).

emu_RET(Core) ->
  disret(Core, 1).

emu_DIS(Core) ->
  disret(Core, 0).

disret(Core, IE) -> % DIS (IE=0) or RET (IE=1)
  X = get_x(Core),
  A = get_r(Core, X),
  Byte = get_byte(Core, A),
  %% The documentation isn't obvious, but R(X) must be incremented _before_
  %% X is restored from the byte read from memory. Otherwise an interrupt
  %% handler executing DEC 2; SAV; ...; RET would trash the user's R(X).
  set_ie(set_x(set_p(set_r(Core, X, uint16_inc(A)), Byte band 16#0F), Byte bsr 4), IE).

%% Input/Output Byte Transfer ==================================================

emu_OUT(Core, N) ->
  X = get_x(Core),
  A = get_r(Core, X),
  Byte = get_byte(Core, A),
  io_out(Core, N, Byte),
  set_r(Core, X, uint16_inc(A)).

emu_INP(Core, N) ->
  Byte = io_inp(Core, N band 7),
  X = get_x(Core),
  A = get_r(Core, X),
  set_d(set_byte(Core, A, Byte), Byte).

%% 1804AC full-width register instructions =====================================

emu_RLXA(Core, N) ->
  X = get_x(Core),
  %% Although we could emulate it, the case when X equals N has confusing
  %% semantics and would almost certainly be a programming error.
  true = X =/= N,
  A = get_r(Core, X),
  Word = get_word(Core, A),
  Core1 = set_r(Core, N, Word),
  set_r(Core1, X, uint16_inc2(A)).

emu_RLDI(Core, N) -> % Note: exactly like RLXA with X=P
  P = get_p(Core),
  %% Although we could emulate it, the case when P equals N has confusing
  %% semantics and would almost certainly be a programming error.
  true = P =/= N,
  A = get_r(Core, P),
  Word = get_word(Core, A),
  Core1 = set_r(Core, N, Word),
  set_r(Core1, P, uint16_inc2(A)).

emu_RSXD(Core, N) ->
  X = get_x(Core),
  A = get_r(Core, X),
  Word = get_r(Core, N),
  Core1 = set_word(Core, uint16_dec(A), Word),
  set_r(Core1, X, uint16_dec2(A)).

emu_RNX(Core, N) ->
  set_r(Core, get_x(Core), get_r(Core, N)).

emu_DBNZ(Core, N) ->
  RN = uint16_dec(get_r(Core, N)),
  Core1 = set_r(Core, N, RN),
  case RN =/= 0 of
    true -> emu_LBR(Core1);
    false -> emu_NLBR(Core1)
  end.

%% 1804AC standard call and return instructions ================================

emu_SCAL(Core, N) ->
  RN = get_r(Core, N),
  X = get_x(Core),
  RX = get_r(Core, X),
  %% Push LINK.
  Core1 = set_word(Core, uint16_dec(RX), RN),
  Core2 = set_r(Core1, X, uint16_dec2(RX)),
  %% Load new PC.
  P = get_p(Core2),
  OldPC = get_r(Core2, P),
  NewPC = get_word(Core2, OldPC),
  Core3 = set_r(Core2, P, NewPC),
  %% Update LINK to old PC + 2.
  set_r(Core3, N, uint16_inc2(OldPC)).

emu_SRET(Core, N) ->
  Core1 = set_r(Core, get_p(Core), get_r(Core, N)),
  X = get_x(Core1),
  A = get_r(Core1, X),
  Word = get_word(Core1, uint16_inc(A)),
  Core2 = set_r(Core1, N, Word),
  set_r(Core2, X, uint16_inc2(A)).

%% Predicates for branch and skip instructions =================================

pred_D_NZ(Core) ->
  get_d(Core) =/= 0.

pred_D_Z(Core) ->
  get_d(Core) =:= 0.

pred_DF_NZ(Core) ->
  get_df(Core) =/= 0.

pred_DF_Z(Core) ->
  get_df(Core) =:= 0.

pred_Q_NZ(Core) ->
  get_q(Core) =/= 0.

pred_Q_Z(Core) ->
  get_q(Core) =:= 0.

pred_EF1_NZ(_Core) ->
  is_signal(?SIGNAL_EF1).

pred_EF1_Z(_Core) ->
  not is_signal(?SIGNAL_EF1).

pred_EF2_NZ(_Core) ->
  is_signal(?SIGNAL_EF2).

pred_EF2_Z(_Core) ->
  not is_signal(?SIGNAL_EF2).

pred_EF3_NZ(_Core) ->
  is_signal(?SIGNAL_EF3).

pred_EF3_Z(_Core) ->
  not is_signal(?SIGNAL_EF3).

pred_EF4_NZ(_Core) ->
  is_signal(?SIGNAL_EF4).

pred_EF4_Z(_Core) ->
  not is_signal(?SIGNAL_EF4).

pred_IE_NZ(Core) ->
  get_ie(Core) =/= 0.

%% Accessing registers =========================================================

-spec get_d(core()) -> uint8_t().
get_d(#core{d = D}) ->
  D.

set_d(Core, D) ->
  Core#core{d = D}.

-spec get_df(core()) -> uint1_t().
get_df(#core{df = DF}) ->
  DF.

set_df(Core, DF) ->
  Core#core{df = DF}.

get_ie(#core{ie = IE}) ->
  IE.

set_ie(Core, IE) ->
  Core#core{ie = IE}.

-spec get_p(core()) -> uint4_t().
get_p(#core{p = P}) ->
  P.

set_p(Core, P) ->
  Core#core{p = P}.

get_q(#core{q = Q}) ->
  Q.

set_q(Core, Q) ->
  Core#core{q = Q}.

-spec get_r(core(), uint4_t()) -> uint16_t().
get_r(#core{r = R}, N) ->
  element(N + 1, R).

set_r(Core = #core{r = R}, N, Val) ->
  Core#core{r = setelement(N + 1, R, Val)}.

get_t(#core{t = T}) ->
  T.

set_t(Core, T) ->
  Core#core{t = T}.

-spec get_x(core()) -> uint4_t().
get_x(#core{x = X}) ->
  X.

set_x(Core, X) ->
  Core#core{x = X}.

%% Signals =====================================================================

clear_signal(Signal) ->
  ets:delete(?SIGNALS_ETS, Signal),
  ok.

init_signals() ->
  ets:new(?SIGNALS_ETS, [named_table, public]),
  ok.

is_signal(Signal) ->
  ets:member(?SIGNALS_ETS, Signal).

set_signal(Signal) ->
  ets:insert(?SIGNALS_ETS, {Signal}),
  ok.

%% Memory accesses =============================================================

-spec get_byte(core(), uint16_t()) -> uint8_t().
get_byte(_Core, Address) ->
  sim1802_memory:get_byte(Address).

-spec get_word(core(), uint16_t()) -> uint16_t().
get_word(Core, Address) ->
  High = get_byte(Core, Address),
  Low = get_byte(Core, uint16_inc(Address)),
  make_word(High, Low).

set_byte(Core, Address, Byte) ->
  sim1802_memory:set_byte(Address, Byte),
  Core.

set_word(Core, Address, Word) ->
  Core1 = set_byte(Core, Address, get_high(Word)),
  set_byte(Core1, uint16_inc(Address), get_low(Word)).

%% I/O accesses ================================================================

io_inp(_Core, N) ->
  sim1802_io:inp(N).

io_out(Core, N, Byte) ->
  sim1802_io:out(Core, N, Byte).

%% Accessing bytes in words ====================================================

get_low(Word) ->
  Word band 16#00FF.

set_low(Word, Byte) ->
  (Word band 16#FF00) bor Byte.

get_high(Word) ->
  Word bsr 8.

set_high(Word, Byte) ->
  (Word band 16#00FF) bor (Byte bsl 8).

make_word(High8, Low8) ->
  (High8 bsl 8) bor Low8.

%% Arithmetic helpers ==========================================================

uint16_inc(V) ->
  (V + 1) band ?UINT16_MAX.

uint16_inc2(V) ->
  (V + 2) band ?UINT16_MAX.

uint16_dec(V) ->
  (V - 1) band ?UINT16_MAX.

uint16_dec2(V) ->
  (V - 2) band ?UINT16_MAX.

uint8_addc(M, D) ->
  uint8_addc(M, D, _DF = 0).

uint8_addc(M, D, DFIn) ->
  Result = M + D + DFIn,
  {Result band 16#FF, _DFOut = Result bsr 8}.

uint8_subc(M, D) ->
  uint8_addc(M, (bnot D) band 16#FF, 1).

uint8_subc(M, D, DF) ->
  uint8_addc(M, (bnot D) band 16#FF, DF).

%% Eunit tests =================================================================

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

%% For addition DF=1 means carry and DF=0 means no carry.
add_test() ->
  ?assertEqual({16#85, 0}, uint8_addc(16#3A, 16#4B)),
  ?assertEqual({16#2A, 1}, uint8_addc(16#3A, 16#F0)).

addc_test() ->
  ?assertEqual({16#68, 0}, uint8_addc(16#3A, 16#2D, 1)),
  ?assertEqual({16#00, 1}, uint8_addc(16#C2, 16#3D, 1)).

%% For subtraction DF=0 means borrow and DF=1 means no borrow.
sub_test() ->
  ?assertEqual({16#34, 1}, uint8_subc(16#42, 16#0E)),
  ?assertEqual({16#00, 1}, uint8_subc(16#42, 16#42)),
  ?assertEqual({16#CB, 0}, uint8_subc(16#42, 16#77)).

subc_test() ->
  ?assertEqual({16#1F, 1}, uint8_subc(16#40, 16#20, 0)),
  ?assertEqual({16#88, 0}, uint8_subc(16#4A, 16#C1, 0)),
  ?assertEqual({16#32, 1}, uint8_subc(16#64, 16#32, 1)),
  ?assertEqual({16#7F, 0}, uint8_subc(16#71, 16#F2, 1)).

-endif.
