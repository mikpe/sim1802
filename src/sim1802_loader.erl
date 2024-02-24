%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Intel HEX file loader for CDP1802 simulator

-module(sim1802_loader).

-export([ load/1
        ]).

%% API =========================================================================

-spec load(string()) -> ok | {error, any()}.
load(File) ->
  case file:open(File, [read, raw, read_ahead]) of
    {ok, Fd} ->
      try
        process(Fd, File, _LineNr = 1)
      after
        file:close(Fd)
      end;
    {error, Reason} = Error ->
      io:format(standard_error, "~ts: Error: ~ts\n", [File, file:format_error(Reason)]),
      Error
  end.

%% Internal ====================================================================
%%
%% The HEX file format uses the term "record" for the data encoded in a line.
%% To avoid confusion with Erlang records, we use the term "entry" instead.
%%
%% The input consists of entries
%%      ":"<Count:1><Address:2><Type:1><Data:Count><Checksum:1>
%% where each <Field:N> consists of 2N upper-case hexadecimal characters denoting
%% N bytes. The sum mod 256 of the Count, Address, Type, Data, and Checksum bytes
%% must be zero. Data before the first entry, between entries, or after the last
%% entry, is ignored. There can be no text line terminators within entries.
%%
%% The original Intel HEX format only considers type 16#01 entries as EOF markers,
%% as an extension we also accept zero-length type 16#00 entries as EOF markers.

process(Fd, File, LineNr) ->
  case read_char(Fd) of
    {ok, $:} -> % start of an entry
      case read_entry(Fd) of
        {ok, Entry} ->
          case load_entry(Entry) of
            ok -> process(Fd, File, LineNr);
            eof -> ok;
            {error, Reason} -> process_error(File, LineNr, Reason)
          end;
        {error, Reason} -> process_error(File, LineNr, Reason)
      end;
    {ok, $\n} -> process(Fd, File, LineNr + 1);
    {ok, _Ch} -> process(Fd, File, LineNr);
    eof -> ok;
    {error, Reason} -> process_error(File, LineNr, Reason)
  end.

process_error(File, LineNr, Reason) ->
  io:format(standard_error, "~ts, line ~p: Error: ~ts\n", [File, LineNr, format_error(Reason)]),
  {error, Reason}.

%% seen ":", now read <count:1><address:2><type:1><data:count><checksum:1>
read_entry(Fd) ->
  case read_byte(Fd) of
    {ok, Count} ->
      case read_address(Fd) of
        {ok, {AddrHigh, AddrLow}} ->
          case read_byte(Fd) of
            {ok, Type} ->
              case read_data(Fd, Count, []) of
                {ok, Data} ->
                  case read_byte(Fd) of
                    {ok, Checksum} -> check_entry(Count, AddrHigh, AddrLow, Type, Data, Checksum);
                    {error, _Reason} = Error -> Error
                  end;
                {error, _Reason} = Error -> Error
              end;
            {error, _Reason} = Error -> Error
          end;
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

check_entry(Count, AddrHigh, AddrLow, Type, Data, Checksum) ->
  Sum = lists:sum([Count, AddrHigh, AddrLow, Type | Data]),
  Actual = ((bnot Sum) + 1) band 255,
  case Actual =:= Checksum of
    true -> {ok, {Count, (AddrHigh bsl 8) bor AddrLow, Type, Data}};
    false -> {error, {invalid_checksum, Actual, Checksum}}
  end.

read_data(_Fd, 0, Acc) -> {ok, lists:reverse(Acc)};
read_data(Fd, N, Acc) ->
  case read_byte(Fd) of
    {ok, Byte} -> read_data(Fd, N - 1, [Byte | Acc]);
    {error, _Reason} = Error -> Error
  end.

read_address(Fd) ->
  case read_byte(Fd) of
    {ok, High} ->
      case read_byte(Fd) of
        {ok, Low} -> {ok, {High, Low}}; % don't combine yet, still need to checksum
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

read_byte(Fd) ->
  case read_hex_digit(Fd) of
    {ok, High} ->
      case read_hex_digit(Fd) of
        {ok, Low} -> {ok, (High bsl 4) bor Low};
        {error, _Reason} = Error -> Error
      end;
    {error, _Reason} = Error -> Error
  end.

read_hex_digit(Fd) ->
  case read_char(Fd) of
    {ok, Ch} ->
      if Ch >= $A, Ch =< $F -> {ok, Ch - ($A - 10)};
         Ch >= $0, Ch =< $9 -> {ok, Ch - $0};
         true -> {error, {invalid_hex_digit, Ch}}
      end;
    eof -> {error, premature_eof};
    {error, _Reason} = Error -> Error
  end.

read_char(Fd) ->
  case file:read(Fd, 1) of
    {ok, [Ch]} -> {ok, Ch};
    eof -> eof;
    {error, Reason} -> {error, {file, Reason}}
  end.

%% Loading an entry ============================================================

load_entry({Count, Address, Type, Data}) ->
  case Type of
    16#00 when Count > 0 -> % Data
      lists:foldl(fun load_byte/2, Address, Data),
      ok;
    16#00 when Count =:= 0 -> % common alternative End Of File encoding
      eof;
    16#01 -> % End Of File
      eof;
    _ ->
      {error, {invalid_entry_type, Type}}
  end.

load_byte(Byte, Address) ->
  sim1802_memory:set_byte(Address, Byte),
  Address + 1.

%% Error formatting ============================================================

format_error(Reason) ->
  case Reason of
    {file, FileReason} ->
      file:format_error(FileReason);
    {invalid_entry_type, Type} ->
      io_lib:format("invalid entry type 0x~2.16.0B", [Type]);
    {invalid_checksum, Actual, Expected} ->
      io_lib:format("invalid checksum 0x~2.16.0B differs from 0x~2.16.0B", [Actual, Expected]);
    {invalid_hex_digit, Ch} ->
      io_lib:format("invalid hex digit '~c'", [Ch]);
    premature_eof ->
      "premature EOF"
  end.
