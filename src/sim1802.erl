%%% -*- erlang-indent-level: 2 -*-
%%%
%%% CDP1802 simulator

-module(sim1802).

-export([ main/1
        , format_error/1
        ]).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main([ImageFile]) ->
  ok = sim1802_memory:init(),
  ok = load(ImageFile),
  ok = sim1802_io:init(),
  Core = sim1802_core:init(),
  run(Core).

%% Run simulator ===============================================================

run(Core) ->
  case sim1802_core:step(Core) of
    {ok, NewCore} -> run(NewCore);
    {error, {_NewCore, {halt, Status}}} -> halt(Status)
  end.

%% Load image file =============================================================

load(ImageFile) ->
  case sim1802_hex_loader:load(ImageFile) of
    ok -> ok;
    {error, Reason} ->
      io:format("Error loading ~ts: ~ts\n", [ImageFile, format_error(Reason)]),
      halt(1)
  end.

%% Format errors ===============================================================

-spec format_error(term()) -> io_lib:chars().
format_error({Module, Reason} = Error) when is_atom(Module) ->
  case erlang:function_exported(Module, format_error, 1) of
    true ->
      try Module:format_error(Reason)
      catch _:_ -> default_format_error(Error)
      end;
    false -> default_format_error(Error)
  end;
format_error(Error) -> default_format_error(Error).

default_format_error(Error) ->
  io_lib:format("~tp", [Error]).
