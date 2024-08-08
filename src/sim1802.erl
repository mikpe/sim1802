%%% -*- erlang-indent-level: 2 -*-
%%%
%%% CDP1802 simulator

-module(sim1802).

-export([ main/1
        , format_error/1
        ]).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(Args) ->
  main(Args, _Trace = false).

%% TODO: use my getopt library here
main(["--trace" | Args], _Trace) -> main(Args, _Trace2 = true);
main(["-t" | Args], _Trace) -> main(Args, _Trace2 = true);
main([ImageFile | Args], Trace) ->
  ok = sim1802_memory:init(),
  ok = load(ImageFile, Args),
  ok = sim1802_io:init(),
  Core = sim1802_core:init(Trace),
  run(Core).

%% Run simulator ===============================================================

run(Core) ->
  case sim1802_core:step(Core) of
    {ok, NewCore} -> run(NewCore);
    {error, {_NewCore, {halt, Status}}} -> halt(Status)
  end.

%% Load image file and write boot args =========================================

load(ImageFile, Args) ->
  case sim1802_loader:load(ImageFile, Args) of
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
