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
  main(Args, maps:new()).

%% TODO: use my getopt library here
main(["--debug" | Args], Map) -> main(Args, maps:put(debug, true, Map));
main(["-d" | Args], Map) -> main(Args, maps:put(debug, true, Map));
main(["--trace" | Args], Map) -> main(Args, maps:put(trace, true, Map));
main(["-t" | Args], Map) -> main(Args, maps:put(trace, true, Map));
main([ImageFile | Args], Map) ->
  ok = sim1802_memory:init(),
  SymTab = load(ImageFile, Args),
  ok = sim1802_io:init(),
  Core = sim1802_core:init(maps:get(trace, Map, false), SymTab),
  sim1802_debugger:run(Core, SymTab, Map).

%% Load image file and write boot args =========================================

load(ImageFile, Args) ->
  case sim1802_loader:load(ImageFile, Args) of
    {ok, SymTab} -> SymTab;
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
