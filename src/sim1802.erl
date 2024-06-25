%%% -*- erlang-indent-level: 2 -*-
%%%
%%% CDP1802 simulator

-module(sim1802).

-export([ main/1
        ]).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main([ImageFile]) ->
  ok = sim1802_memory:init(),
  ok = sim1802_hex_loader:load(ImageFile),
  ok = sim1802_io:init(),
  Core = sim1802_core:init(),
  run(Core).

%% Run simulator ===============================================================

run(Core) ->
  case sim1802_core:step(Core) of
    {ok, NewCore} -> run(NewCore);
    {error, {_NewCore, {halt, Status}}} -> halt(Status)
  end.
