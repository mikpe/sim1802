%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Symbol table for CDP1802 simulator

-module(sim1802_symtab).

-export([ init/1
        , resolve/2
        ]).

-export_type([ address/0
             , name/0
             , offset/0
             , symtab/0
             ]).

-type address() :: non_neg_integer().
-type name() :: string().
-type offset() :: non_neg_integer().
-type symtab() :: ets:tid().

%% API =========================================================================

-spec init([{name(), address()}]) -> symtab().
init(Syms) ->
  ETS = ets:new(undefined, [ordered_set]),
  lists:foreach(
    fun({Name, Address}) ->
      ets:insert(ETS, {Address, Name})
    end, Syms),
  ETS.

-spec resolve(symtab(), address()) -> false | {name(), offset()}.
resolve(ETS, Address) ->
  case ets:lookup(ETS, Address) of
    [{_Address, Name}] -> {Name, 0};
    [] ->
      case ets:prev(ETS, Address) of
        '$end_of_table' -> false;
        Base ->
          Name = ets:lookup_element(ETS, Base, 2),
          {Name, Address - Base}
      end
  end.
