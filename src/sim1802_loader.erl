%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Loader for CDP1802 simulator

-module(sim1802_loader).

-export([ load/2
        ]).

%% API =========================================================================

-spec load(string(), [string()]) -> {ok, sim1802_symtab:symtab()} | false | {error, {module(), term()}}.
load(File, Args) ->
  case (case sim1802_elf_loader:load(File) of
          false ->
            case sim1802_hex_loader:load(File) of
              ok -> {ok, sim1802_symtab:init([])};
              {error, _Reason2} = Error2 -> Error2
            end;
          OkOrError -> OkOrError
        end) of
    {ok, _SymTab} = Result ->
      write_boot_args([File | Args]),
      Result;
    {error, _Reason} = Error -> Error
  end.

%% Write Boot Args =============================================================
%%
%% Set up the top of RAM to contain:
%%
%% -2:   ENVP
%% -4:   ARGV
%% -6:   ARGC
%% -8:   0x1802 magic cookie
%% ..    environment strings
%% ENVP: NULL-terminated array of pointers to environment strings
%% ..    argument strings
%% ARGV: NULL-terminated array of pointers to argument strings
%% ..    available as stack

write_boot_args(Args) ->
  Env = simenv(),
  Anchor = (1 bsl 16) - 8,
  Envp = push_strings(Env, Anchor),
  Argv = push_strings(Args, Envp),
  Argc = length(Args),
  write_word(Anchor + 0, 16#1802),
  write_word(Anchor + 2, Argc),
  write_word(Anchor + 4, Argv),
  write_word(Anchor + 6, Envp).

push_strings(Strings, SP0) ->
  {PointersRev, SP1} = lists:foldl(fun push_string/2, {[], SP0}, Strings),
  lists:foldl(fun push_pointer/2, SP1, [0 | PointersRev]).

push_string(String, {Pointers, SP0}) ->
  SP = SP0 - (length(String) + 1),
  write_string(SP, String),
  {[SP | Pointers], SP}.

push_pointer(Pointer, SP0) ->
  SP = SP0 - 2,
  write_word(SP, Pointer),
  SP.

simenv() ->
  lists:filtermap(fun filter_env/1, os:getenv()).

filter_env("HOME=" ++ _) -> true;
filter_env("HOSTNAME=" ++ _) -> true;
filter_env("HOSTTYPE=" ++ _) -> {true, "HOSTTYPE=cdp1802-none"};
filter_env("MACHTYPE=" ++ _) -> {true, "MACHTYPE=cdp1802"};
filter_env("OSTYPE=" ++ _) -> {true, "OSTYPE=none"};
filter_env("PATH=" ++ _) -> {true, "PATH=/usr/bin"};
filter_env("USER=" ++ _) -> true;
filter_env(_) -> false.

write_word(Address, Word) ->
  write_byte(Address, Word bsr 8),
  write_byte(Address + 1, Word band 255).

write_string(Address, []) ->
  write_byte(Address, $\0);
write_string(Address, [Byte | Bytes]) ->
  write_byte(Address, Byte),
  write_string(Address + 1, Bytes).

write_byte(Address, Byte) ->
  sim1802_memory:set_byte(Address, Byte).
