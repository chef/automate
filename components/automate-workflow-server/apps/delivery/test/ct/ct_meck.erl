%% @doc A few helper funcs to wrap meck stuff
-module(ct_meck).

-export([unload/1]).

%% @doc This should be called instead of `meck:unload/1'
%% What it does is that it reloads the vanilla module(s)
%% after unloading it(them) from meck; otherwise said
%% modules are not loaded any more.
%% Of course, this shouldn't be used for `non_strict` mocked
%% modules.
%% Particularly useful when using the `sync_tests' mode.
%% As a consequence, `meck:unload/0' should _NEVER_ be used!
-spec unload(atom() | [atom()]) -> ok.
unload(Mod) when erlang:is_atom(Mod) ->
    ok = meck:unload(Mod),
    {module, Mod} = code:ensure_loaded(Mod),
    ok;
unload(Mods) when erlang:is_list(Mods) ->
    lists:foreach(fun unload/1, Mods),
    ok.
