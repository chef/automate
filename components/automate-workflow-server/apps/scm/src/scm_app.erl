%% ===================================================================
%% Bitbucket application module to start up the supervision tree.
%% ===================================================================
-module(scm_app).

-behaviour(application).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(application:start_type(), term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    scm_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
