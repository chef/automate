-module(delivery_app).

-behaviour(application).

-export([
         start/0,
         start/2,
         %% start_app_with_deps/1 and /2 are used in tests too
         start_app_with_deps/1,
         start_app_with_deps/2,
         stop/1,
         get_env/1, get_env/2
        ]).

%% application doesn't export this type
-type restart_type() :: 'permanent' | 'transient' | 'temporary'.

%% To start from command line
-spec start() -> ok.
start() ->
    start_app_with_deps(delivery, temporary).

-spec start_app_with_deps(atom() | list(atom())) -> _.
start_app_with_deps(App) ->
    start_app_with_deps(App, temporary).

-spec start_app_with_deps(App, Type) -> ok when
    App :: atom() | list(atom()),
    Type :: restart_type().
start_app_with_deps(App, Type) when is_atom(App) ->
    case application:start(App, Type) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            start_app_with_deps(Dep, Type),
            start_app_with_deps(App, Type);
        {error, {already_started, App}} ->
            ok;
        Error ->
            chef_log:error("Failed to start app ~s: ~p", [App, Error]),
            erlang:error({App, Error})
    end;
start_app_with_deps(List, Type) when erlang:is_list(List) ->
    [ start_app_with_deps(App, Type) || App <- List ],
    ok.

get_env(Key) ->
    get_env(Key, '$error').

get_env(Key, Default) ->
    case application:get_env(delivery, Key) of
        undefined ->
            case Default of
                '$error' ->
                    erlang:error({missing_app_config,
                                  {delivery, Key}});
                _ ->
                    Default
            end;
        {ok, Val} ->
            Val
    end.

start(_Type, _StartArgs) ->
    delivery_sup:start_link().

%% @private
stop(_State) ->
    ok.
