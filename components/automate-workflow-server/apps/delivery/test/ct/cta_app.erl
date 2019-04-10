-module(cta_app).

-include_lib("delivery/include/deliv_types.hrl").

-export([ent_name/0,
         start/0]).

-define(DELIV_ACCEPTANCE_CT_APP_NAME_ENV, acceptance_ct_app_name).
-define(ENT_PREFIX, <<"delivery-acceptance-test-">>).
-define(DELIV_CT_APP_VERSION_ENV_VAR, "DELIV_CT_APP_VERSION").

%% @doc The name of the test enterprise used for acceptance level
%% CT tests
-spec ent_name() -> binary().
ent_name() ->
    case application:get_env(delivery, ?DELIV_ACCEPTANCE_CT_APP_NAME_ENV) of
        undefined ->
            Name = retrieve_ent_name(),
            ok = application:set_env(delivery,
                                     ?DELIV_ACCEPTANCE_CT_APP_NAME_ENV,
                                     Name),
            Name;
        {ok, Name} ->
            Name
    end.

%% @private
%% @doc One of two things: either this is running on a builder node
%% during an acceptance stage, and then the `DELIV_CT_APP_VERSION_ENV_VAR'
%% env var should be set, or we're running locally on a dev VM,
%% in which case we just create the enterprise locally
-spec retrieve_ent_name() -> binary().
retrieve_ent_name() ->
    case os:getenv(?DELIV_CT_APP_VERSION_ENV_VAR) of
        false ->
            app_test_helpers:start_fresh(),
            {EntName, _} = db_test_helpers:new_enterprise(?ENT_PREFIX),
            db_test_helpers:new_intern_user(EntName, <<"admin">>, <<"admin">>),
            ok = deliv_authz:assign_roles(EntName, <<"admin">>, [<<"admin">>]),
            EntName;
        AppVersion ->
            ent_name(AppVersion)
    end.

%% @private
%% @doc Of course must remain consistent with the name laid out
%% in the 'delivery-server::default' recipe
-spec ent_name(str_or_binary()) -> binary().
ent_name(AppVersion) ->
    %% we don't support special chars yet
    SafeAppVersion = re:replace(AppVersion,"[^.0-9a-zA-Z]", "_", [global]),
    SafeAppVersionBin = erlang:iolist_to_binary(SafeAppVersion),
    Prefix = ?ENT_PREFIX,
    <<Prefix/binary, SafeAppVersionBin/binary>>.

%% @doc Starts the applications needed for acceptance CT tests
-spec start() -> ok.
start() ->
    app_test_helpers:cd_to_root(),
    [ delivery_app:start_app_with_deps(Dep, temporary)
     || Dep <- required_dependencies() ],
    app_test_helpers:setup_app_env(),
    ok.

%% @private
%% @doc Deps required to run acceptance CT tests
-spec required_dependencies() -> [atom()].
required_dependencies() ->
    [ibrowse,
     %% `ssl' is needed by `ibrowse' to make HTTPS requests, but it doesn't
     %% declare it as a dependency per so, so...
     ssl].
