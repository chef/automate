-module(auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    % periodic metadata refresh for SAML metadata
    auth_saml_config:periodic_metadata_refresh(),

    % store seen assertions
    auth_saml_assertion_timer:start_link(),

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Start OIDC backend
    auth_oidc_backend:start(),

    {ok, { {one_for_one, 5, 10}, []} }.

