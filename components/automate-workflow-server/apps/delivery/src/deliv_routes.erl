-module(deliv_routes).

-export([
         routes/0,
         set_ttls/1,
         get_non_neg_integer/1
        ]).

-include("deliv_types.hrl").

routes() ->
    AuthzRules = deliv_authz:authz_rules(),
    HalMap = deliv_hal:hal_map(),
    State = set_ttls(#handler{}),
    [
     {"/api/_status",
      deliv_hand_status, State},

      {"/api/" ++ ?DELIV_API_VERSION ++ "/license",
       deliv_hand_license_sse, State},

     {"/api/" ++ ?DELIV_API_VERSION ++ "/canonical_enterprise",
      deliv_hand_canonical_enterprise, State},

     {"/data-collector/v0/[:exchange]", insights_hand_data_collector, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/get-token",
      deliv_hand_user_authn, State},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/users/:user_name/get-token",
      deliv_hand_user_authn, State},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/verify-token",
      deliv_hand_verify_token, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name",
      deliv_hand_enterprise,
      set_authz(deliv_hand_enterprise,
                set_hal_map(deliv_hand_enterprise,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},

     {deliv_web_utils:route_prefix() ++ ":ent_name/users",
      deliv_hand_users, set_authz(deliv_hand_users,
                                  set_hal_map(deliv_hand_users,
                                              State,
                                              HalMap,
                                              AuthzRules),
                                  AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/users/:user_name",
      deliv_hand_users_named,
      set_authz(deliv_hand_users_named,
                set_hal_map(deliv_hand_users_named,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/users/:user_name/revoke-token",
      deliv_hand_revoke_token, State},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/users/:user_name/set-oauth-alias",
      deliv_hand_user_oauth_alias,
      set_authz(deliv_hand_user_oauth_alias,
                set_hal_map(deliv_hand_users_named,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},


     {deliv_web_utils:route_prefix() ++ ":ent_name/external-users",
      deliv_hand_external_users,
      set_authz(deliv_hand_external_users,
                set_hal_map(deliv_hand_users_named,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},

     {deliv_web_utils:route_prefix() ++ ":ent_name/saml-users",
      auth_hand_saml_users,
      set_authz(auth_hand_saml_users,
                set_hal_map(deliv_hand_users_named,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},

     {deliv_web_utils:route_prefix() ++ ":ent_name/internal-users",
      deliv_hand_intern_users,
      set_authz(deliv_hand_intern_users,
                set_hal_map(deliv_hand_users_named,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/internal-users/:user_name/change-password",
      deliv_hand_intern_users_change_password,
      set_authz(deliv_hand_intern_users_change_password, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/internal-users/:user_name/reset-password",
      deliv_hand_intern_users_reset_password, State},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/queue-status",
      deliv_hand_queue_status,
      set_authz(deliv_hand_queue_status, State, AuthzRules)},

     {deliv_web_utils:route_prefix() ++ ":ent_name/pipelines",
      deliv_hand_pipelines_ent,
      set_authz(deliv_hand_pipelines_ent,
                State,
                AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/pipelines/:pipeline_id",
      deliv_hand_pipeline_ent,
      set_authz(deliv_hand_pipeline_ent,
                State,
                AuthzRules)},

     {deliv_web_utils:route_prefix() ++ ":ent_name/build-nodes",
      deliv_hand_build_nodes, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/build-node-status",
      deliv_hand_build_node_status, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/scm-providers",
      deliv_hand_scm_providers, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/teams",
      deliv_hand_teams, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/teams/:team_name",
      deliv_hand_teams_named, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/teams/:team_name/members",
      deliv_hand_teams_members, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/teams/:team_name/members/:member_name",
      deliv_hand_teams_members_named, State},

     {deliv_web_utils:route_prefix() ++ ":ent_name/orgs",
      deliv_hand_orgs, set_authz(deliv_hand_orgs,
                                 set_hal_map(deliv_hand_orgs,
                                             State,
                                             HalMap,
                                             AuthzRules),
                                 AuthzRules)},

     {deliv_web_utils:route_prefix() ++ ":ent_name/pipeline_status",
      deliv_hand_pipeline_status,
      set_authz(deliv_hand_pipeline_status, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name",
      deliv_hand_orgs_named,
      set_authz(deliv_hand_orgs_named,
                set_hal_map(deliv_hand_orgs_named,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/reviews",
      deliv_hand_org_reviews,
      set_authz(deliv_hand_org_reviews,
                State,
                AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects",
      deliv_hand_projects, set_authz(deliv_hand_projects,
                                     set_hal_map(deliv_hand_projects,
                                                 State,
                                                 HalMap,
                                                 AuthzRules),
                                     AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/:proj_name",
      deliv_hand_projects_named,
      set_authz(deliv_hand_projects_named,
                set_hal_map(deliv_hand_projects_named,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/:proj_name/stash-webhook",
      deliv_hand_stash_webhook, State},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/:proj_name/changes",
      %% TODO: Add HAL!
      deliv_hand_changes, set_authz(deliv_hand_changes,
                                    State,
                                    AuthzRules)},

      {deliv_web_utils:route_prefix()
       ++ ":ent_name/orgs/:org_name/projects/:proj_name/dependencies",
       deliv_hand_dependencies, State},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id",
      deliv_hand_change, set_authz(deliv_hand_change,
                                   State,
                                   AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id/streaming",
      deliv_hand_change_sse, set_authz(deliv_hand_change,
                                       State,
                                       AuthzRules)},

     %% FIXME: needs to be properly wired up to project stuff.
     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/pipelines",
      deliv_hand_pipelines, set_authz(deliv_hand_pipelines,
                                      set_hal_map(deliv_hand_pipelines,
                                                  State,
                                                  HalMap,
                                                  AuthzRules),
                                      AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/pipelines/:pipe_name"
      ++ "/phase_runs/:run_id",
      deliv_hand_phase_runs_named, set_authz(deliv_hand_phase_runs_named,
                                             State,
                                             AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/pipelines/:pipe_name"
      ++ "/phase_runs/:run_id/log",
      deliv_hand_phase_runs_named_log,
      set_authz(deliv_hand_phase_runs_named_log, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/pipelines/:pipe_name"
      ++ "/phase_runs/:run_id/log_objects",
      deliv_hand_phase_run_log_objects,
      set_authz(deliv_hand_phase_run_log_objects, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/:proj_name"
      ++ "/changes/:change_id/compare",
      deliv_hand_compare,
      set_authz(deliv_hand_compare,
                set_hal_map(deliv_hand_compare,
                            State,
                            HalMap,
                            AuthzRules),
                AuthzRules)
     },

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id/merge",
      deliv_hand_change_merge,
      set_authz(deliv_hand_change_merge, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id/description",
      deliv_hand_change_description,
      set_authz(deliv_hand_change_description, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id/trigger/:stage",
      deliv_hand_trigger,
      set_authz(deliv_hand_trigger, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id/accept",
      deliv_hand_change_accept,
      set_authz(deliv_hand_change_accept, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id/comments",
      deliv_hand_comments,
      set_authz(deliv_hand_comments, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/changes/:change_id/comments/:comment_id",
      deliv_hand_comments_named,
      set_authz(deliv_hand_comments_named, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/pipelines/:pipe_name",
      deliv_hand_pipes_named,
      set_authz(deliv_hand_pipes_named, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/pipelines/:pipe_name"
      ++ "/changelog/unshipped",
      deliv_hand_changelog,
      set_authz(deliv_hand_changelog, State, AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/pipe_events",
      deliv_hand_pipe_events,
      set_authz(deliv_hand_pipe_events, State, AuthzRules)},

   {deliv_web_utils:route_prefix()
    ++ ":ent_name/blocked_projects",
    deliv_hand_blocked_projects,
    set_authz(deliv_hand_blocked_projects, State, AuthzRules)},

     %% Authz Endpoints
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/authz/users/:user_name",
      deliv_hand_user_authz, set_authz(deliv_hand_user_authz,
                                       enterprise,
                                       State,
                                       AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/authz/users/:user_name",
      deliv_hand_user_authz, set_authz(deliv_hand_user_authz,
                                       organization,
                                       State,
                                       AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/:org_name/projects/"
      ++ ":proj_name/authz/users/:user_name",
      deliv_hand_user_authz, set_authz(deliv_hand_user_authz,
                                       project,
                                       State,
                                       AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++  ":ent_name/orgs/:org_name/projects/:proj_name"
      ++  "/pipelines/:pipe_name/authz/users/:user_name",
      deliv_hand_user_authz, set_authz(deliv_hand_user_authz,
                                       pipeline,
                                       State,
                                       AuthzRules)},

     {deliv_web_utils:route_prefix()
      ++ ":ent_name/searches" ,
      deliv_hand_ent_default_searches, set_authz(deliv_hand_ent_default_searches,
                                   State,
                                   AuthzRules)},


     %% Internal Endpoints (can only be hit from localhost, and
     %% not through nginx)
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

     {"/internal/ssh_authz",
      deliv_hand_ssh_authz, State},

     %% how to write stubs:

     %% create a new route for your endpoint, point the
     %% module at deliv_stub, and add a new file to
     %% delivery/priv/stubs/.  The expected filename format
     %% is: "METHOD.file_name.json". Where is the name of the
     %% HTTP method that you're stubbing in ALLCAPS.  .json
     %% isn't required, but might help with editor mode
     %% detection.

     %% this stub module is for development convenience
     %% rather than for testing. unaccepted methods will be
     %% converted into 404s, which might not be what you're
     %% expecting. All authentication is totally ignored.

     {"/stub",
      deliv_hand_stub, State#handler{stub="stub_test.json"}},

     {"/internal/post_receive_hook",
      deliv_hand_post_receive_hook, State},

     {"/[...]",
      deliv_hand_error, State}
    ].

%% TTL Processing

%% @doc Injects TTL values from application configuration into a
%% handler state record.  Handlers should access TTLs from this state
%% instead of getting them from the application configuration
%% directly.
set_ttls(HandlerState0=#handler{}) ->
    HandlerState1 = HandlerState0#handler{read_ttl = get_non_neg_integer(read_ttl_secs)},
    HandlerState1#handler{write_ttl = get_non_neg_integer(write_ttl_secs)}.

set_authz(Module, State, Rules) ->
    Authz = deliv_authz:authz_rules_for(Module, Rules),
    State#handler{authz=Authz}.

set_authz(Module, Scope, State, Rules) ->
    Authz = deliv_authz:authz_rules_for(Module, Scope, Rules),
    State#handler{authz=Authz}.

set_hal_map(Module, State, Map, AuthzRules) ->
    HalMappings = deliv_hal:hal_mapping_for(Module, Map),
    HalAuthMap = lists:flatmap(
                   fun({_HttpMethod, HalTags}) ->
                           lists:map(fun({_,_,HalModule}) ->
                                             HalModRules = deliv_authz:authz_rules_for(HalModule,
                                                                                       AuthzRules),
                                             {HalModule, HalModRules}
                                     end,
                                     HalTags)
                   end,
                   HalMappings),
    HalAuthz = lists:ukeysort(1, HalAuthMap),
    State#handler{hal_map=HalMappings,
                  hal_authz=HalAuthz}.

get_non_neg_integer(Key) ->
    case delivery_app:get_env(Key) of
        Val when erlang:is_integer(Val) andalso Val >= 0 ->
            Val;
        Bad ->
            erlang:error({invalid_app_config,
                          {delivery, Key, Bad}})
    end.
