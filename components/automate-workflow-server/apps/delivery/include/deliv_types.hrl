%% @doc Centralizes all the custom types, and some app-wide constants

-include_lib("chef_common/include/chef_common.hrl").

-define(DELIV_API_VERSION, "v0").

%% @doc Currently, there is a "builder" user for each
%% enterprise. Eventually, we're probably going to track builder
%% identities in a different way, but for now they're just (internal)
%% users.
-define(BUILDER_NAME, <<"builder">>).

%% @doc the label to use when quarantining PRs.
-define(QUARANTINED_LABEL, <<"Review Required">>).

%% @doc location of the project config relative to the repository root
-define(PROJECT_CONFIG_FILE, <<".delivery/config.json">>).

%% DB-related types
-type deliv_sqerl_record() :: tuple().

%% Our records
-type d_a2_user() :: deliv_sqerl_record().
-type d_intern_user() :: deliv_sqerl_record().
-type d_user() :: deliv_sqerl_record().
-type d_enterprise() :: deliv_sqerl_record().
-type d_organization() :: deliv_sqerl_record().
-type d_team() :: deliv_sqerl_record().
-type d_project() :: deliv_sqerl_record().
-type d_pipeline() :: deliv_sqerl_record().
-type d_change() :: deliv_sqerl_record().
-type d_patchset() :: deliv_sqerl_record().
-type d_changeset() :: deliv_sqerl_record().
-type d_comment() :: deliv_sqerl_record().
-type d_stage_run() :: deliv_sqerl_record().
-type d_phase_run() :: deliv_sqerl_record().
-type d_patchset_changed_file() :: deliv_sqerl_record().
-type d_patchset_diffstat() :: deliv_sqerl_record().
-type d_patchset_commit() :: deliv_sqerl_record().
-type d_project_bitbucket_metadata() :: deliv_sqerl_record().
-type d_project_github_metadata() :: deliv_sqerl_record().
-type d_github_patchset() :: deliv_sqerl_record().
-type d_basic_auth_application() :: deliv_sqerl_record().
-type d_oauth_application() :: deliv_sqerl_record().
-type d_oauth_token() :: deliv_sqerl_record().
-type d_oauth_user_alias() :: deliv_sqerl_record().
-type d_common_scope() :: tuple().
-type d_log_object() :: deliv_sqerl_record().
-type d_saml_config() :: deliv_sqerl_record().

%% those do not map to actual records, but they make the specs for some
%% functions more informative
-type d_patchset_comment() :: d_comment().
-type d_line_comment() :: d_comment().
-type d_comment_comment() :: d_comment().

-type d_comment_status() :: draft | published.
-type d_comment_type() :: patchset | line | comment.
-type d_intrange() :: {non_neg_integer(), non_neg_integer()}.

-type d_project_mod_type() :: deliv_scm_local | deliv_scm_github | bitbucket_scm.

%% This is the internal state of the `deliv_stage' FSM
-type deliv_stage() :: tuple().

-type deliv_ttl() :: non_neg_integer().

-type deliv_scope() :: enterprise |
                       organization |
                       project |
                       pipeline.


%% @doc admin, committer, reviewer, shipper
%% There's also `observer' (at least still there at the DB level
%% but it's deprecated for now)
-type deliv_role() :: binary().
-define(ALL_DELIV_ROLES, [<<"admin">>, <<"committer">>,
                          <<"reviewer">>, <<"shipper">>]).

-type deliv_authz() :: {deliv_scope(),
                        %% `all' is a special case which means that anyone
                        %% authenticated will have access to that resource
                        [{http_method_bin(), [deliv_role()] | all}]}.

-type deliv_authz_rules() :: [{module(), deliv_authz()}].

-type deliv_hal_path() :: binary() | {t, binary()}.
-type deliv_hal_link() :: {atom(), deliv_hal_path(), [binary()]} |
                          {atom(), deliv_hal_path(), [binary()], ignore_authz}.
-type deliv_hal_mapping() :: {binary(), http_method_bin(), atom()}.
-type deliv_hal_method_mapping() :: [{http_method_bin(), [deliv_hal_link()]}].

-type deliv_hal_map() :: [{module(), deliv_hal_method_mapping()}].

%% Generic state for cowboy requests
-record(handler, {
          %% All successfully authenticated requests will have an
          %% enterprise name and user name associated with them. Since
          %% these data are required for more than just
          %% authentication, we stuff them into the handler state for
          %% ease of access (pattern-matching!), and to save ourselves
          %% a bit of work.
          user_name :: binary(),
          ent_name  :: binary(),

          %% We allow server administrators to set different timeout
          %% policy on tokens based on the action being performed
          %% (currently "read" or "write"). These are declared in the
          %% application configuration, and set in the handler state
          %% at start-up time by the supervisor.
          read_ttl  :: deliv_ttl(),
          write_ttl :: deliv_ttl(),

          authz :: deliv_authz(),
          hal_map :: deliv_hal_method_mapping(),
          hal_authz :: [{module(), deliv_authz()}],

          effective_roles = [] :: [deliv_role()],

          %% should likely remove this once the API has settled down
          stub :: string()
         }).

%% cowboy-related types
-type cowboy_req() :: cowboy_req:req().
%% this is the type of the state we pass around in cowboy callback
-type req_handler() :: #handler{}.

%% @doc RFC-2616 (HTTP/1.1) compliant header to add for repsonses when
%% token authentication fails.
-define(WWW_AUTHENTICATE, <<"ChefToken realm=\"ChefDelivery\"">>).

-type token() :: binary().

%% Encapsulate phase run summary and stage run summary information into records;
%% we do this to facilitate processing, since records are better than proplists
%% for pattern matching!
-record(phase_run_summary, {description,
                            phase,
                            phase_id,
                            phase_status,
                            search_description,
                            search_query,
                            stage,
                            stage_id,
                            stage_status}).

%% Encapsulate stage run summary information into records; we do this to
%% facilitate processing, since records are better than proplists for pattern
%% matching! These are created with a stored proc in the db. Pipeline_latest
%% and system_latest are generated as part of the query.
-record(stage_run_summary, {id :: binary(),
                            stage :: binary(),
                            status :: binary(),
                            finished :: boolean(),
                            pipeline_latest :: boolean(),
                            system_latest :: boolean()}).

-record(dependency_summary, {pipeline_id  :: non_neg_integer(),
                             dependencies :: [non_neg_integer()],
                             consumers    :: [non_neg_integer()]
                            }).

-type deliv_stage_name() :: verify | build | acceptance | union | rehearsal | delivered.
-type deliv_phase_name() :: unit | lint | syntax | quality | security | publish |
                            provision | deploy | smoke | functional.
-type delete_branch_errors() :: change_not_found | feature_branch_delete_failed |
                                feature_branch_merge_failed | pipeline_branch_missing |
                                update_patchset_failed | update_change_failed |
                                github_patchset_not_found.

%% Github json payloads - json conforming to a certain "schema"
-type github_pull_request() :: json().
-type github_issue() :: json().
-record(deliv_enterprise_default_search, {
          id :: binary(),
          ent_id :: db_id(),
          search :: binary()
         }).

-type change_action() :: delete | approve | trigger_verify | trigger_build |
                         trigger_acceptance | deliver | trigger_union |
                         trigger_rehearsal | trigger_delivered.

-type change_status() :: open | approved | delivered | superseded | blocked.

-type pipeline_id()       :: db_id().
-type pipeline_status()   :: passed | failed | unknown.
-type pipeline_statuses() :: #{ pipeline_id() => pipeline_status() }.

-record(status_metadata, {service                :: binary(),
                          status                 :: pong | fail | degraded | not_running,
                          additional_attributes=[] :: proplist()
                         }).

-define(JOB_DISPATCH_V1, <<"v1">>).
-define(JOB_DISPATCH_V2, <<"v2">>).

-record(deliv_ssh_job_criteria, {
          os :: [binary()],
          platform :: [binary()],
          platform_family :: [binary()],
          platform_version :: [binary()]
         }).

-type deliv_ssh_job_criteria() :: #deliv_ssh_job_criteria{}.

-record(deliv_change_info, {
          project :: binary(),
          id :: binary(),
          title :: binary(),
          org :: binary(),
          stage :: binary(),
          phase :: binary(),
          submitted_at :: calendar:datetime()
         }).

-type deliv_change_info() :: #deliv_change_info{}.

% Similar to what is in deliverance_types.hrl. Should maybe move that one into delivery_push_job.erl.
-record(deliv_ssh_job_state, {
          id            :: binary(),
          phase_pid     :: pid(),
          criteria      :: deliv_ssh_job_criteria(),
          command       :: binary(),
          change_info   :: deliv_change_info(),
          timeout       :: integer()
         }).
