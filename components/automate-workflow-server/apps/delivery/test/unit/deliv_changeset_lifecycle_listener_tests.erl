%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

-module(deliv_changeset_lifecycle_listener_tests).

-include_lib("hoax/include/hoax.hrl").

-include("deliv_types.hrl").
-include("deliv_events.hrl").

-compile(export_all).

init_fixture_test_() ->
    hoax:fixture(?MODULE, "init").

handle_info_fixture_test_() ->
    hoax:fixture(?MODULE, "handle_info", setup, teardown).

integration_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "integration", setup_integration, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    {ok, Pid} = deliv_changeset_lifecycle_listener:start_link(),
    unlink(Pid),
    ok.

-spec setup() -> d_change().
setup_integration() ->
    setup(),
    eu_database:setup(),
    eu_data:with_enterprise(<<"deliv_changeset_test_enterprise">>,
      eu_data:with_organization(<<"deliv_changeset_test_organization">>,
        eu_data:with_project(<<"deliv_changeset_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                    User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_changeset_eunit_user">>),

                    FeatureBranch = <<"deliv_changeset_eunit_feature">>,
                    Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
                    Change = eu_data:change_from_patchset(Patchset),

                    Change
            end)))).

teardown(_) ->
    case whereis(deliv_changeset_lifecycle_listener) of
        undefined ->
            ok;
        Pid ->
            erlang:exit(Pid, shutdown)
    end,
    application:stop(gproc),
    eu_database:teardown(),
    error_logger:tty(true),
    ok.

init_should_listen_only_for_acceptance_stage_start_events() ->
    hoax:mock(deliv_event,
              ?expect(subscribe,
                      ?withArgs([[{{stage,started}, acceptance}]]),
                      ?andReturn(true))),

    ?assertEqual({ok, stateless},
                 deliv_changeset_lifecycle_listener:init([])),

    ?verifyAll.

handle_info_should_act_on_acceptance_stage_start_events() ->

    Payload = stage_started_for(acceptance),
    Change = Payload#stage_event.change,
    Scope = Payload#stage_event.scope,

    hoax:mock(deliv_changeset,
              ?expect(add_to_changeset,
                      ?withArgs([Change, [123]]),
                      ?andReturn({ok, changeset_whose_structure_is_to_be_determined_later}))),
    hoax:mock(deliv_proj_config,
              ?expect(get_dependency_ids,
                      ?withArgs([Scope, Change]),
                      ?andReturn({ok, [123]}))),

    deliv_event:subscribe(deliv_changeset_lifecycle_listener),
    %% TODO: ideally want to call deliv_stage:publish_stage_started/1
    %% and verify that we respond
    deliv_event:publish({{stage,started}, acceptance}, Payload),

    receive_stage_started_processed(),
    ?verifyAll.

integration_stage_started_message_should_add_change_to_changeset(Change) ->
    Payload = stage_started_for(acceptance, Change),
    Scope = Payload#stage_event.scope,

    hoax:mock(deliv_proj_config,
              ?expect(get_dependency_ids,
                      ?withArgs([Scope, Change]),
                      ?andReturn({ok, [123]}))),

    deliv_event:subscribe(deliv_changeset_lifecycle_listener),
    deliv_event:publish({{stage,started}, acceptance}, Payload),

    receive_stage_started_processed(),

    deliv_changeset_tests:assert_change_in_own_changeset(Change).

%% Helper functions
stage_started_for(StageName) ->
    stage_started_for(StageName, deliv_change:'#new'()).

stage_started_for(StageName, Change) ->
    #stage_event{
       action = started,
       stage_name = StageName,
       stage_run = ignored_for_now,
       change = Change
      }.

receive_stage_started_processed() ->
    receive
        {_, deliv_changeset_lifecycle_listener, stage_started_processed} ->
            ok;
        Other ->
            ?debugFmt("Other Message Received ~p", Other)
    end.
