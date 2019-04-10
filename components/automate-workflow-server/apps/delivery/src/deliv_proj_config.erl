%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% generic wrapper for delivery config stuff.

-module(deliv_proj_config).

-include("deliv_phase.hrl").

-export([
         project_config_file/0,
         get_version/1,
         get_build_cookbook/1,
         get_searches_for_phase/2,
         get_phases_to_skip/1,
         job_dispatch_version/1,
         validate/1,
         get_dependency_ids/2,
         get_phase_filters_from_config/2,
         get_timeout_for_phase/2,
         criteria_to_json/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-opaque(config() :: orddict:orddict()).
-export_type([config/0]).

%% TODO: Swap this for json schema?
-define(CONFIG_SPEC(BuildCookbookValidation),
        {[{<<"version">>, string},
          {<<"build_cookbook">>, BuildCookbookValidation},
          {{opt, <<"job_dispatch">>},
           {[
             {<<"version">>, {any_of, {[?JOB_DISPATCH_V1, ?JOB_DISPATCH_V2], <<"">>}}},
             {{opt, <<"filters">>},
              %% So what we really want here is:
              %%
              %% default: object_map with os, plat, etc
              %% phase_name: array_map of object_map with os, plat, etc
              %%
              %% However, I don't think such a branching spec is possible in ej, so what we have is:
              %%
              %% default | phase_name:
              %%   array_map of object_map with os, plat, etc | object_map with os, plat, etc
              %%
              %% That's the best we could do in ej.
              {object_map, {
                 {keys, {any_of, {[<<"default">> |
                                   %% Uses PHASE macro and converts to list of
                                   %% binaries instead of atoms.
                                   lists:map(fun chef_utils:to_bin/1, ?PHASES)],
                                  <<"">>}}
                 }, % end keys
                 {values,
                  {any_of, {[
                             {array_map,
                              {object_map,
                               {{keys, {any_of, {[<<"os">>,
                                                  <<"platform">>,
                                                  <<"platform_family">>,
                                                  <<"platform_version">>], <<"">>}}},
                                {values, {array_map, string}}}
                              }
                             },
                             {object_map,
                              {{keys, {any_of, {[<<"os">>,
                                                 <<"platform">>,
                                                 <<"platform_family">>,
                                                 <<"platform_version">>], <<"">>}}},
                               {values, {fun_match, {
                                           fun([_H|_T]) -> ok;
                                              (_) -> error
                                           end, array, <<"">>}}}}
                             }
                            ],<<"">>}} % end any_of
                 } % end values
                } % end object_map
              }
             }
            ]}
          },
          {{opt, <<"phase_timeouts">>},
           {object_map, {
             {keys, {any_of, {[<<"default">> |
                               %% Uses PHASE macro and converts to list of
                               %% binaries instead of atoms.
                               lists:map(fun chef_utils:to_bin/1, ?PHASES)],
                              <<"">>}}
             },
             {values, number}
          }}},
          {{opt, <<"build_nodes">>},
           {object_map, {{keys, string},
                         {values,
                          %% array_map any_of seems broken in ej:valid however
                          %% passing the empty bins seems to make it happy.
                          {array_map, {any_of, {[string,
                                                 {object_map, {{keys, {any_of, {[<<"query">>,<<"description">>],
                                                                                <<"">>}}},
                                                               {values, string}}}
                                                ], <<"">>}}}

                         }}}}]}).

-spec project_config_file() -> binary().
project_config_file() ->
    ?PROJECT_CONFIG_FILE.

-spec get_version(tuple()) -> binary() | undefined.
get_version(Config) ->
    ej:get({<<"version">>}, Config).

-spec get_build_cookbook(tuple()) -> binary() | undefined.
get_build_cookbook(Config) ->
    ej:get({<<"build_cookbook">>}, Config).

-spec validate(tuple() | binary()) -> {ok, tuple()} | {error, atom()}.
validate(Config) when is_binary(Config) ->
    case chef_json:decode(Config) of
        {error, _} ->
            {error, invalid_config};
        Json ->
            validate(Json)
    end;
validate(Config) when is_tuple(Config) ->
    %% TODO: Swap this for json schema?
    case ej:valid(?CONFIG_SPEC(build_cookbook_validation(get_version(Config))), Config) of
        ok ->
            {ok, Config};
        _ ->
            {error, invalid_config}
    end.

build_cookbook_validation(<<"1">>) ->
    string;
build_cookbook_validation(<<"2">>) ->
    {object_map, {{keys, string}, {values, string}}};
%% Defaulting to v1 is a temporary workaround until we can create a more wholistic solution
%% to handle the different config versions.
build_cookbook_validation(_) ->
    build_cookbook_validation(<<"1">>).

%% @doc Return an array of phase names that the configuration has specified
%% should be skipped. If key `skip_phases` is empty then return an empty
%% array.
-spec get_phases_to_skip(tuple()) -> [binary()].
get_phases_to_skip(Config) ->
    ej:get({<<"skip_phases">>}, Config, []).

%% @doc Return the configured timeout for the given phase name. Return
%% default if phase not set and default set, Return undefined otherwise.
-spec get_timeout_for_phase(tuple(), binary()) -> integer() | undefined.
get_timeout_for_phase(Config, Phase) ->
    case ej:get({<<"phase_timeouts">>, Phase}, Config) of
        undefined ->
            ej:get({<<"phase_timeouts">>, <<"default">>}, Config);
        Timeout ->
            Timeout
    end.

%% @doc For given project config, return its desired version of job dispatch.
%% If job_dispatch or job_dispatch.version is undefined, returns ?JOB_DISPATCH_V1.
-spec job_dispatch_version(tuple()) -> binary().
job_dispatch_version(Config) ->
    ej:get({<<"job_dispatch">>, <<"version">>}, Config, ?JOB_DISPATCH_V1).

-spec get_phase_filters_from_config(json(), binary()) -> [deliv_ssh_job_criteria()].
get_phase_filters_from_config(Config, <<"default">>) ->
    case ej:get([<<"job_dispatch">>, <<"filters">>, <<"default">>], Config) of
        undefined -> [ #deliv_ssh_job_criteria{} ];
        [Criteria|_] -> [ parse_criteria(Criteria) ];
        Criteria -> [ parse_criteria(Criteria) ]
    end;
get_phase_filters_from_config(Config, Phase) ->
    case ej:get([<<"job_dispatch">>, <<"filters">>, Phase], Config) of
        undefined -> get_phase_filters_from_config(Config, <<"default">>);
        Criteria when is_list(Criteria) -> [ parse_criteria(C) || C <- Criteria ];
        Criteria -> [ parse_criteria(Criteria) ]
    end.

parse_criteria(Criteria) ->
    #deliv_ssh_job_criteria{
       os = ej:get([<<"os">>], Criteria),
       platform = ej:get([<<"platform">>], Criteria),
       platform_family = ej:get([<<"platform_family">>], Criteria),
       platform_version = ej:get([<<"platform_version">>], Criteria)
      }.

-spec criteria_to_json(deliv_ssh_job_criteria()) -> binary().
criteria_to_json(Criteria) ->
    ListOfProps = [
        {<<"os">>, Criteria#deliv_ssh_job_criteria.os},
        {<<"platform">>, Criteria#deliv_ssh_job_criteria.platform},
        {<<"platform_family">>, Criteria#deliv_ssh_job_criteria.platform_family},
        {<<"platform_version">>, Criteria#deliv_ssh_job_criteria.platform_version}
    ],
    chef_json:encode({[ {BinaryName, Value} || {BinaryName, Value} <- ListOfProps, Value =/= undefined ]}).

-spec get_searches_for_phase(binary(), tuple()) -> [binary()].
get_searches_for_phase(Phase, Config) ->
    case ej:get({<<"build_nodes">>, Phase}, Config) of
        undefined ->
            ej:get({<<"build_nodes">>, <<"default">>}, Config, []);
        Search when is_list(Search) ->
            Search
    end.

%% @private
%% For given project config, read the dependencies and return a list of scoping names
%% for each dependency.
-spec get_dependencies(tuple(), binary(), binary()) -> [{binary(), binary(), binary(), binary()}].
get_dependencies(Config, EntName, OrgName) ->
    Deps = ej:get({<<"dependencies">>}, Config, []),
    [ format_dependency(parse_dependency(Dep), EntName, OrgName) || Dep <- Deps].

%% @private
%% Extract the Org, Proj and Pipe names from dependency string ($ORG/$PROJ:$PIPE)
parse_dependency(Dep) ->
    do_parse_dep(binary:split(Dep, <<"/">>)).

%% @private
%% Dependening on which components were provided, build a list of names that
%% can be used in format_dependency.
%% Pipe defaults to <<"master">> when unspecified.
do_parse_dep([Org, ProjPipe]) ->
    [Org | do_parse_dep([ProjPipe])];
do_parse_dep([ProjPipe]) ->
    case binary:split(ProjPipe, <<":">>) of
        [_Proj, _Pipe] = Result -> Result;
        [Proj] -> [Proj, <<"master">>]
    end.

%% @private
%% Constructs the tuple {Ent, Org, Proj, Pipe} using the current Org as a
%% default when unspecified.
format_dependency([OrgName, ProjName, PipeName], EntName, _) ->
    {EntName, OrgName, ProjName, PipeName};
format_dependency([ProjName, PipeName], EntName, OrgName) ->
    {EntName, OrgName, ProjName, PipeName}.

%% Returns a list of dependency ids.
-spec get_dependency_ids(d_common_scope(), d_change()) -> {ok, [integer()]} | {error, atom()}.
get_dependency_ids(Scope, Change) ->
    ChangeId = deliv_change:getval(id, Change),
    case get_config(ChangeId, Scope) of
        {ok, Config} ->
            [EntName, OrgName, ProjName, _PipeName]
                = deliv_scopes:'#get'(scoping_names, Scope),
            Deps = get_dependencies(Config, EntName, OrgName),
            get_dep_ids_by_scoping_names(ProjName, Deps, []);
        {error, _} = Error -> Error
    end.

%% @private
%% Get the config from the latest patchset for the given change ID.
get_config(ChangeId, Scope) ->
    case deliv_patchset:latest_patchset_for_change(ChangeId) of
        {ok, Patchset} ->
            ScmMod = deliv_scopes:'#get'(scm_module, Scope),
            ScmMod:load_config_for_patchset(Patchset, Scope);
        {error, _} = Error -> Error
    end.

%% @private
%% Returns a list of dependency ids from scoped dependency name.
%% Skips pipeline names that don't correspond to a known pipeline
get_dep_ids_by_scoping_names(_ConsumerName, [], DepIds) -> {ok, DepIds};
get_dep_ids_by_scoping_names(ConsumerName, [{EntName, OrgName, ProjName, PipeName} | RemDeps],
                             DepIds) ->
    case deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) of
        {ok, Pipeline} ->
            PipeId = deliv_pipeline:getval(id, Pipeline),
            get_dep_ids_by_scoping_names(ConsumerName, RemDeps, [PipeId | DepIds]);
        {error, not_found} ->
            chef_log:warning("module=~p, function=get_dep_ids_by_scoping_names, pipeline=[~s, ~s, ~s, ~s], dependency_for=~s event=dependency_not_found, result='Ignoring this pipeline for determining deps'",
                            [?MODULE, EntName, OrgName, ProjName, PipeName, ConsumerName]),
            get_dep_ids_by_scoping_names(ConsumerName, RemDeps, DepIds)
    end.
