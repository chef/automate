%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(deliv_proj_config_tests).

-include_lib("deliv_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

deliv_proj_config_test_() ->
    [
     validate(),
     get_version(),
     get_build_cookbook_v1(),
     get_build_cookbook_unspecified(),
     get_build_cookbook_v2(),
     get_phases_to_skip(),
     get_timeout_for_phase(),
     job_dispatch_version(),
     job_dispatch_v2_filter(),
     get_phase_filters_from_config(),
     criteria_to_json(),
     get_dependencies_returns_empty_list_if_no_dependencies(),
     hoax:fixture(?MODULE, "get_dependency_ids"),
     hoax:fixture(?MODULE, "start_phase")
    ].

validate() ->
    [{"can load minimal valid config.",
      fun() ->
              MinimalJson = make_minimal_valid_json(),
              Cfg = deliv_proj_config:validate(make_config(MinimalJson)),
              ?assertMatch({ok, _Config}, Cfg)
      end},
     {"can load a full valid config.",
      fun() ->
              FullJson = make_full_valid_json(),
              Cfg = deliv_proj_config:validate(make_config(FullJson)),
              ?assertMatch({ok, _Config}, Cfg)
      end},
     {"can load a full valid config with search as object.",
      fun() ->
              FullJson = make_full_valid_json(),
              NVJson = ej:set({<<"build_nodes">>}, FullJson,
                              {[
                                {<<"default">>, [{[{<<"query">>,<<"name:*">>},{<<"description">>,<<"Case1">>}]}]}
                               ]}),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({ok, _Config}, Cfg)
      end},
     {"can load a full valid config with dependencies.",
     fun() ->
              FullJson = make_full_valid_json(2),
              NVJson = ej:set({<<"dependencies">>}, FullJson,
                              {[
                                <<"project1">>, <<"project2:not_master">>
                               ]}),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({ok, _Config}, Cfg)
     end},
     {"can load a full valid config with phase_timeouts.",
     fun() ->
              FullJson = make_full_valid_json(2),
              NVJson = ej:set({<<"phase_timeouts">>}, FullJson,
                              {[
                                <<"default">>, 5000,
                                <<"unit">>, 4000
                               ]}),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({ok, _Config}, Cfg)
     end},
     {"error if invalid json.",
      fun() ->
              InvalidJson = make_invalid_json(),
              Cfg = deliv_proj_config:validate(InvalidJson),
              ?assertMatch({error, _Config}, Cfg)
      end},
     {"error if no version.",
      fun() ->
              FullJson = make_full_valid_json(),
              NVJson = ej:delete({<<"version">>}, FullJson),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({error, _Config}, Cfg)
      end},
     {"error if no build cookbook.",
      fun() ->
              FullJson = make_full_valid_json(),
              NVJson = ej:delete({<<"build_cookbook">>}, FullJson),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({error, _Config}, Cfg)
      end},
     {"error if version is not string.",
      fun() ->
              FullJson = make_full_valid_json(),
              NVJson = ej:set({<<"version">>}, FullJson, 1),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({error, _Config}, Cfg)
      end},
     {"error if build cookbook is not string.",
      fun() ->
              FullJson = make_full_valid_json(),
              NVJson = ej:set({<<"build_cookbook">>}, FullJson, 1),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({error, _Config}, Cfg)
      end},
     {"error if phase_timeouts is not {key, number}.",
      fun() ->
           FullJson = make_full_valid_json(),
           NVJson = ej:set({<<"phase_timeouts">>}, FullJson,
                           {[
                             {<<"default">>, <<"5000">>}
                            ]}),
           Cfg = deliv_proj_config:validate(make_config(NVJson)),
           ?assertMatch({error, _Config}, Cfg)
      end},
     {"error if build nodes is not {key, [string]} or {key, [{'query':string, 'description':string}]}.",
      fun() ->
              FullJson = make_full_valid_json(),
              NVJson = ej:set({<<"build_nodes">>}, FullJson,
                              {[
                                {<<"default">>, [1]}
                               ]}),
              Cfg = deliv_proj_config:validate(make_config(NVJson)),
              ?assertMatch({error, _Config}, Cfg)
      end},
      {"error if search object keys are not 'query' or 'description'",
       fun() ->
               FullJson = make_full_valid_json(),
               NVJson = ej:set({<<"build_nodes">>}, FullJson,
                               {[
                                 {<<"default">>, [{[{<<"not_query">>,<<"name:*">>},{<<"not_description">>,<<"Case1">>}]}]}
                                ]}),
               Cfg = deliv_proj_config:validate(make_config(NVJson)),
               ?assertMatch({error, _Config}, Cfg)
       end}].

get_version() ->
    [{"can get version from valid config.",
      fun() ->
              FullJson = make_full_valid_json(),
              {ok, Cfg} = deliv_proj_config:validate(make_config(FullJson)),
              ?assertEqual(<<"1">>, deliv_proj_config:get_version(Cfg))
      end}].

%% any versions other than 1 or 2 are "unmanaged" and should default
%% to version 1. This is a temporary workaround until a more wholistic
%% solution can be put in place.
get_build_cookbook_unspecified() ->
    [{"defaults to version 1 if version is currently unmanaged.",
      fun() ->
              FullJson = make_full_valid_json(3),
              {ok, Cfg} = deliv_proj_config:validate(make_config(FullJson)),
              ?assertEqual(<<"simple_build">>,
                           deliv_proj_config:get_build_cookbook(Cfg))
      end}].


get_build_cookbook_v1() ->
    [{"can get build cookbook from valid version 1 config.",
      fun() ->
              FullJson = make_full_valid_json(1),
              {ok, Cfg} = deliv_proj_config:validate(make_config(FullJson)),
              ?assertEqual(<<"simple_build">>,
                           deliv_proj_config:get_build_cookbook(Cfg))
      end}].

get_build_cookbook_v2() ->
    [{"can get build cookbook from valid version 2 config.",
      fun() ->
              FullJson = make_full_valid_json(2),
              {ok, Cfg} = deliv_proj_config:validate(make_config(FullJson)),
              ?assertEqual({[
                             {<<"name">>, <<"simple_build">>},
                             {<<"path">>, <<"/tmp">>}
                            ]},
                           deliv_proj_config:get_build_cookbook(Cfg))
      end}].

get_phases_to_skip() ->
    [{"can get phases to skip from valid config.",
      fun() ->
              FullJson = make_full_valid_json(),
              {ok, Cfg} = deliv_proj_config:validate(make_config(FullJson)),
              ?assertEqual([<<"lint">>,<<"syntax">>],
                           deliv_proj_config:get_phases_to_skip(Cfg))
      end}].

get_timeout_for_phase() ->
    [{"returns undefined if phase_timeouts not present",
      fun() ->
              FullJson = make_full_valid_json(),
              {ok, Cfg} = deliv_proj_config:validate(make_config(FullJson)),
              ?assertEqual(undefined,
                           deliv_proj_config:get_timeout_for_phase(Cfg, <<"unit">>))
      end},
     {"returns undefined if phase_timeouts does not conatain default or valid phase",
      fun() ->
           FullJson = make_full_valid_json(),
           NVJson = ej:set({<<"phase_timeouts">>}, FullJson,
                           {[]}),
           {ok, Cfg} = deliv_proj_config:validate(make_config(NVJson)),
           ?assertEqual(undefined,
                        deliv_proj_config:get_timeout_for_phase(Cfg, <<"unit">>))
      end},
     {"returns timeout if phase_timeouts conatains default and valid phase is passed",
      fun() ->
        FullJson = make_full_valid_json(),
        NVJson = ej:set({<<"phase_timeouts">>}, FullJson,
                        {[
                          {<<"default">>, 3600}
                        ]}),
        {ok, Cfg} = deliv_proj_config:validate(make_config(NVJson)),
        ValidPhases = [<<"unit">>, <<"lint">>, <<"syntax">>,
                       <<"security">>, <<"quality">>,
                       <<"provision">>, <<"deploy">>, <<"smoke">>, <<"functional">>],
        [?assertEqual(3600, deliv_proj_config:get_timeout_for_phase(Cfg, Phase))
         || Phase <- ValidPhases]
      end},
     {"returns timeout if phase_timeouts conatains default and valid phase is passed",
      fun() ->
        FullJson = make_full_valid_json(),
        NVJson = ej:set({<<"phase_timeouts">>}, FullJson,
                        {[
                          {<<"default">>, 600},
                          {<<"unit">>, 3600},
                          {<<"lint">>, 3601},
                          {<<"syntax">>, 3602},
                          {<<"security">>, 3603},
                          {<<"quality">>, 3604},
                          {<<"provision">>, 3605},
                          {<<"deploy">>, 3606},
                          {<<"smoke">>, 3607},
                          {<<"functional">>, 3608}
                        ]}),
        {ok, Cfg} = deliv_proj_config:validate(make_config(NVJson)),
        ValidPhases = [{3600, <<"unit">>}, {3601, <<"lint">>}, {3602, <<"syntax">>},
                       {3603, <<"security">>}, {3604, <<"quality">>},
                       {3605, <<"provision">>}, {3606, <<"deploy">>},
                       {3607, <<"smoke">>}, {3608, <<"functional">>}],
        [?assertEqual(Timeout, deliv_proj_config:get_timeout_for_phase(Cfg, Phase))
         || {Timeout, Phase} <- ValidPhases]
      end},
     {"returns undefined if phase not valid",
      fun() ->
              FullJson = make_full_valid_json(),
              {ok, Cfg} = deliv_proj_config:validate(make_config(FullJson)),
              ?assertEqual(undefined,
                           deliv_proj_config:get_timeout_for_phase(Cfg, <<"invalid">>))
      end}
      ].

job_dispatch_version() ->
    [{"can load version v1 if job_dispatch key is set.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[{<<"version">>, ?JOB_DISPATCH_V1}]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
     end},
     {"can load version v2 if job_dispatch key is set.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[{<<"version">>, ?JOB_DISPATCH_V2}]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
     end},
     {"returns v1 if no key exists for 'job_dispatch', 'version'.",
      fun() ->
          FullJson = make_full_valid_json(),
          {ok, Config} = deliv_proj_config:validate(make_config(FullJson)),
          ?assertEqual(?JOB_DISPATCH_V1, deliv_proj_config:job_dispatch_version(Config))
      end},
     {"error if job_dispatch key passed but no version subkey is set.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson, {[{}]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({error, _Config}, Cfg)
     end},
     {"error if job_dispatch key passed but invalid version is set.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[{<<"version">>, <<"not_a_version">>}]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({error, _Config}, Cfg)
     end}].

job_dispatch_v2_filter() ->
    [{"error if filters is not a hash of values.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>, <<"wrong">>}
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({error, _Config}, Cfg)
      end},
     {"error if filter recieves keys that are invalid.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"some_key">>, [<<"some_value">>]}
                              ]}
                            }
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({error, _Config}, Cfg)
      end},
     {"can load if a default filter is specified",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"default">>,
                                {[
                                  {<<"os">>, [<<"some_value">>]}
                                 ]}
                               }
                              ]}
                            }
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
      end},
     {"error if filters passes a correct key with a string value.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"default">>,
                                {[
                                  {<<"os">>, <<"some_value">>}
                                 ]}
                               }
                              ]}
                            }
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({error, _Config}, Cfg)
      end},
      {"error if filters passes a correct key with an empty array value.",
       fun() ->
           FullJson = make_full_valid_json(),
           NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                           {[
                             {<<"version">>, ?JOB_DISPATCH_V2},
                             {<<"filters">>,
                              {[
                                {<<"default">>,
                                 {[
                                   {<<"os">>, []}
                                  ]}
                                }
                               ]}
                             }
                            ]}),
           Cfg = deliv_proj_config:validate(make_config(NVJson)),
           ?assertMatch({error, _Config}, Cfg)
       end},
     {"can load if filters passes a correct keys with an arrays of strings.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"default">>,
                                {[
                                  {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                  {<<"platform">>, [<<"some_value">>]},
                                  {<<"platform_family">>, [<<"some_value">>]},
                                  {<<"platform_version">>, [<<"some_value">>]}
                                 ]}
                               }
                              ]}
                            }
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
      end},
      {"can load if filters paseses the default key an array of valid filters.",
       fun() ->
           FullJson = make_full_valid_json(),
           NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                           {[
                             {<<"version">>, ?JOB_DISPATCH_V2},
                             {<<"filters">>,
                              {[
                                {<<"default">>,
                                 [
                                  {[
                                    {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                    {<<"platform">>, [<<"some_value">>]},
                                    {<<"platform_family">>, [<<"some_value">>]},
                                    {<<"platform_version">>, [<<"some_value">>]}
                                   ]}
                                 ]
                                }
                               ]}
                             }
                            ]}),
           Cfg = deliv_proj_config:validate(make_config(NVJson)),
           ?assertMatch({ok, _Config}, Cfg)
       end},
       {"can load if filters passes a phase key with a valid filters.",
        fun() ->
            FullJson = make_full_valid_json(),
            NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                            {[
                              {<<"version">>, ?JOB_DISPATCH_V2},
                              {<<"filters">>,
                               {[
                                 {<<"unit">>,
                                  {[
                                    {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                    {<<"platform">>, [<<"some_value">>]},
                                    {<<"platform_family">>, [<<"some_value">>]},
                                    {<<"platform_version">>, [<<"some_value">>]}
                                   ]}
                                 }
                                ]}
                              }
                             ]}),
            Cfg = deliv_proj_config:validate(make_config(NVJson)),
            ?assertMatch({ok, _Config}, Cfg)
        end},
     {"can load if filters passes a phase key with an array of valid filters.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"unit">>,
                                [
                                 {[
                                   {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                   {<<"platform">>, [<<"some_value">>]},
                                   {<<"platform_family">>, [<<"some_value">>]},
                                   {<<"platform_version">>, [<<"some_value">>]}
                                  ]}
                                ]
                               }
                              ]}
                            }
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
      end},
     {"can load if filters passes a phase key with an array of multiple valid filters json blobs.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"unit">>,
                                [
                                 {[
                                   {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                   {<<"platform">>, [<<"some_value">>]},
                                   {<<"platform_family">>, [<<"some_value">>]},
                                   {<<"platform_version">>, [<<"some_value">>]}
                                  ]},
                                 {[
                                   {<<"os">>, [<<"other_value">>, <<"other_value">>]},
                                   {<<"platform">>, [<<"other_value">>]},
                                   {<<"platform_family">>, [<<"other_value">>]},
                                   {<<"platform_version">>, [<<"other_value">>]}
                                  ]}
                                ]
                               }
                              ]}
                            }
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
      end},
     {"can load if filters passed default and multiple phase keys.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"default">>,
                                {[
                                  {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                  {<<"platform">>, [<<"some_value">>]},
                                  {<<"platform_family">>, [<<"some_value">>]},
                                  {<<"platform_version">>, [<<"some_value">>]}
                                 ]}
                               },
                               {<<"unit">>,
                                [
                                 {[
                                   {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                   {<<"platform">>, [<<"some_value">>]},
                                   {<<"platform_family">>, [<<"some_value">>]},
                                   {<<"platform_version">>, [<<"some_value">>]}
                                  ]},
                                 {[
                                   {<<"os">>, [<<"other_value">>, <<"other_value">>]},
                                   {<<"platform">>, [<<"other_value">>]},
                                   {<<"platform_family">>, [<<"other_value">>]},
                                   {<<"platform_version">>, [<<"other_value">>]}
                                  ]}
                                ]
                               },
                               {<<"syntax">>,
                                [
                                 {[
                                   {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                                   {<<"platform">>, [<<"some_value">>]},
                                   {<<"platform_family">>, [<<"some_value">>]},
                                   {<<"platform_version">>, [<<"some_value">>]}
                                  ]},
                                 {[
                                   {<<"os">>, [<<"other_value">>, <<"other_value">>]},
                                   {<<"platform">>, [<<"other_value">>]},
                                   {<<"platform_family">>, [<<"other_value">>]},
                                   {<<"platform_version">>, [<<"other_value">>]}
                                  ]}
                                ]
                               }
                              ]}
                            }
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
      end},
     {"can load if no filters but has a version.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2}
                           ]}),
          Cfg = deliv_proj_config:validate(make_config(NVJson)),
          ?assertMatch({ok, _Config}, Cfg)
     end}
    ].

get_phase_filters_from_config() ->
    [{"if no default filter is passed, then an empty #deliv_ssh_job_criteria{} is returned.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[{
                                                  <<"version">>, ?JOB_DISPATCH_V1
                                                 }]}),
          {ok, Config} = deliv_proj_config:validate(NVJson),
          ?assertEqual([#deliv_ssh_job_criteria{}], deliv_proj_config:get_phase_filters_from_config(Config, <<"some_phase">>))
     end},
     {"if default filter is passed the config, pass back the default filter info.",
      fun() ->
          FullJson = make_full_valid_json(),
          DefaultCriteria = #deliv_ssh_job_criteria{
            os = [<<"os_value">>, <<"os_value2">>],
            platform = [<<"platform_value">>, <<"platform_value2">>],
            platform_family = [<<"platform_family_value">>, <<"platform_family_value2">>],
            platform_version = [<<"platform_version_value">>, <<"platform_version_value2">>]
          },
          DefaultFilterInfo =  {[
                                 {<<"os">>, DefaultCriteria#deliv_ssh_job_criteria.os},
                                 {<<"platform">>, DefaultCriteria#deliv_ssh_job_criteria.platform},
                                 {<<"platform_family">>, DefaultCriteria#deliv_ssh_job_criteria.platform_family},
                                 {<<"platform_version">>, DefaultCriteria#deliv_ssh_job_criteria.platform_version}
                                ]},
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"default">>, DefaultFilterInfo}
                              ]}
                            }
                           ]}),
          {ok, Config} = deliv_proj_config:validate(NVJson),
          ?assertEqual([DefaultCriteria],
                       deliv_proj_config:get_phase_filters_from_config(Config, <<"some_phase">>))
     end},
     %% Ideally this would be an error scenario, but we cannot figure out how to get our EJ parsing to
     %% handle this correctly
     {"if default filter is passed an array of config, pass back the first item",
      fun() ->
          FullJson = make_full_valid_json(),
          DefaultCriteria = #deliv_ssh_job_criteria{
                               os = [<<"os_value">>, <<"os_value2">>],
                               platform = [<<"platform_value">>, <<"platform_value2">>],
                               platform_family = [<<"platform_family_value">>, <<"platform_family_value2">>],
                               platform_version = [<<"platform_version_value">>, <<"platform_version_value2">>]
                              },
          DefaultFilterInfo =  [
                                {[
                                  {<<"os">>, DefaultCriteria#deliv_ssh_job_criteria.os},
                                  {<<"platform">>, DefaultCriteria#deliv_ssh_job_criteria.platform},
                                  {<<"platform_family">>, DefaultCriteria#deliv_ssh_job_criteria.platform_family},
                                  {<<"platform_version">>, DefaultCriteria#deliv_ssh_job_criteria.platform_version}
                                 ]},
                                {[
                                  {<<"os">>, [<<"other_value">>]},
                                  {<<"platform">>, [<<"other_value">>]},
                                  {<<"platform_family">>, [<<"other_value">>]},
                                  {<<"platform_version">>, [<<"other_value">>]}
                                 ]}
                               ],
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                          {[
                            {<<"version">>, ?JOB_DISPATCH_V2},
                            {<<"filters">>,
                             {[
                               {<<"default">>, DefaultFilterInfo}
                              ]}
                            }
                           ]}),
          {ok, Config} = deliv_proj_config:validate(NVJson),
          ?assertEqual([DefaultCriteria],
                       deliv_proj_config:get_phase_filters_from_config(Config, <<"some_phase">>))
     end},
     {"if no filter for current phase or default, pass back empty criteria.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                  {[
                    {<<"version">>, ?JOB_DISPATCH_V2},
                    {<<"filters">>,
                     {[
                       {<<"unit">>,
                        {[
                          {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                          {<<"platform">>, [<<"some_value">>]},
                          {<<"platform_family">>, [<<"some_value">>]},
                          {<<"platform_version">>, [<<"some_value">>]}
                         ]}
                       }
                      ]}
                    }
                   ]}),
          {ok, Config} = deliv_proj_config:validate(NVJson),
          ?assertEqual([#deliv_ssh_job_criteria{}], deliv_proj_config:get_phase_filters_from_config(Config, <<"not_unit">>))
      end},
     {"if no filter for current phase, but default filter defined, pass back default criteria.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                  {[
                    {<<"version">>, ?JOB_DISPATCH_V2},
                    {<<"filters">>,
                     {[
                       {<<"default">>,
                        {[
                          {<<"os">>, [<<"default_value">>, <<"default_value">>]},
                          {<<"platform">>, [<<"default_value">>]},
                          {<<"platform_family">>, [<<"default_value">>]},
                          {<<"platform_version">>, [<<"default_value">>]}
                         ]}
                       },
                       {<<"unit">>,
                        {[
                          {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                          {<<"platform">>, [<<"some_value">>]},
                          {<<"platform_family">>, [<<"some_value">>]},
                          {<<"platform_version">>, [<<"some_value">>]}
                         ]}
                       }
                      ]}
                    }
                   ]}),
              JobCriteriaExpected = #deliv_ssh_job_criteria{
                                       os = [<<"default_value">>, <<"default_value">>],
                                       platform = [<<"default_value">>],
                                       platform_family = [<<"default_value">>],
                                       platform_version = [<<"default_value">>]
                                      },
          {ok, Config} = deliv_proj_config:validate(NVJson),
          ?assertEqual([JobCriteriaExpected], deliv_proj_config:get_phase_filters_from_config(Config, <<"not_unit">>))
      end},
     {"if filter exists for phase, then proper record is returned.",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                  {[
                    {<<"version">>, ?JOB_DISPATCH_V2},
                    {<<"filters">>,
                     {[
                       {<<"unit">>,
                        {[
                          {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                          {<<"platform">>, [<<"some_value">>]},
                          {<<"platform_family">>, [<<"some_value">>]},
                          {<<"platform_version">>, [<<"some_value">>]}
                         ]}
                       }
                      ]}
                    }
                   ]}),
          {ok, Config} = deliv_proj_config:validate(NVJson),
          Criteria = #deliv_ssh_job_criteria{
              os = [<<"some_value">>, <<"some_value">>],
              platform = [<<"some_value">>],
              platform_family = [<<"some_value">>],
              platform_version = [<<"some_value">>]
             },
          ?assertEqual([Criteria], deliv_proj_config:get_phase_filters_from_config(Config, <<"unit">>))
      end},
     {"if matrix definition exists for phase, then all filters returned",
      fun() ->
          FullJson = make_full_valid_json(),
          NVJson = ej:set({<<"job_dispatch">>}, FullJson,
                  {[
                    {<<"version">>, ?JOB_DISPATCH_V2},
                    {<<"filters">>,
                     {[
                       {<<"unit">>,
                        [
                         {[
                           {<<"os">>, [<<"some_value">>, <<"some_value">>]},
                           {<<"platform">>, [<<"some_value">>]},
                           {<<"platform_family">>, [<<"some_value">>]},
                           {<<"platform_version">>, [<<"some_value">>]}
                          ]},
                         {[
                           {<<"os">>, [<<"other_value">>, <<"other_value">>]},
                           {<<"platform">>, [<<"other_value">>]},
                           {<<"platform_family">>, [<<"other_value">>]},
                           {<<"platform_version">>, [<<"other_value">>]}
                          ]}
                        ]
                       }
                      ]}
                    }
                   ]}),
          {ok, Config} = deliv_proj_config:validate(NVJson),
          Filter1 = #deliv_ssh_job_criteria{
              os = [<<"some_value">>, <<"some_value">>],
              platform = [<<"some_value">>],
              platform_family = [<<"some_value">>],
              platform_version = [<<"some_value">>]
             },
          Filter2 = #deliv_ssh_job_criteria{
              os = [<<"other_value">>, <<"other_value">>],
              platform = [<<"other_value">>],
              platform_family = [<<"other_value">>],
              platform_version = [<<"other_value">>]
             },
          ?assertEqual([Filter1, Filter2], deliv_proj_config:get_phase_filters_from_config(Config, <<"unit">>))
      end}
    ].

criteria_to_json() ->
    [{"If empty deliv_ssh_job_criteria is provided empty JSON object is returned",
      fun() ->
          ?assertEqual(<<"{}">>, deliv_proj_config:criteria_to_json(#deliv_ssh_job_criteria{}))
     end},
     {"If partially populated deliv_ssh_job_criteria is provided partially populated JSON object is returned",
       fun() ->
           Criteria = #deliv_ssh_job_criteria{os = [<<"linux">>]},
           ?assertEqual(<<"{\"os\":[\"linux\"]}">>, deliv_proj_config:criteria_to_json(Criteria))
      end},
      {"If fully populated deliv_ssh_job_criteria is provided fully populated JSON object is returned",
        fun() ->
            Criteria = #deliv_ssh_job_criteria{
                os = [<<"linux">>],
                platform = [<<"ubuntu">>],
                platform_family = [<<"debian">>],
                platform_version = [<<"16.04">>]
            },
            ?assertEqual(
                <<"{\"os\":[\"linux\"],\"platform\":[\"ubuntu\"],\"platform_family\":[\"debian\"],\"platform_version\":[\"16.04\"]}">>,
                deliv_proj_config:criteria_to_json(Criteria)
            )
       end}
     ].

get_dependencies_returns_empty_list_if_no_dependencies() ->
    [{"can get dependencies from valid version 2 config.",
      fun() ->
          FullJson = make_full_valid_json(2),
          NVJson = ej:set({<<"dependencies">>}, FullJson, []),
          {ok, Cfg} = deliv_proj_config:validate(make_config(NVJson)),

          ExpectedDependencies = [],
          ActualDependencies = deliv_proj_config:get_dependencies(Cfg, <<"test_enterprise">>, <<"test_organization">>),

          ?assertEqual(ExpectedDependencies, ActualDependencies)
      end}].

get_dependency_ids_throws_error_if_patchset_does_not_exist() ->
    ChangeId = <<"1">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),
    Scope = deliv_scopes:'#new_common'(),

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                  ?withArgs([ChangeId]),
                  ?andReturn({error, patchset_not_found}))),

    Actual = deliv_proj_config:get_dependency_ids(Scope, Change),
    ?assertEqual({error, patchset_not_found}, Actual),
    ?verifyAll.

get_dependency_ids_throws_error_if_config_cant_be_loaded() ->
    ChangeId = <<"1">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),

    ScmMod = deliv_scm_module,
    Scope = deliv_scopes:'#new_common'([{scm_module, ScmMod}]),

    Patchset = deliv_patchset:'#new'(),

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                  ?withArgs([ChangeId]),
                  ?andReturn({ok, Patchset}))),
    hoax:mock(ScmMod,
              ?expect(load_config_for_patchset,
                  ?withArgs([Patchset, Scope]),
                  ?andReturn({error, config_error}))),

    Actual = deliv_proj_config:get_dependency_ids(Scope, Change),
    ?assertEqual({error, config_error}, Actual),
    ?verifyAll.

get_dependency_ids_skips_invalid_pipeline_names() ->
    ChangeId = <<"1">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),

    ScmMod = deliv_scm_module,
    EntName = <<"test_enterprise">>,
    OrgName = <<"test_org">>,
    Scope = deliv_scopes:'#new_common'([
        {scm_module, ScmMod},
        {scoping_names, [EntName, OrgName, proj, pipe]}
    ]),

    ProjName = <<"project1">>,
    ProjName2 = <<"project2">>,
    PipeName2 = <<"not_master">>,
    FullJson = make_full_valid_json(2),
    Dependencies = [ProjName, chef_utils:join_binaries([ProjName2,PipeName2], <<":">>)],
    NVJson = ej:set({<<"dependencies">>}, FullJson, Dependencies),
    {ok, Cfg} = deliv_proj_config:validate(make_config(NVJson)),

    Patchset = deliv_patchset:'#new'(),
    Pipeline = deliv_pipeline:'#new'(),

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                  ?withArgs([ChangeId]),
                  ?andReturn({ok, Patchset}))),
    hoax:mock(ScmMod,
              ?expect(load_config_for_patchset,
                  ?withArgs([Patchset, Scope]),
                  ?andReturn({ok, Cfg}))),
    hoax:mock(deliv_pipeline,
             [?expect(fetch,
                  ?withArgs([EntName, OrgName, ProjName, <<"master">>]),
                  ?andReturn({ok, Pipeline})),
              ?expect(getval,
                  ?withArgs([id, Pipeline]),
                  ?andReturn(1)),
              ?expect(fetch,
                   ?withArgs([EntName, OrgName, ProjName2, PipeName2]),
                   ?andReturn({error, not_found}))
              ]),

    Actual = deliv_proj_config:get_dependency_ids(Scope, Change),
    ?assertEqual({ok, [1]}, Actual),
    ?verifyAll.

get_dependency_ids_returns_dep_ids() ->
    ChangeId = <<"1">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),

    ScmMod = deliv_scm_module,
    EntName = <<"test_enterprise">>,
    Org1Name = <<"test_org">>,
    Org2Name = <<"test_org2">>,
    Scope = deliv_scopes:'#new_common'([
        {scm_module, ScmMod},
        {scoping_names, [EntName, Org1Name, proj, pipe]}
    ]),
    Proj1Name = <<"project1">>,
    Proj2Name = <<"project2">>,
    Proj3Name = <<"project3">>,
    Proj4Name = <<"project4">>,
    Master = <<"master">>,
    NotMaster = <<"not_master">>,

    FullJson = make_full_valid_json(2),
    Dependencies = [
        <<"test_org/project1:not_master">>,
        <<"test_org2/project2">>,
        <<"project3:not_master">>,
        Proj4Name
    ],
    NVJson = ej:set({<<"dependencies">>}, FullJson, Dependencies),
    {ok, Cfg} = deliv_proj_config:validate(make_config(NVJson)),

    Patchset = deliv_patchset:'#new'(),
    Pipeline = Pipeline2 = Pipeline3 = Pipeline4 = deliv_pipeline:'#new'(),

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                  ?withArgs([ChangeId]),
                  ?andReturn({ok, Patchset}))),
    hoax:mock(ScmMod,
              ?expect(load_config_for_patchset,
                  ?withArgs([Patchset, Scope]),
                  ?andReturn({ok, Cfg}))),
    hoax:mock(deliv_pipeline,
             [?expect(fetch,
                  ?withArgs([EntName, Org1Name, Proj1Name, NotMaster]),
                  ?andReturn({ok, Pipeline})),
              ?expect(fetch,
                  ?withArgs([EntName, Org2Name, Proj2Name, Master]),
                  ?andReturn({ok, Pipeline2})),
              ?expect(fetch,
                  ?withArgs([EntName, Org1Name, Proj3Name, NotMaster]),
                  ?andReturn({ok, Pipeline3})),
              ?expect(fetch,
                  ?withArgs([EntName, Org1Name, Proj4Name, Master]),
                  ?andReturn({ok, Pipeline4})),
              ?expect(getval,
                  ?withArgs([id, Pipeline]),
                  ?andReturn(1)),
              ?expect(getval,
                  ?withArgs([id, Pipeline2]),
                  ?andReturn(2)),
              ?expect(getval,
                  ?withArgs([id, Pipeline3]),
                  ?andReturn(3)),
              ?expect(getval,
                  ?withArgs([id, Pipeline4]),
                  ?andReturn(4))
             ]),

    ActualResult = deliv_proj_config:get_dependency_ids(Scope, Change),
    ?assertEqual({ok, [4, 3, 2, 1]}, ActualResult),
    ?verifyAll.

%% administrivia
make_config(Json) ->
  chef_json:encode(Json).

make_minimal_valid_json() ->
    make_minimal_valid_json(1).

make_minimal_valid_json(1) ->
  {[{<<"version">>, <<"1">>},
    {<<"build_cookbook">>, <<"simple_build">>}]};
make_minimal_valid_json(2) ->
  {[
    {<<"version">>, <<"2">>},
    {<<"build_cookbook">>,
     {[
       {<<"name">>, <<"simple_build">>},
       {<<"path">>, <<"/tmp">>}
      ]}
    }
   ]};
make_minimal_valid_json(_) ->
    make_minimal_valid_json(1).



make_moderate_valid_json(Version) ->
  Min = make_minimal_valid_json(Version),
  ej:set({<<"skip_phases">>}, Min, [<<"lint">>, <<"syntax">>]).

make_full_valid_json() ->
    make_full_valid_json(1).

make_full_valid_json(Version) ->
  Min = make_moderate_valid_json(Version),
  ej:set({<<"build_nodes">>}, Min,
         {[
           {<<"default">>, [<<"default:search">>]},
           {<<"lint">>, [<<"lint:search">>]},
           {<<"syntax">>, [<<"syntax:search">>]},
           {<<"unit">>, [<<"unit:search">>]},
           {<<"security">>, [<<"security:search">>]},
           {<<"quality">>, [<<"quality:search">>]},
           {<<"publish">>, [<<"publish:search">>]},
           {<<"provision">>, [<<"provision:search">>]},
           {<<"deploy">>, [<<"deploy:search">>]},
           {<<"smoke">>, [<<"smoke:search">>]},
           {<<"functional">>, [<<"functional:search">>]}
         ]}).

make_valid_config_default_search() ->
  Min = make_minimal_valid_json(),
  ej:set({<<"build_nodes">>}, Min,
         {[
            {<<"default">>, [<<"default:search">>]}
         ]}).

make_config_missing_version() ->
  chef_json:encode({[{<<"build_cookbook">>, <<"simple_build">>}]}).

make_config_missing_build_cookbook() ->
  chef_json:encode({[{<<"version">>, <<"1">>}]}).

make_invalid_json() ->
    <<"adl;kfjgnba;ksdjfgba;dkjgba;dkfjbasdf;kajbdfiq;uebnfiawuebfgallksdj">>.
