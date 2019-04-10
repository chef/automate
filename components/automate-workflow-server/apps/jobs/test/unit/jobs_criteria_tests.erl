-module(jobs_criteria_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_criteria_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "match_")
    ].

match_when_match_is_perfect_matches() ->
    Os = <<"linux">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    PlatformVersion = <<"15.04">>,
    Criteria = #deliv_ssh_job_criteria{os = [Os],
                                       platform_family = [PlatformFamily],
                                       platform = [Platform],
                                       platform_version = [PlatformVersion]},
    Properties = #runner_properties{os = Os,
                                    platform_family = PlatformFamily,
                                    platform = Platform,
                                    platform_version = PlatformVersion},
    Actual = jobs_criteria:match(Criteria, Properties),
    ?assertEqual(true, Actual).

match_when_criteria_contains_many_things_matches_including_valid_match_returns_true() ->
    Os = <<"linux">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    PlatformVersion = <<"15.04">>,
    Criteria = #deliv_ssh_job_criteria{os = [Os, <<"other">>],
                                       platform_family = [PlatformFamily, <<"other">>],
                                       platform = [Platform, <<"other">>],
                                       platform_version = [PlatformVersion, <<"other">>]},
    Properties = #runner_properties{os = Os,
                                    platform_family = PlatformFamily,
                                    platform = Platform,
                                    platform_version = PlatformVersion},
    Actual = jobs_criteria:match(Criteria, Properties),
    ?assertEqual(true, Actual).

match_when_criteria_are_not_given_and_runner_properties_are_defined_returns_true() ->
    Os = <<"linux">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    PlatformVersion = <<"15.04">>,
    Criteria = #deliv_ssh_job_criteria{os = undefined,
                                       platform_family = undefined,
                                       platform = undefined,
                                       platform_version = undefined},
    Properties = #runner_properties{os = Os,
                                    platform_family = PlatformFamily,
                                    platform = Platform,
                                    platform_version = PlatformVersion},
    Actual = jobs_criteria:match(Criteria, Properties),
    ?assertEqual(true, Actual).

match_when_a_criterium_is_undefined_but_rest_are_matching_returns_true() ->
    Os = <<"linux">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    PlatformVersion = <<"15.04">>,
    Criteria = #deliv_ssh_job_criteria{os = [Os],
                                       platform_family = [PlatformFamily],
                                       platform = [Platform]},
    Properties = #runner_properties{os = Os,
                                    platform_family = PlatformFamily,
                                    platform = Platform,
                                    platform_version = PlatformVersion},
    Actual = jobs_criteria:match(Criteria, Properties),
    ?assertEqual(true, Actual).

match_when_runner_properties_are_not_matching_version_returns_false() ->
    Os = <<"linux">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    PlatformVersion = <<"15.04">>,
    Criteria = #deliv_ssh_job_criteria{os = [Os],
                                       platform_family = [PlatformFamily],
                                       platform = [Platform],
                                       platform_version = [PlatformVersion]},
    Properties = #runner_properties{os = Os,
                                    platform_family = PlatformFamily,
                                    platform = Platform,
                                    platform_version = <<"15.10">>},
    Actual = jobs_criteria:match(Criteria, Properties),
    ?assertEqual(false, Actual).

match_when_runner_properties_are_not_matching_with_many_values_returns_false() ->
    Os = <<"linux">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    PlatformVersion = <<"15.04">>,
    Criteria = #deliv_ssh_job_criteria{os = [Os, <<"other">>],
                                       platform_family = [PlatformFamily],
                                       platform_version = [PlatformVersion]},
    Properties = #runner_properties{os = Os,
                                    platform_family = PlatformFamily,
                                    platform = Platform,
                                    platform_version = <<"15.10">>},
    Actual = jobs_criteria:match(Criteria, Properties),
    ?assertEqual(false, Actual).
