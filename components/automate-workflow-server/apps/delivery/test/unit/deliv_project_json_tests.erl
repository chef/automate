-module(deliv_project_json_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Fixtures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_body_6_test_() ->
    hoax:fixture(?MODULE, "create_body_6_").

create_body_8_test_() ->
    hoax:fixture(?MODULE, "create_body_8_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_body_6_returns_json_when_scm_type_is_local() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    ScmType = <<"local">>,
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,

    AppEnv = [
              {deliv_ssh_git_port, 8888},
              {deliv_ssh_git_hostname, "host"}
             ],

    [application:set_env(delivery, K,V) || {K,V} <- AppEnv],
    Result = deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType, UserName, [PipeName]),
    Expected = {[{<<"name">>, ProjName},
                 {<<"ent_name">>, EntName},
                 {<<"org_name">>, OrgName},
                 {<<"scm">>,
                  {[
                    {<<"type">>, ScmType}
                   ]}},
                 {<<"git_url">>, deliv_git:uri(UserName, EntName, OrgName, ProjName)},
                 {<<"_links">>,
                  {[
                    {<<"full">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary>>}]}},
                    {<<"list_pipelines">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines">>}]}},
                    {pipelines,
                     [{[{href,
                         <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary,
                           "/pipelines/", PipeName/binary>>},
                        {name, PipeName}]}]}
                   ]}
                 }]},
    ?assertEqual(Expected, Result).

create_body_6_throws_undef_when_scm_type_is_not_local() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    ScmType = <<"bitbucket">>,
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,

    AppEnv = [
              {deliv_ssh_git_port, 8888},
              {deliv_ssh_git_hostname, "host"}
             ],

    [application:set_env(delivery, K,V) || {K,V} <- AppEnv],
    ?assertException(error, function_clause,
                     deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType, UserName, [PipeName])).


create_body_8_returns_json_when_scm_is_github() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    ScmType = <<"github">>,
    MetaData = deliv_project_github_metadata:fromlist([{project_id, 1},
                                                       {repo_owner, <<"github_owner">>},
                                                       {repo_name, <<"repo_name">>}]),
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,

    AppEnv = [
              {deliv_ssh_git_port, 8888},
              {deliv_ssh_git_hostname, "host"}
             ],

    ScmUrl = <<"https://github.com">>,

    [application:set_env(delivery, K,V) || {K,V} <- AppEnv],
    Result = deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType, MetaData, UserName, [PipeName], ScmUrl),
    Expected = {[{<<"name">>, ProjName},
                 {<<"ent_name">>, EntName},
                 {<<"org_name">>, OrgName},
                 {<<"scm">>,
                  {[
                    {<<"type">>, ScmType},
                    {<<"repo_owner">>, <<"github_owner">>},
                    {<<"repo_name">>, <<"repo_name">>}
                   ]}},
                 {<<"_links">>,
                  {[
                    {<<"full">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary>>}]}},
                    {<<"list_pipelines">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines">>}]}},
                    {pipelines,
                     [{[{href,
                         <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary,
                           "/pipelines/", PipeName/binary>>},
                        {name, PipeName}]}]}
                   ]}
                 }]},
    ?assertEqual(Expected, Result).

create_body_8_returns_json_when_scm_is_bitbucket() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    ScmType = <<"bitbucket">>,
    MetaData = scm_bitbucket_project_metadata:fromlist([{project_id, 1},
                                                    {bitbucket_project, <<"project_key">>},
                                                    {repo_name, <<"repo_name">>}]),
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,

    AppEnv = [
              {deliv_ssh_git_port, 8888},
              {deliv_ssh_git_hostname, "host"}
             ],

    ScmUrl = <<"https://bitbucket.com">>,
    FullScmUrl = <<"https://bitbucket.com/projects/project_key/repos/repo_name">>,

    [application:set_env(delivery, K,V) || {K,V} <- AppEnv],
    Result = deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType, MetaData, UserName, [PipeName], ScmUrl),
    Expected = {[{<<"name">>, ProjName},
                 {<<"ent_name">>, EntName},
                 {<<"org_name">>, OrgName},
                 {<<"scm">>,
                  {[
                    {<<"type">>, ScmType},
                    {<<"url">>, FullScmUrl},
                    {<<"project_key">>, <<"project_key">>},
                    {<<"repo_name">>, <<"repo_name">>}
                   ]}},
                 {<<"_links">>,
                  {[
                    {<<"full">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary>>}]}},
                    {<<"list_pipelines">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines">>}]}},
                    {pipelines,
                     [{[{href,
                         <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary,
                           "/pipelines/", PipeName/binary>>},
                        {name, PipeName}]}]}
                   ]}
                 },
                 {<<"git_url">>, deliv_git:uri(UserName, EntName, OrgName, ProjName)}]},
    ?assertEqual(ej:get({"git_url"}, Expected), ej:get({"git_url"}, Result)),
    ?assertEqual(Expected, Result).

create_body_8_returns_json_when_scm_is_githubV2() ->
    EntName = <<"entname">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    ScmType = <<"githubV2">>,
    MetaData = scm_github_project_metadata:fromlist([{project_id, 1},
                                                     {repo_owner, <<"github_owner">>},
                                                     {repo_name, <<"repo_name">>}]),
    UserName = <<"username">>,
    PipeName = <<"pipelinename">>,

    AppEnv = [
              {deliv_ssh_git_port, 8888},
              {deliv_ssh_git_hostname, "host"}
             ],

    ScmUrl = <<"https://github.com">>,
    FullScmUrl = <<"https://github.com/github_owner/repo_name">>,

    [application:set_env(delivery, K,V) || {K,V} <- AppEnv],
    Result = deliv_project_json:create_body(EntName, OrgName, ProjName, ScmType, MetaData, UserName, [PipeName], ScmUrl),
    Expected = {[{<<"name">>, ProjName},
                 {<<"ent_name">>, EntName},
                 {<<"org_name">>, OrgName},
                 {<<"scm">>,
                  {[
                    {<<"type">>, ScmType},
                    {<<"url">>, FullScmUrl},
                    {<<"repo_owner">>, <<"github_owner">>},
                    {<<"repo_name">>, <<"repo_name">>}
                   ]}},
                 {<<"_links">>,
                  {[
                    {<<"full">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary>>}]}},
                    {<<"list_pipelines">>,
                     {[{<<"href">>,
                        <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary, "/pipelines">>}]}},
                    {pipelines,
                     [{[{href,
                         <<"/api/v0/e/", EntName/binary, "/orgs/", OrgName/binary, "/projects/", ProjName/binary,
                           "/pipelines/", PipeName/binary>>},
                        {name, PipeName}]}]}
                   ]}
                 },
                 {<<"git_url">>, deliv_git:uri(UserName, EntName, OrgName, ProjName)}]},
    ?assertEqual(ej:get({"git_url"}, Expected), ej:get({"git_url"}, Result)),
    ?assertEqual(Expected, Result).
