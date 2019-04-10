-module(scm_git_client_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

async_force_push_test_() ->
    hoax:fixture(?MODULE, "async_force_push_").

async_delete_branch_test_() ->
    hoax:fixture(?MODULE, "async_delete_branch_").

force_push_test_() ->
    hoax:fixture(?MODULE, "force_push_").

delete_branch_test_() ->
    hoax:fixture(?MODULE, "delete_branch_").

async_force_push_returns_ok_and_calls_worker_force_push() ->
    Source = <<"SourceBranch">>,
    Destination = <<"DestinationBranch">>,
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},

    hoax:mock(scm_git_worker,
              ?expect(force_push,
                      ?withArgs([Source, Destination, Coords]),
                      ?andReturn(ok))),

    ?assertEqual(ok, scm_git_client:async_force_push(Source, Destination, Coords)),
    ?verifyAll.

async_delete_branch_returns_ok_and_calls_worker_delete() ->
    Branch = <<"Branch">>,
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},

    hoax:mock(scm_git_worker,
              ?expect(delete_branch,
                      ?withArgs([Branch, Coords]),
                      ?andReturn(ok))),

    ?assertEqual(ok, scm_git_client:async_delete_branch(Branch, Coords)),
    ?verifyAll.

force_push_pushes_code_to_bitbucket() ->
    Source = <<"SourceBranch">>,
    Destination = <<"DestinationBranch">>,
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    RepoPath = <<"repo/path">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitUrl = <<"https://git.url">>,
    GitCmd = [<<"push">>, <<"--force">>,
              GitUrl, <<Source/binary, ":", Destination/binary>>],
    TrustedCertPath = "/etc/ssl/certs/ca-certificates.crt",
    Env = [{"GIT_SSL_CAINFO", TrustedCertPath}],

    hoax:expect(
      receive
          scm_repo:url(Coords) -> {ok, GitUrl};
          delivery_app:get_env(trusted_certificates_file) -> TrustedCertPath;
          deliv_project:repo_path(EntName, OrgName, ProjName) -> {ok, RepoPath};
          deliv_git:run_git(bitbucket_scm, RepoPath, GitCmd, Env) -> {ok, <<"output">>}
      end),

    Expected = {ok, <<"output">>},
    Result = scm_git_client:force_push(Source, Destination, Coords),
    ?assertEqual(Expected, Result),
    ?verifyAll.

delete_branch_pushes_with_empty_source_branch() ->
    FeatureBranch = <<"FeatureBranch">>,
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    RepoPath = <<"repo/path">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitUrl = <<"https://git.url">>,
    GitCmd = [<<"push">>, <<"--force">>,
              GitUrl, <<":", FeatureBranch/binary>>],
    TrustedCertPath = "/etc/ssl/certs/ca-certificates.crt",
    Env = [{"GIT_SSL_CAINFO", TrustedCertPath}],


    hoax:expect(
      receive
          scm_repo:url(Coords) -> {ok, GitUrl};
          delivery_app:get_env(trusted_certificates_file) -> TrustedCertPath;
          deliv_project:repo_path(EntName, OrgName, ProjName) -> {ok, RepoPath};
          deliv_git:run_git(bitbucket_scm, RepoPath, GitCmd, Env) -> {ok, <<"output">>}
      end),

    Expected = {ok, <<"output">>},
    ?assertEqual(Expected, scm_git_client:delete_branch(FeatureBranch, Coords)),
    ?verifyAll.
