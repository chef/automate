-module(deliv_git_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_types.hrl").

this_repo() ->
    app_test_helpers:project_root(?MODULE).

fixture_test_() ->
    hoax:fixture(?MODULE, "ensure_git_and_repo_path_").

-compile(export_all).

create_repo_test_() ->
    eu_git:with_test_env(fun(_) ->
        TmpRepoPath = ec_file:insecure_mkdtemp(),
        TemplateDir = delivery_app:get_env(deliv_git_repo_template),
        [%% create the repo
         ?_assertMatch({ok, _}, deliv_git:create_repo(TmpRepoPath)),
         %% let's check it looks like a bare repo
         [ ?_assert(filelib:is_dir(filename:join(TmpRepoPath, Dir)))
          || Dir <- ["hooks", "objects", "refs"]],
         [ ?_assert(filelib:is_regular(filename:join(TmpRepoPath, File)))
          || File <- ["HEAD", "config", "description"]],
         %% and let's check our template dir got loaded properly
         ?_assertEqual(
             {0, <<>>},
             chef_utils:run_cmd(["find . -type f -print0 | xargs -0 -I{} diff -q {}", filename:join([TmpRepoPath, "{}"])],
                                 TemplateDir)
         )]
    end).

uri_test_() ->
    eu_git:with_test_env(fun(_) ->
        [?_assertEqual(<<"ssh://ab@opscode@my-delivery.chef.io:1234/opscode/chef/delivery">>,
                      deliv_git:uri(<<"ab">>, <<"opscode">>,
                                    <<"chef">>, <<"delivery">>))]
    end).

fips_uri_test_() ->
    eu_git:with_test_env(fun(_) ->
        [?_assertEqual(<<"ssh://ab@opscode@localhost:11111/opscode/chef/delivery">>,
                      deliv_git:fips_uri(<<"ab">>, <<"opscode">>,
                                         <<"chef">>, <<"delivery">>))]
    end).

process_diffstats_test_() ->
    Stats = fun(FilesChanged, Insertions, Deletions) ->
                    {ok, deliv_patchset_diffstat:fromlist([{files_changed, FilesChanged},
                                                           {insertions, Insertions},
                                                           {deletions, Deletions}])}
            end,
    [
     {Label,
     ?_assertEqual(Stats(Files, Insertions, Deletions),
                   deliv_git:process_diffstats({0, Output}, undefined))} ||
        {Label, Output, Files, Insertions, Deletions} <-
            [{"All diff information present",
              <<" 19 files changed, 1079 insertions(+), 24 deletions(-)\n">>,
              19, 1079, 24},

             {"Just deletions",
              <<" 19 files changed, 24 deletions(-)\n">>,
              19, 0, 24
             },

             {"Just insertions",
              <<" 19 files changed, 1079 insertions(+)\n">>,
              19, 1079, 0
             },

             {"One file changed",
              <<" 1 file changed, 1079 insertions(+), 24 deletions(-)\n">>,
              1, 1079, 24
             },

             {"EL GIGANTE",
              <<" 12352341 files changed, 107984829294 insertions(+), 2248482914 deletions(-)\n">>,
              12352341, 107984829294, 2248482914
             },

             {"Try it with a fully-trimmed string, for giggles",
              <<"19 files changed, 1079 insertions(+), 24 deletions(-)">>,
              19, 1079, 24},

             {"Blank output (This happens with empty commits)",
              <<"">>,
              0, 0, 0},

             {"Single insertion",
              <<" 1 file changed, 1 insertion(+)\n">>,
              1, 1, 0},

             {"Single deletion",
              <<" 2 files changed, 2 insertions(+), 1 deletion(-)\n">>,
              2, 2, 1}

            ]].

create_delete_branch_test_() ->
    eu_git:with_work_repo_and_master_branch(fun({BareRepoPath, _WorkRepoPath}) ->
        MasterBranchName = <<"master">>,
        NewBranchName = <<"new_branch">>,
        %% check that the branch actually doesn't exist before we do anything
        [?_assertEqual(no_branch_exists,
                       deliv_git:branch_exists(BareRepoPath, NewBranchName)),
         ?_assertEqual(ok, deliv_git:create_branch(BareRepoPath,
                                                   NewBranchName,
                                                   MasterBranchName)),
         %% let's check that the new branch actually exists
         ?_assertEqual(branch_exists,
                       deliv_git:branch_exists(BareRepoPath, NewBranchName)),
         %% that the ref branch also still exits
         ?_assertEqual(branch_exists,
                       deliv_git:branch_exists(BareRepoPath, MasterBranchName)),
         %% and finally that the sha for the new branch is the same as for the old one
         ?_assertEqual(eu_git:get_sha(BareRepoPath, MasterBranchName),
                       eu_git:get_sha(BareRepoPath, NewBranchName)),
         %% delete the branch and reassure ourselves that it's no longer there
         ?_assertEqual(ok, deliv_git:force_delete_branch(BareRepoPath, NewBranchName)),
         ?_assertEqual(no_branch_exists, deliv_git:branch_exists(BareRepoPath, NewBranchName))]
    end).

move_branch_test_() ->
    eu_git:with_work_repo_and_master_branch(fun({BareRepoPath, _WorkRepoPath}) ->
        MasterBranchName = <<"master">>,
        NewBranchName = <<"new_branch">>,
        Sha = eu_git:get_sha(BareRepoPath, MasterBranchName),
        [?_assertEqual(ok, deliv_git:move_branch(BareRepoPath,
                                                 MasterBranchName,
                                                 NewBranchName)),
         %% let's check that the new branch actually exists
         ?_assertEqual(branch_exists,
                       deliv_git:branch_exists(BareRepoPath, NewBranchName)),
         %% that the old branch doesn't exist any more
         ?_assertEqual(no_branch_exists,
                       deliv_git:branch_exists(BareRepoPath, MasterBranchName)),
         %% and finally that the sha for the new branch is the same as for the old one
         ?_assertEqual(Sha,
                       eu_git:get_sha(BareRepoPath, NewBranchName))]
    end).

diffstats_test_() ->
    eu_git:with_test_env(fun(_) ->
        {ok, Stats1} = deliv_git:diffstats(this_repo(), "ea62402d6", "0264da1e1271", undefined),
        {ok, Stats2} = deliv_git:diffstats(this_repo(), "42314f35f", "e8edc7343bf5", undefined),
        [?_assertEqual(deliv_patchset_diffstat:fromlist([{files_changed, 34},
                                                         {insertions, 378},
                                                         {deletions, 216}]), Stats1),
         ?_assertEqual(deliv_patchset_diffstat:fromlist([{files_changed, 1},
                                                         {insertions, 0},
                                                         {deletions, 47}]), Stats2)]
    end).

changed_files_test_() ->
    eu_git:with_test_env(fun(_) ->
        {ok, Files} = deliv_git:changed_files(this_repo(), "ea62402d6", "0264da1e1271", undefined),

        ExpectedStatesAndFileNames =
        [ {<<"modified">>, <<"infra/Berksfile">>,<<"1">>,<<"1">>},
          {<<"modified">>, <<"infra/cookbooks/delivery/Berksfile">>,<<"1">>, <<"1">>},
          {<<"modified">>, <<"infra/cookbooks/delivery/Berksfile.lock">>,<<"3">>, <<"3">>},
          {<<"modified">>, <<"infra/cookbooks/delivery/templates/default/delivery-sys.config.erb">>, <<"1">>,<<"4">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/Berksfile.lock">>,<<"3">>, <<"3">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/build-essential/CHANGELOG.md">>, <<"5">>,<<"0">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/build-essential/libraries/xcode_command_line_tools.rb">>, <<"4">>,<<"3">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/build-essential/metadata.json">>, <<"1">>,<<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/build-essential/recipes/_rhel.rb">>, <<"1">>,<<"0">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/enterprise/recipes/runit.rb">>, <<"1">>,<<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/enterprise/recipes/runit_sysvinit.rb">>, <<"1">>,<<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/enterprise/recipes/runit_upstart.rb">>, <<"1">>,<<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum-epel/CHANGELOG.md">>, <<"11">>,<<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum-epel/README.md">>,<<"4">>, <<"0">>},
          {<<"added">>, <<"infra/vendor/cookbooks/yum-epel/attributes/default.rb">>, <<"1">>,<<"0">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum-epel/metadata.json">>, <<"6">>,<<"2">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum-epel/recipes/default.rb">>, <<"15">>,<<"10">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/CHANGELOG.md">>,<<"26">>, <<"0">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/README.md">>,<<"11">>, <<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/attributes/main.rb">>, <<"1">>,<<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/metadata.json">>,<<"2">>, <<"2">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/providers/repository.rb">>, <<"17">>,<<"3">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/recipes/default.rb">>, <<"72">>,<<"7">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/resources/globalconfig.rb">>, <<"5">>,<<"2">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/resources/repository.rb">>, <<"7">>,<<"3">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/templates/default/main.erb">>, <<"19">>,<<"1">>},
          {<<"modified">>, <<"infra/vendor/cookbooks/yum/templates/default/repo.erb">>, <<"17">>,<<"4">>},
          {<<"modified">>, <<"src/deliv_change.erl">>,<<"6">>,<<"9">>},
          {<<"modified">>, <<"src/deliv_git.erl">>,<<"124">>,<<"111">>},
          {<<"modified">>, <<"src/deliv_hand_pipelines.erl">>,<<"1">>,<<"1">>},
          {<<"modified">>, <<"src/deliv_hand_post_receive_hook.erl">>,<<"2">>, <<"2">>},
          {<<"modified">>, <<"src/deliv_utils.erl">>,<<"3">>,<<"22">>},
          {<<"modified">>, <<"test/ct/app_test_helpers.erl">>,<<"1">>,<<"2">>},
          {<<"modified">>, <<"test/unit/deliv_git_tests.erl">>,<<"4">>,<<"13">>}],

        Expected = [deliv_patchset_changed_file:fromlist([{status, State},
                                                           {file, File},
                                                           {inserts, Inserts},
                                                           {deletes, Deletes}])
                    || {State, File, Inserts, Deletes} <- ExpectedStatesAndFileNames ],

        [?_assertEqual(Expected, Files)]
    end).

log_test_() ->
    eu_git:with_test_env(fun(_) ->
        {ok, Log} = deliv_git:log(this_repo(), "ea62402d6", "0264da1e1271"),
        Expected = deliv_patchset_commit:fromlist([{sha, <<"0264da1e12713a23bdebe04d1cd29d2e75f93ae6">>},
                                                        {subject, <<"Re-vendor cookbooks, point to Supermarket API">>},
                                                        {body, <<>>}]),

        [?_assertEqual([Expected], Log)]
    end).

merge_base_test_() ->
    eu_git:with_test_env(fun(_) ->
        Base = deliv_git:merge_base(this_repo(), "ea62402d6", "0264da1e1271"),

        [?_assertEqual(<<"da0ad433a7ea170b4f49f4f9e354e92c9a50fc8f">>,
                      Base)]
    end).


%% file_at_sha is just a wrapper, no need to test separately until we
%% hit error cases, maybe
files_at_sha_test_() ->
    eu_git:with_test_env(fun(_) ->
        Files = deliv_git:files_at_sha(this_repo(), "9af1fd3eec5eb843fb",
                                       ["relx.config", "rebar.config", "concrete.mk"]),
        [File|_] = Files,
        [?_assertEqual(3, erlang:length(Files)),
         ?_assertEqual(
            <<"%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-\n"
              "{release, {delivery, \"0.0.1\"},\n"
              " [delivery,\n  %% We want sync to be in our build, but do not want it to start by\n"
              "  %% default. With this here, it _will_ get started by the relx generating boot\n"
              "  %% script. We're going to live with that for now since it is useful for\n"
              "  %% development and that's what we're doing. But before we \"ship\" we need a\n"
              "  %% different solution.\n  sync\n ]}.\n\n{extended_start_script, true}.\n">>, File)]
    end).

diff_sha_test_() ->
    eu_git:with_test_env(fun(_) ->
        DiffContent = deliv_git:file_diff(this_repo(), "ea62402d6", "0264da1e1271", "../../../infra/Berksfile"),
         ?_assertEqual(
            {ok, <<"diff --git a/infra/Berksfile b/infra/Berksfile\nindex 9a2549e220..a962886a1d 100644\n---"
                   " a/infra/Berksfile\n+++ b/infra/Berksfile\n@@ -1,4 +1,4 @@\n-source "
                   "\"http://api.berkshelf.com\"\n+source \"https://supermarket.getchef.com\"\n \n "
                   "# By default, pulls in the current master of the given cookbook from\n # the "
                   "'opscode-cookbooks' organization on Github.  The branch, tag, or\n">>}, DiffContent)
    end).

%% should compare with the previous version instead of returning nothing
diff_same_sha_diff_test_() ->
    eu_git:with_test_env(fun(_) ->
         DiffContent = deliv_git:file_diff(this_repo(),
             <<"01a3ff453a5fb88ca37b98b92ffd42f9d6b76b7b">>,
             <<"01a3ff453a5fb88ca37b98b92ffd42f9d6b76b7b">>,
             "../../../infra/Berksfile", 3),
         ?_assertEqual(
            {ok,<<"diff --git a/infra/Berksfile b/infra/Berksfile\nindex a28c81c27b..37bc75cb0a 100644\n--- a/infra/Berksfile\n+++ b/infra/Berksfile\n@@ -42,7 +42,6 @@ end\n # Get the paths for all cookbooks in a local directory\n #\n # @example Basic Usage\n-#   cookbooks_for(\"cookbooks\") => [\"cookbooks/delivery\", \"cookbooks/gerrit\"]\n def cookbooks_for(path)\n   Dir[\"#{path}/*\"].reject{|cb| File.basename(cb) == \"Berksfile.lock\"}\n end\n">>}, DiffContent)
    end).

commit_newline_test_() ->
    eu_git:with_test_env(fun(_) ->
        {ok, [Commit]} =
            deliv_git:log(this_repo(), "0264da1e12713a23bdebe04d1cd29d2e75f93ae6", "ea62402d6c2bfcd1e77a2d5b7f64b956f9408bd1"),

        Body = deliv_patchset_commit:getval(body, Commit),
        ?_assertEqual(
            <<"the primary motivation for this patch was moving `deliv_git` to use\n"
               "`{spawn_executable, Git}`, rather than a plain spawn, which would\n"
               "involve the shell.  This requires a slight amount of rewriting for where\n"
               "we were quoting or using backticks to do subcommands.\n\n"
               "additionally I refactored the module to use the run_git command, and got\n"
               "rid of all straight calls to `deliv_util:run_cmd`.\n\n"
               "I've opened a card planning some future work getting rid of all calls to\n"
               "`run_git` and a full audit of the code for other uses of git, as well as\n"
               "expanding unit test coverage of the `deliv_git` module, but am pushing\n"
               "out this change without those enhancements in the hopes of preventing\nbitrot.">>,
            Body)
   end).

ensure_hooks_test_() ->
    eu_git:with_test_env(fun(_) ->
        TmpRepoPath = ec_file:insecure_mkdtemp(),
        TemplateDir = delivery_app:get_env(deliv_git_repo_template),
        [%% create the repo
         ?_assertMatch({ok, _}, deliv_git:create_repo(TmpRepoPath)),
         ?_assertEqual(ok, deliv_git:ensure_hooks(TmpRepoPath)),
         %% let's make sure that hooks is a link.
         ?_assertEqual({ok, TemplateDir ++ "/hooks"},
                       file:read_link(filename:join(TmpRepoPath,
                                                    "hooks")))
        ]
    end).

ensure_hooks_no_dir_test_() ->
    eu_git:with_test_env(fun(_) ->
        TmpRepoPath = ec_file:insecure_mkdtemp(),
        TemplateDir = delivery_app:get_env(deliv_git_repo_template),
        [%% create the repo
         ?_assertMatch({ok, _}, deliv_git:create_repo(TmpRepoPath)),
         ?_assertEqual(ok, ec_file:remove(TmpRepoPath ++ "/hooks",
                                          [recursive])),
         ?_assertEqual(ok, deliv_git:ensure_hooks(TmpRepoPath)),
         %% let's make sure that hooks is a link.
         ?_assertEqual({ok, TemplateDir ++ "/hooks"},
                       file:read_link(filename:join(TmpRepoPath,
                                                    "hooks")))
        ]
    end).

ensure_hooks_file_not_dir_test_() ->
    eu_git:with_test_env(fun(_) ->
        TmpRepoPath = ec_file:insecure_mkdtemp(),
        TemplateDir = delivery_app:get_env(deliv_git_repo_template),
        [%% create the repo
         ?_assertMatch({ok, _}, deliv_git:create_repo(TmpRepoPath)),
         ?_assertEqual(ok, ec_file:remove(TmpRepoPath ++ "/hooks",
                                          [recursive])),
         ?_assertEqual(ok, file:write_file(TmpRepoPath ++ "/hooks",
                                           "Fake Hook")),
         ?_assertEqual(ok, deliv_git:ensure_hooks(TmpRepoPath)),
         %% let's make sure that hooks is a link.
         ?_assertEqual({ok, TemplateDir ++ "/hooks"},
                       file:read_link(filename:join(TmpRepoPath,
                                                    "hooks")))
        ]
    end).

ensure_hooks_bad_link_test_() ->
    eu_git:with_test_env(fun(_) ->
        TmpRepoPath = ec_file:insecure_mkdtemp(),
        TemplateDir = delivery_app:get_env(deliv_git_repo_template),
        [%% create the repo
         ?_assertMatch({ok, _}, deliv_git:create_repo(TmpRepoPath)),
         ?_assertEqual(ok, ec_file:remove(TmpRepoPath ++ "/hooks",
                                          [recursive])),
         ?_assertEqual(ok, file:make_symlink(ec_file:insecure_mkdtemp(),
                                             TmpRepoPath ++ "/hooks")),
         ?_assertEqual(ok, deliv_git:ensure_hooks(TmpRepoPath)),
         %% let's make sure that hooks is a link.
         ?_assertEqual({ok, TemplateDir ++ "/hooks"},
                       file:read_link(filename:join(TmpRepoPath,
                                                    "hooks")))
        ]
    end).

ensure_git_and_repo_path_returns_error_no_git_or_repo_path_if_no_git() ->
    hoax:expect(receive
                    chef_utils:find_executable("git") -> false
                end),
    Result = deliv_git:ensure_git_and_repo_path(undefined, anything),
    ?assertEqual({error, no_git_or_repo_path}, Result),
    ?verifyAll.

ensure_git_and_repo_path_returns_system_git_if_the_repo_path_exists() ->
    Path = "/usr/bin/git",
    hoax:expect(receive
                    chef_utils:find_executable("git") -> Path
                end),
    Result = deliv_git:ensure_git_and_repo_path(undefined, ok),
    ?assertEqual({ok, Path}, Result),
    ?verifyAll.

ensure_git_and_repo_path_returns_git_path_if_git_is_defined() ->
    Path = "/usr/bin/git",
    Result = deliv_git:ensure_git_and_repo_path({ok, Path}, ok),
    ?assertEqual({ok, Path}, Result),
    ?verifyAll.
