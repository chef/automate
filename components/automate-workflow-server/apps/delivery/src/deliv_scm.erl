%% @doc Behavior definition for deliv_scm.
%%
%% This (and other dynamic scm modules) are used for communicating with the
%% different scms that host the code associated with different projects. Examples
%% of scms that could be used are local, github, and bitbucket.
%%
%% These modules exist because there are requests made to the VCS by the system
%% where the actual implementation will differ based on the scm behind that VCS.
%% For example, getting diffstats for a git commit will differ between the local
%% git server on the Delivery Server and Github.
-module(deliv_scm).
-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

%% @doc Inspect the scm to get details about the specified Patchset. This includes
%% a list of changed files, diffstats for those files, and a list of commits.
-callback patchset_metadata_ejson(Scope :: d_common_scope(),
                                  Patchset :: d_patchset()) -> {ok, {json(), json(), json()}} |
                                                               {error, term()}.

%% @doc Return the URL that the builders should use to clone the project.
-callback clone_url(Username :: iodata(),
                    Patchset :: d_patchset(),
                    Scope :: d_common_scope()) -> binary().

%% @doc Return the FIPS URL that the builders should use to clone the project in FIPS mode.
-callback clone_url(Username :: iodata(),
                    Patchset :: d_patchset(),
                    Scope :: d_common_scope(),
                    FipsMode :: boolean()) -> binary().

%% @doc Return the project config for project at the given patchset.
-callback load_config_for_patchset(Patchset :: d_patchset(),
                                   Scope :: d_common_scope()) -> {ok, tuple()} |
                                                                 {error, invalid_config | atom() | tuple()}.

%% @doc Return the branch name that builders should use
-callback patchset_branch(Change :: d_change(),
                          Patchset :: d_patchset(),
                          Scope :: d_common_scope()) -> binary().

%% @doc Handles merging new code into mainline
-callback merge_feature_branch(Scope :: d_common_scope(),
                               Patchset :: d_patchset(),
                               Approver :: d_user()) -> {ok, term()} |
                                                        {error, _Why}.

%% @doc Delete the feature branch associated with a change
-callback delete_feature_branch(Scope :: d_common_scope(),
                                Patchset :: d_patchset()) -> {ok, term()} |
                                                             {error, delete_branch_errors()}.

%% @doc Process newly created patchset
-callback process_new_patchset(Patchset :: d_patchset(),
                               ProjDir :: binary(),
                               PipeName :: binary(),
                               FeatBranchName :: binary(),
                               RawBranchName :: binary(),
                               Coords :: #proj_coordinates{}) -> {ok, binary()} |
                                                                 {error, binary()}.

%% @doc Returns title text for a link to a pull request in the external SCM.
-callback pull_request_description(PRID :: non_neg_integer()) -> binary().
