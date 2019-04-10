%% @doc Convenience API for finding data in Github Pull Request JSON
-module(deliv_github_pull_request).

%% Pull values from payload
-export([
         user_name/1,
         pipe_name/1,
         feature_branch/1,
         commit_sha/1,
         change_title/1,
         change_desc/1,
         merge_sha/1,
         merged/1,
         repo_name/1,
         repo_owner/1,
         pull_request_number/1,
         forked/1
        ]).

%% Actions
-export([
         quarantine/2,
         add_label/3
        ]).

-include_lib("delivery/include/deliv_types.hrl").

%% @doc Github user name of pull request initiator
-spec user_name(github_pull_request()) -> binary().
user_name(Payload) ->
    ej:get([<<"pull_request">>, <<"user">>, <<"login">>], Payload).

%% @doc Pipeline name - target branch into which to merge
-spec pipe_name(github_pull_request()) -> binary().
pipe_name(Payload) ->
    ej:get([<<"pull_request">>, <<"base">>, <<"ref">>], Payload).

%% @doc Name of the feature branch containing the change
-spec feature_branch(github_pull_request()) -> binary().
feature_branch(Payload) ->
    ej:get([<<"pull_request">>, <<"head">>, <<"ref">>], Payload).

%% @doc Commit SHA representing tip of the feature branch
-spec commit_sha(github_pull_request()) -> binary().
commit_sha(Payload) ->
    ej:get([<<"pull_request">>, <<"head">>, <<"sha">>], Payload).

%% @doc Pull request title as it appears on Github
-spec change_title(github_pull_request()) -> binary().
change_title(Payload) ->
    ej:get([<<"pull_request">>, <<"title">>], Payload).

%% @doc Pull request description as it appears on Github
-spec change_desc(github_pull_request()) -> binary().
change_desc(Payload) ->
    ej:get([<<"pull_request">>, <<"body">>], Payload).

%% @doc Merge sha for closed pull requests
-spec merge_sha(github_pull_request()) -> binary().
merge_sha(Payload) ->
    ej:get([<<"pull_request">>, <<"merge_commit_sha">>], Payload).

%% @doc Is the pull request merged
-spec merged(github_pull_request()) -> boolean().
merged(Payload) ->
    ej:get([<<"pull_request">>, <<"merged">>], Payload).

%% @doc The repository name on github
-spec repo_name(github_pull_request()) -> binary().
repo_name(Payload) ->
    ej:get([<<"pull_request">>, <<"head">>, <<"repo">>, <<"name">>], Payload).

%% @doc The username of the owner of the repository
-spec repo_owner(github_pull_request()) -> binary().
repo_owner(Payload) ->
    ej:get([<<"pull_request">>, <<"head">>, <<"repo">>, <<"owner">>, <<"login">>], Payload).

%% @doc The pullrequest number
-spec pull_request_number(github_pull_request()) -> integer().
pull_request_number(Payload) ->
    ej:get([<<"pull_request">>, <<"number">>], Payload).

%% @doc The pullrequest number whether or not the project submitting a request is forked
-spec forked(github_pull_request()) -> true | false.
forked(Payload) ->
  ej:get([<<"pull_request">>, <<"head">>, <<"repo">>, <<"fork">>], Payload).

%% @doc When a pull request is opened on Github, sometimes we want to prevent that
%% pull request from immediately being turned into a change. Othertimes, especially
%% for public open source repositories, that pull request could be opened by a
%% user not in the Delivery system. This is where the concecpt of "Quarantine"
%% comes in.
%%
%% Quarantine is a label applied to a Pull Request to idenitfy it as something
%% that needs further review by an authorized user. Currently the only time we
%% are quarantining something is when the user creating the pull request does
%% not exist in the system.
%%
%% In the future there will be a chatops operation to unquarantine that pull
%% request and allow it to flow through the system.
-spec quarantine(json(), d_common_scope()) -> {ok, binary()} | {error, atom()} | {error, binary()}.
quarantine(Payload, Scope) ->
    IssueId = pull_request_number(Payload),
    Comment = <<"Hi. I'm the Delivery bot. Thanks for the pull request! I've alerted "
                "the maintainers of this project so that they can review the change.">>,
    add_label(?QUARANTINED_LABEL, Payload, Scope),
    deliv_github_api:create_issue_comment(Scope, <<IssueId>>, Comment).

%% @doc Add a label to the specified Pull Request
-spec add_label(binary(), json(), d_common_scope()) -> ok | {error, atom()}.
add_label(Label, Payload, Scope) ->
    IssueId = pull_request_number(Payload),
    Org = repo_owner(Payload),
    Repo = repo_name(Payload),

    case deliv_github_api:add_label_to_issue(Scope, IssueId, Label) of
        {error, Why} = Err ->
            chef_log:warning("Failed to add label \"~s\" to Github PR #~p for ~s/~s with error ~p",
                              [Label, IssueId, Org, Repo, Why]),
            Err;
        ok ->
            chef_log:debug("Added label \"~s\" to Github PR #~p for ~s/~s",
                            [Label, IssueId, Org, Repo]),
            ok
    end.
