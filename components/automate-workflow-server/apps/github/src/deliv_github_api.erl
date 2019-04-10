%% @doc Interface for Github functionality including API calls and errata.
-module(deliv_github_api).

-include("deliv_github_user_messages.hrl").
-include_lib("delivery/include/deliv_types.hrl").

%% Github API Interface
-export([
         add_label_to_issue/3,
         delete_label_from_issue/3,
         create_commit_comment/3,
         create_issue_comment/3,
         delete_branch/2,
         get_file_contents/3,
         merge_pull_request/4,
         set_pull_request_status/5,
         set_pull_request_status/6,
         update_commit_comment/3
        ]).

%% Github integration setup
-export([
         get_token_for_enterprise/2
        ]).

%% @doc Add a label to a Github Issue for the specified project.
-spec add_label_to_issue(d_common_scope(), non_neg_integer(), binary()) -> ok |
                                                                           {error, term()}.
add_label_to_issue(Scope, IssueId, Label) ->
    Route = ["/issues/", erlang:integer_to_list(IssueId), "/labels"],
    Body = [Label],
    case deliv_github_client:req(Scope, post, Route, Body) of
        {ok, 200, _Headers, _ResponseBody} -> ok;
        {ok, _ErrorCode, _Headers, _ResponseBody} -> {error, failed_to_add_label};
        {error, _Why} = Err -> Err
    end.

%% @doc Delete a label to a Github Issue for the specified project.
-spec delete_label_from_issue(d_common_scope(), non_neg_integer(), binary()) -> ok |
                                                                                {error, term()}.
delete_label_from_issue(Scope, IssueId, Label) ->
    Route = ["/issues/", erlang:integer_to_list(IssueId), "/labels/",
             deliv_web_utils:encode_url(Label)],
    case deliv_github_client:req(Scope, delete, Route) of
        {ok, 204, _Headers, _ResponseBody} -> ok;
        {ok, _ErrorCode, _Headers, _ResponseBody} -> {error, failed_to_delete_label};
        {error, _Why} = Err -> Err
    end.

%% @doc Get the file contents of the specified file from the Github API
-spec get_file_contents(d_common_scope(), binary(), binary()) -> {ok, binary()} |
                                                                 {error, term()}.
get_file_contents(Scope, FilePath, Sha) ->
    Route = [<<"/contents/">>, FilePath, <<"?ref=">>, Sha],
    [Ent, Org, Proj, Pipe] = deliv_scopes:'#get'(scoping_names, Scope),
    case deliv_github_client:req(Scope, get, Route) of
        {ok, 200, _Headers, ResponseBody} ->
            Contents = base64:decode(ej:get([<<"content">>], chef_json:decode(ResponseBody))),
            {ok, Contents};
        {ok, ErrorCode, _Headers, _ResponseBody} ->
            chef_log:error("Could not load the file ~s for "
                            "~s/~s/~s/~s at ~s and sha ~s : HTTP Reponse Code ~b",
                            [FilePath, Ent, Org, Proj, Pipe, Route, Sha, ErrorCode]),
            {error, config_not_found};
        {error, Why} = Error ->
            chef_log:error("Could not load the file ~s for "
                            "~s/~s/~s/~s at ~s and sha ~s : ~p",
                            [FilePath, Ent, Org, Proj, Pipe, Route, Sha, Why]),
            Error
    end.

%% @doc Start the process for requesting a token from Github
-spec get_token_for_enterprise(binary(), binary()) -> {ok | error, string()}.
get_token_for_enterprise(OauthAppName, EntName) ->
    case deliv_enterprise:fetch(EntName) of
        {ok, Enterprise} ->
            EntId = deliv_enterprise:getval(id, Enterprise),
            init_and_return_authorize_url(OauthAppName, EntId, EntName);
        {error, Why} ->
            chef_log:failed_call(?MODULE, get_token_for_enterprise, [OauthAppName, EntName], Why),
            {error, chef_utils:iodata_to_str(?GITHUB_ENT_EXIST_MSG(EntName))}
    end.


%% @private
init_and_return_authorize_url(OauthAppName, EntId, EntName) ->
    case deliv_oauth_token:initialize(OauthAppName, enterprise, EntId) of
        {ok, TokenRec} ->
            {ok, deliv_oauth_token:authorize_url(TokenRec)};
        {error, conflict} ->
            {error, chef_utils:iodata_to_str(?GITHUB_TOKEN_EXIST_MSG(EntName))};
        {error, not_found} ->
            {error, chef_utils:iodata_to_str(?GITHUB_OAUTH_APP_EXIST_MSG(OauthAppName))};
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, init_and_return_authorize_url, [OauthAppName, enterprise, EntId], Why),
            Err
    end.

%% @doc Merge a github managed Pull Request
-spec merge_pull_request(d_common_scope(), integer(), binary(), binary()) -> {ok, string()} |
                                                                             {error, term()}.
merge_pull_request(Scope, PullRequestNumber, Sha, MergeMessage) ->
    Route = ["/pulls/", chef_utils:to_str(PullRequestNumber), "/merge"],
    Body = {[{<<"commit_message">>, MergeMessage}, {<<"sha">>, Sha}]},
    case deliv_github_client:req(Scope, put, Route, Body) of
        {ok, 200, _Headers, ResponseBody} ->
            Json = chef_json:decode(ResponseBody),
            { ok, ej:get({<<"sha">>}, Json)};
        %% TODO(jmink) better error handling
        {ok, ErrorCode, _Headers, ResponseBody} ->
            chef_log:error("Failed to merge ~p. Got ~p : ~p", [Route, ErrorCode, ResponseBody]),
            {error, failed_to_merge_branch};
        {error, Why} = Err ->
            chef_log:error("Failed to merge ~p. Error: ~p", [Route, Why]),
            Err
    end.

%% @doc Delete the specified branch on Github
-spec delete_branch(d_common_scope(), binary()) -> ok | {error, term()}.
delete_branch(Scope, Branch) ->
    Route = ["/git/refs/heads/", Branch],
    case deliv_github_client:req(Scope, delete, Route, {[]}) of
        {ok, 204, _Headers, _ResponseBody} -> ok;
        {ok, ErrorCode, _Headers, ResponseBody} ->
            chef_log:error("Failed to delete branch ~p on github for change ~p. Got: ~p : ~p",
                            [Branch, deliv_scopes:'#get'(change_id, Scope), ErrorCode, ResponseBody]),
            {error, github_branch_delete_failed};
        {error, Why} = Err ->
            chef_log:error("Failed to delete branch ~p. Error: ~p", [Branch, Why]),
            Err
    end.

%% @doc Sets the status of the Pull Request in Github
%%      State, Description, and TargetUrl are passed straight through to the github statuses api
%%      (https://developer.github.com/v3/repos/statuses/)
-spec set_pull_request_status(d_common_scope(), binary(), atom(), binary(), binary()) -> ok.
set_pull_request_status(Scope, Sha, State, Description, TargetUrl) ->
    set_pull_request_status(Scope, Sha, State, Description,
                            TargetUrl, <<"chef_delivery">>).

-spec set_pull_request_status(d_common_scope(), binary(), atom(), binary(), binary(), binary()) -> ok | {error, atom() | _Why}.
set_pull_request_status(Scope, Sha, State, Description, TargetUrl, Context) ->
    Route = ["/statuses/", Sha],
    Body = {[{<<"state">>, chef_utils:to_bin(State)},
             {<<"target_url">>, TargetUrl},
             {<<"description">>, Description},
             {<<"context">>, Context}]},
    case deliv_github_client:req(Scope, post, Route, Body) of
        {ok, 201, _Headers, _ResponeBody} -> ok;
        {ok, ErrorCode, _Headers, ResponseBody} ->
            chef_log:error("Failed to post status ~p to github for change ~p. Got: ~p : ~p",
                            [State, deliv_scopes:'#get'(change_id, Scope), ErrorCode, ResponseBody]),
            {error, status_update_failed};
        {error, Why} = Err ->
            chef_log:error("Failed to post status ~p.  Error: ~p", [State, Why]),
            Err
    end.

%% @doc Create a comment on a specific commit in github.
-spec create_commit_comment(d_common_scope(), binary(), binary()) -> {ok, binary()} | {error, atom()} | {error, binary()}.
create_commit_comment(Scope, Sha, Comment) ->
    Route = ["/commits/", Sha, "/comments"],
    Body = {[{<<"body">>, Comment}]},
    case deliv_github_client:req(Scope, post, Route, Body) of
        {ok, 201, _Headers, ResponeBody} -> {ok, ResponeBody};
        {ok, ErrorCode, _Headers, ResponseBody} ->
            chef_log:error("Failed to create comment. Got: ~p : ~p", [ErrorCode, ResponseBody]),
            {error, create_commit_comment_failed};
        {error, Why} = Err ->
            chef_log:error("Failed to create comment. Error: ~p", [Why]),
            Err
    end.
%% @doc Create a comment on a specific commit in github.
-spec create_issue_comment(d_common_scope(), binary() | integer(), binary()) -> {ok, binary()} | {error, atom()} | {error, binary()}.
create_issue_comment(Scope, IssueId, Comment) ->
    Route = ["/issues/", chef_utils:to_bin(IssueId), "/comments"],
    Body = {[{<<"body">>, Comment}]},
    case deliv_github_client:req(Scope, post, Route, Body) of
        {ok, 201, _Headers, ResponseBody} -> {ok, ResponseBody};
        {ok, ErrorCode, _Headers, ResponseBody} ->
            chef_log:error("Failed to create comment. Got: ~p : ~p", [ErrorCode, ResponseBody]),
            {error, create_commit_comment_failed};
        {error, Why} = Err ->
            chef_log:error("Failed to create comment. Error: ~p", [Why]),
            Err
    end.

%% @doc Update a specific commit comment in github.
-spec update_commit_comment(d_common_scope(), binary(), binary()) -> {ok, binary()} | {error, atom()} | {error, binary()}.
update_commit_comment(Scope, Id, Comment) ->
    Route = ["/comments/", Id],
    Body = {[{<<"body">>, Comment}]},
    case deliv_github_client:req(Scope, patch, Route, Body) of
        {ok, 200, _Headers, ResponseBody} -> {ok, ResponseBody};
        {ok, ErrorCode, _Headers, ResponseBody} ->
            chef_log:error("Failed to update comment. Got: ~p : ~p", [ErrorCode, ResponseBody]),
            {error, update_commit_comment_failed};
        {error, Why} = Err ->
            chef_log:error("Failed to update comment. Error: ~p", [Why]),
            Err
    end.
