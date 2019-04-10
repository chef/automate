%% @doc Handler for the different events sent to us by Github webhooks.
-module(deliv_github_event).

-include_lib("delivery/include/deliv_types.hrl").
-include("deliv_github_event_types.hrl").

-export([
         handle/1
        ]).

-define(QUARANTINE_USER, <<"untrusted_github_user">>).

%% @doc Based on the combination of Github EventType and Action, perform different
%% actions within the Delivery System.
%%   * pull_request/opened - Create a new Change and Patchset
%%   * pull_request/synchronize - Add a new Patchset to an existing Change
%%
%% TODO: Handle errors from patchset and change - send back to Github
-spec handle(github_input_event()) -> any().
handle(#github_pr{action=Action, payload=Payload, proj_coordinates=Coords} = GithubPR)
  when Action == opened; Action == synchronized ->
    #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName} = Coords,
    Scope = deliv_scopes:from_scoping_names(EntName, OrgName, ProjName),

    prevent_create_change_from_forked_project(deliv_github_pull_request:forked(Payload), Scope, GithubPR);
handle(#github_pr{proj_coordinates=Coords, action=closed}) ->
    #proj_coordinates{ent_name=EntName, org_name=OrgName, proj_name=ProjName} = Coords,
    chef_log:debug("Received pull request closed from Github for ~s/~s/~s", [EntName, OrgName, ProjName]);
handle(#github_comment{payload=Payload, action=created}) ->
    Comment = deliv_github_issue:comment_body(Payload),
    Author = deliv_github_issue:comment_author(Payload),
    IssueNumber = deliv_github_issue:issue_number(Payload),
    case deliv_github_patchset:fetch_latest_by_issue_url(deliv_github_issue:url(Payload)) of
        [GithubPatchset] ->
            ChangeId = deliv_github_patchset:get_change_id_by_patchset(GithubPatchset),
            Scope = deliv_scopes:from_change_id(ChangeId),
            Msg = {Scope, IssueNumber, Comment, Author},
            deliv_event:publish(github_comment_created, Msg);
        [] ->
            chef_log:info("Comment ~p is on a pull request that has not been added to Delivery. The comment event is not being published.", [Comment]),
            ok
        end.

prevent_create_change_from_forked_project(true, Scope, #github_pr{payload=Payload}) ->
    PrNum = deliv_github_pull_request:pull_request_number(Payload),
    Comment = <<"I'm sorry, but this project does not accept pull requests from forked repositories.">>,

    deliv_github_api:create_issue_comment(Scope, PrNum, Comment),
    {error, forked_project};
prevent_create_change_from_forked_project(false, Scope, GithubPR) ->
    handle_create_change(Scope, GithubPR).

handle_create_change(Scope, #github_pr{action=Action, payload=Payload, proj_coordinates=Coords}) ->
    UserName = deliv_github_pull_request:user_name(Payload),
    PrNum = deliv_github_pull_request:pull_request_number(Payload),
    Org = deliv_github_pull_request:repo_owner(Payload),
    Repo = deliv_github_pull_request:repo_name(Payload),
    #proj_coordinates{ent_name = EntName} = Coords,

    %% Query for the Oauth Application
    {ok, OauthApp} = deliv_github_oauth:fetch_app_by_enterprise(EntName),

    chef_log:debug("~p action received for Github Pull Request #~p from ~s/~s", [Action, PrNum, Org, Repo]),
    case deliv_user:fetch_by_alias(OauthApp, UserName) of
        {ok, User} ->
            DeliveryUserName = deliv_user:getval(name, User),
            ChangeId = create_change(Action, Coords, DeliveryUserName, Payload),
            deliv_change:verify_patchset(ChangeId);
        {error, not_found} ->
            chef_log:warning("Unauthorized user ~s attempted to ~p Github Pull Request #~p " ++
                                "from ~s/~s. The PR will be quarantined.",
                            [UserName, Action, PrNum, Org, Repo]),
            create_change(Action, Coords, ?QUARANTINE_USER, Payload),
            deliv_github_pull_request:quarantine(Payload, Scope)
    end.

%% @private
%% @doc Return the event key associated with the Github event action.
-spec event_key(atom()) -> atom().
event_key(opened) -> github_pull_request_opened;
event_key(synchronized) -> github_pull_request_synchronize.

%% @private
%% @doc Take the incoming Pull Request and create the change.
create_change(Action, #proj_coordinates{ent_name = EntName,
                                        org_name = OrgName,
                                        proj_name = ProjName}, UserName, Payload) ->
    PipeName = deliv_github_pull_request:pipe_name(Payload),
    FeatBranchName = deliv_github_pull_request:feature_branch(Payload),
    CommitSha = deliv_github_pull_request:commit_sha(Payload),
    ChangeTitle = deliv_github_pull_request:change_title(Payload),
    ChangeDescription = deliv_github_pull_request:change_desc(Payload),

    %% Create/Update Change
    [Patchset] = deliv_patchset:new(EntName, UserName, OrgName, ProjName,
                                    PipeName, FeatBranchName, CommitSha),
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    Change1 = deliv_change:setvals([{title, ChangeTitle},
                                    {description, ChangeDescription}], Change),

    %% Write the Github payload to the database
    deliv_github_patchset:save(Payload, Patchset),

    %% Event
    %% We should hydrate this event with enough detail to not need as many
    %% db calls in the handlers.
    Msg = {ChangeId, Payload},
    deliv_event:publish(event_key(Action), Msg),
    deliv_event:publish(patchset_created, Patchset),

    deliv_change:update(Change1),
    ChangeId.
