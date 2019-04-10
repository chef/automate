%% @doc Takes the commands sent as github comments and responds correctly
-module(deliv_github_chatops).

-include_lib("delivery/include/deliv_types.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.

-export([
         handle_build_event_for_change/1,
         handle_comment_created/1
        ]).

-export([
         check_user_permissions/3
        ]).

%% Parse the user's comment and follow any delivery commands if present
-spec handle_comment_created({d_common_scope(),
                              binary(), binary(),
                              binary()}) -> ok | {ok, binary()} |
                                                 {error, binary()}.
handle_comment_created({Scope, IssueNumber, Comment, Author}) ->
    case binary:match(Comment, <<"@delivery">>) of
      nomatch -> ok;
      Found ->
        CommandList = format_comment_command(Comment, Found),
        chef_log:debug("We got a comment created_event in chatops by ~p, Command: ~p",
                       [Author, CommandList]),
        process_command(Scope, IssueNumber, Author, CommandList)
    end.

%% Handle build events which means posting a ready for approve comment.
-spec handle_build_event_for_change(d_common_scope()) -> ok | {ok, binary()} |
                                                              {error, atom()} |
                                                              {error, binary()}.
handle_build_event_for_change(Scope) ->
    ChangeId = deliv_scopes:'#get'(change_id, Scope),
    {ok, Patchset} = deliv_patchset:latest_patchset_for_change(ChangeId),
    LatestStatus = deliv_patchset:getval(status, Patchset),

    case {LatestStatus, deliv_change:validate_merge(ChangeId)} of
        {<<"open">>, ok} ->
            post_ready_to_approve_comment(Scope, Patchset);
        _ ->
            %% Doing nested case statement here to limit the additional db call.
            %% We can probably figure out something better with the validate
            %% trigger function in deliv_change.
            case deliv_change:validate_accept(ChangeId) of
                ok ->
                    post_ready_to_deliver_comment(Scope, Patchset);
                _ ->
                    ok
            end
    end.

%% @private
%% @doc posts a comment to github indicating a change is ready to approve.
post_ready_to_approve_comment(Scope, Patchset) ->
    post_ready_comment(Scope, Patchset, approve).

%% @private
%% @doc posts a comment to github indicating a change is ready to approve.
post_ready_to_deliver_comment(Scope, Patchset) ->
    post_ready_comment(Scope, Patchset, deliver).

%% @private
post_ready_comment(Scope, Patchset, Action) ->
    PatchsetId = deliv_patchset:getval(id, Patchset),
    [GHPatchset] = deliv_github_patchset:fetch_by_patchset_id(PatchsetId),
    GHPayloadJson = chef_json:decode(deliv_github_patchset:getval(payload, GHPatchset)),
    IssueNumber = deliv_github_pull_request:pull_request_number(GHPayloadJson),

    deliv_github_api:create_issue_comment(Scope,
                                          chef_utils:to_bin(IssueNumber),
                                          determine_comment_for_action(Action)).

%% @private
%% Strip off the @delivery command and split the words into an array for easier matching
format_comment_command(Comment, {Pos, Length}) ->
    Start = Pos+Length,
    CommandString = binary:part(Comment, {Start, size(Comment)-Start}),
    [H|CommandListTail] = binary:split(CommandString, <<" ">>, [global]),

    %% If there was a leading space the first entry will be <<>>
    case H of
      <<>> -> CommandListTail;
      Head -> [Head|CommandListTail]
    end.

%% @private
%% Actually do the command The first 3 clauses catch comments we post and short
%% circuits.
%%
%% Matching <<"review'">> so comments within ticks are not executed. This is to
%% prevent our bot executing the approval phase when it comments:
%% "@user run '@delivery approve' to approve that change."
process_command(_Scope, _IssueNumber, _Author, [<<"review'">>|_Tail]) -> ok;
process_command(_Scope, _IssueNumber, _Author, [<<"approve'">>|_Tail]) -> ok;
process_command(_Scope, _IssueNumber, _Author, [<<"deliver'">>|_Tail]) -> ok;
process_command(Scope, IssueNumber, Author, [CommandBinary|_Tail]) ->
    ChangeId = deliv_scopes:'#get'(change_id, Scope),
    Approver = fetch_user(Scope, Author),
    {ok, Command} = command_to_atom(CommandBinary),
    UserPermissionCheck =  check_user_permissions(Scope, Approver, Command),
    ActionResult = run_action_for_command(UserPermissionCheck, Scope, IssueNumber, Approver, Command),
    {Status, Message} = determine_comment(UserPermissionCheck, ChangeId, Author, ActionResult, Command),
    deliv_github_api:create_issue_comment(Scope, IssueNumber, Message),
    {Status, Message}.

%% @private
%% Will verify, merge, or deliver a change for allowed users.
run_action_for_command({error, _} = Error, _Scope, _IssueNumber, _User, _) ->
    Error;
run_action_for_command(forbid, _Scope, _IssueNumber, _User, _) ->
    ignored;
run_action_for_command(allow, Scope, IssueNumber, _User, review) ->
    ChangeId = deliv_scopes:'#get'(change_id, Scope),
    handle_trigger_stage(Scope, IssueNumber,
                         deliv_change:trigger_stage(verify, ChangeId));
run_action_for_command(_, Scope, _IssueNumber, {ok, User}, approve) ->
    ChangeId = deliv_scopes:'#get'(change_id, Scope),
    deliv_change:merge(ChangeId, User);
run_action_for_command(_, Scope, _IssueNumber, {ok, User}, deliver) ->
    ChangeId = deliv_scopes:'#get'(change_id, Scope),
    deliv_change:accept(ChangeId, User);
run_action_for_command(_, _Scope, _IssueNumber, {error, _} = Error, _Command) ->
    Error;
run_action_for_command(_, _Scope, IssueNumber, User, Command) ->
    chef_log:info("Didn't recognize command ~p by ~p for issue: ~p", [Command, User, IssueNumber]),
    {error, unknown_command}.

%% @private
handle_trigger_stage(Scope, IssueNumber, ok) ->
    deliv_github_api:delete_label_from_issue(Scope, IssueNumber,
                                             ?QUARANTINED_LABEL);
handle_trigger_stage(_Scope, _IssueNumber, Error) -> Error.

%% @private
%% Determine user based on username
fetch_user(Scope, Author) ->
   case fetch_by_alias_name(Scope, Author) of
       {ok, Approver} -> {ok, Approver};
       {error, oauth_not_found} ->
            {error, <<"Oauth setup is incorrect.  No link found for this repo">>};
       {error, user_not_found} ->
                    Message = ["Delivery user ", Author, " not found.  Please contact your delivery",
                               " administrator to create an account with that name, or approve the",
                               " commit as a different user"],
                    {error, iolist_to_binary(Message)}
   end.

%% @private
fetch_by_alias_name(Scope, Author) ->
    EntName = deliv_scopes:'#get'(ent_name, Scope),
    case deliv_github_oauth:fetch_app_by_enterprise(EntName) of
        {ok, OauthApp} ->
            deliv_user:fetch_by_alias(OauthApp, Author);
        {error, Why} ->
            chef_log:failed_call(?MODULE, fetch_by_enterprise, [EntName], Why),
            {error, oauth_not_found}
     end.

%% @private
check_user_permissions(_, {ok, _}, unknown) ->
    allow;
check_user_permissions(Scope, {ok, Approver}, Command) ->
    UserName = deliv_user:getval(name, Approver),
    deliv_git:authorized_git_action(Scope, UserName, Command);
check_user_permissions(_Scope, {error, _} = Error, _Command) ->
    Error.
%% @private
command_to_atom(<<"review">>) ->
    {ok, review};
command_to_atom(<<"deliver">>) ->
    {ok, deliver};
command_to_atom(<<"approve">>) ->
    {ok, approve};
command_to_atom(_Command)->
    {ok, unknown}.

%% Generate the end user visibile comment to display after an attempted action.
determine_comment({error, _} = Error, _, _, _, _) ->
    Error;
determine_comment(forbid, _, Author, _, Command) ->
    CommandBinary = atom_to_binary(Command, unicode),
    {error, iolist_to_binary(["Github user ", Author, " does not have permissions to execute action ", CommandBinary, "."])};
determine_comment(allow, ChangeId, Author, ok, Command) ->
    PastTense = action_to_past_tense(Command),
    {ok, <<"Change: ", ChangeId/binary, " ", PastTense/binary,
           " by: @", Author/binary>>};
determine_comment(_, ChangeId, Author, {error, unknown_command}, Command) ->
    chef_log:debug("~s attempted to ~s change '~s', but delivery doesn't"
                    ++ " understand this command.~n", [Author, Command, ChangeId]),
    CommandBinary = atom_to_binary(Command, unicode),
    {ok, <<"Failed to ", CommandBinary/binary, " change: ", ChangeId/binary, "\n",
              "  Delivery does not understand this command. @", Author/binary>>};
determine_comment(_, ChangeId, Author, {error, invalid_state}, review) ->
    chef_log:debug("~s attempted to review change '~s', but it is in the"
                    ++ " wrong status.~n", [Author, ChangeId]),
    {ok, <<"Failed to review change: ", ChangeId/binary, "\n",
              "  Change must not have been reviewed or be in verify to be",
              " reviewed. @", Author/binary>>};
determine_comment(_, ChangeId, Author, {error, invalid_state}, approve) ->
    chef_log:debug("~s attempted to approve change '~s', but it is in the"
                    ++ " wrong status.~n", [Author, ChangeId]),
    {ok, <<"Failed to approve change: ", ChangeId/binary, "\n",
              "  Change must pass verify before approving. @", Author/binary>>};
determine_comment(_, ChangeId, Author, {error, invalid_state}, deliver) ->
    chef_log:debug("~s attempted to deliver change '~s', but it is in the"
                    ++ " wrong status.~n", [Author, ChangeId]),
    {ok, <<"Failed to deliver change: ", ChangeId/binary, "\n",
              "  Change must pass acceptance before delivering. @",
              Author/binary>>};
determine_comment(_, ChangeId, Author,
                  {error, patchset_already_merged}, approve) ->
    chef_log:debug("~s attempted to approve change '~s', but it is already"
                    ++ " merged.~n", [Author, ChangeId]),
    {ok, <<"Failed to approve change: ", ChangeId/binary, "\n",
      "  Change is already merged. @", Author/binary>>};
determine_comment(_, ChangeId, Author, {error, not_found}, deliver) ->
    chef_log:debug("~s attempted to deliver change '~s', but it is in the"
                    ++ " wrong status.~n", [Author, ChangeId]),
    {ok, <<"Failed to deliver change: ", ChangeId/binary, "\n",
              "  Change must pass acceptance before delivering. @",
              Author/binary>>};
determine_comment(_, ChangeId, Author,
                  {error, changeset_already_accepted}, deliver) ->
    chef_log:debug("~s attempted to deliver change '~s', but it is already"
                    ++ " delivered.~n", [Author, ChangeId]),
    {ok, <<"Failed to deliver change: ", ChangeId/binary, "\n",
      "  Change is already delivered. @", Author/binary>>};
determine_comment(_, ChangeId, Author, Error, Command) ->
    chef_log:debug("~s attempted to ~s change '~s', but got error: ~p~n",
                    [Author, Command, ChangeId, Error]),
    CommandBinary = atom_to_binary(Command, unicode),
    {ok, <<"Failed to ", CommandBinary/binary, " change: ",
              ChangeId/binary, " @", Author/binary>>}.

%% @private
action_to_past_tense(deliver) ->
    <<"delivered">>;
action_to_past_tense(review) ->
    <<"reviewed">>;
action_to_past_tense(approve) ->
    <<"approved">>.

%% @private
%% Determine comment when change is ready to approve or deliver
determine_comment_for_action(approve) ->
    <<"This PR has passed 'Verify' and is ready for review and approval!\n"
      "  Use: '@delivery approve' when code review is complete.">>;
determine_comment_for_action(deliver) ->
    <<"This PR has passed 'Acceptance' and is ready to be delivered!\n"
      "  Use: '@delivery deliver' when validated in acceptance.">>.
