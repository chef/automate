-module(notification_slack_content).

%% Forms notification json.

-include("notification_macros.hrl").
-include("notification_types.hrl").

-export([
         acceptance_failed/1,
         acceptance_passed/1,
         change_approved/1,
         change_delivered/1,
         comment_created/1,
         build_failed/1,
         delivered_failed/1,
         delivered_passed/1,
         rehearsal_failed/1,
         union_failed/1,
         verify_passed/1
        ]).

%% additional_fields is a list of tuples {<<"FieldTitle">>, field_value_key}
%% where <<"FieldTitle">> is the title text and field_value_key is the
%% name of the field in the change record to display.
%%
%% Example: [{<<"Approved By", approved_by}] will add
%%    ```
%       Approved By
%%      oliver
%%    ```
%% to the notification message, if the change was approved by a user named oliver.
-record(notification_metadata, {change                 :: d_change(),
                                title                  :: binary(),
                                text = <<"">>          :: binary(),
                                color = grey           :: green | grey | red,
                                url_scope              :: url_scope(),
                                additional_fields = [] :: [{binary(), atom()}]
                               }).

-spec acceptance_passed(d_change()) -> json().
acceptance_passed(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Acceptance Passed.">>,
                                             text = <<"Change is ready for delivery.">>,
                                             color = green,
                                             url_scope = summary}).

-spec acceptance_failed(d_change()) -> json().
acceptance_failed(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Change failed at the Acceptance stage.">>,
                                             color = red,
                                             url_scope = status}).

-spec change_approved(d_change()) -> json().
change_approved(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Change Approved!">>,
                                             url_scope = status,
                                             additional_fields = [{<<"Approved by:">>, approved_by}]}).

-spec verify_passed(d_change()) -> json().
verify_passed(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Verify Passed.">>,
                                             text = <<"Change is ready for review.">>,
                                             color = green,
                                             url_scope = review}).

-spec change_delivered(d_change()) -> json().
change_delivered(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Change Delivered!">>,
                                             url_scope = status,
                                             additional_fields = [{<<"Delivered by:">>, delivered_by}]}).

-spec rehearsal_failed(d_change()) -> json().
rehearsal_failed(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Change failed at the Rehearsal stage.">>,
                                             color = red,
                                             url_scope = status}).

-spec union_failed(d_change()) -> json().
union_failed(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Change failed at the Union stage.">>,
                                             color = red,
                                             url_scope = status}).

 -spec delivered_passed(d_change()) -> json().
 delivered_passed(Change) ->
     notification_json(#notification_metadata{change = Change,
                                              title = <<"Delivered stage has completed for this change.">>,
                                              color = green,
                                              url_scope = status}).

-spec delivered_failed(d_change()) -> json().
 delivered_failed(Change) ->
     notification_json(#notification_metadata{change = Change,
                                              title = <<"Change failed at the Delivered stage.">>,
                                              color = red,
                                              url_scope = status}).

-spec comment_created({d_change(), d_comment()}) -> json().
comment_created({Change, Comment}) ->
    Content = deliv_comment:getval(content, Comment),
    CommenterId = deliv_comment:getval(submitter_id, Comment),

    case deliv_user:fetch(CommenterId) of
        {ok, Commenter} ->
            CommenterName = deliv_user:getval(name, Commenter),
            AdditionalFields = [{<<"New comment:">>, Content}, {<<"Comment from:">>, CommenterName}],

            Ejson = notification_json(#notification_metadata{change = Change,
                                                             title = <<"">>,
                                                             url_scope = review,
                                                             additional_fields = AdditionalFields}),

            %% add thumb_url if commenter has an email address.
            Ejson1 = handle_commenter_email(Ejson, deliv_user:getval(email, Commenter)),

            %% turn on markdown for fields.
            ej:set({"attachments", first, "mrkdwn_in"}, Ejson1, [<<"fields">>]);
        {error, Why} = Error ->
            chef_log:failed_call(deliv_user, fetch, [CommenterId], Why),
            Error
    end.

-spec build_failed(d_change()) -> json().
build_failed(Change) ->
    notification_json(#notification_metadata{change = Change,
                                             title = <<"Change failed at the Build stage.">>,
                                             color = red,
                                             url_scope = status}).

%% Private

notification_json(#notification_metadata{change = Change,
                                         title = Title,
                                         text = Text,
                                         color = Color,
                                         url_scope = URLScope,
                                         additional_fields = AdditionalFields}) ->

    ChangeId = deliv_change:getval(id, Change),
    ChangeTitle = escape_bin(deliv_change:getval(title, Change)),

    [EntName, OrgName, ProjName | _] = deliv_change:scoping_names(ChangeId),

    %% Create the URL we link to
    ScopedURL = notification_web_utils:scoped_web_url_for_change(EntName, OrgName,
                                                                 ProjName, ChangeId,
                                                                 URLScope),

    NotificationText = notification_text(ScopedURL, ChangeTitle, Title, Text),

    FieldList = build_field_list(Change, ProjName, AdditionalFields),

    {[
        {<<"username">>, <<"Chef Automate">>},
        {<<"icon_url">>, ?CHEF_ICON_URL},
        {<<"text">>, NotificationText},
        {<<"attachments">>, [
            {[
                {<<"fallback">>, NotificationText},
                {<<"color">>, notification_color(Color)},
                {<<"fields">>, FieldList}
            ]}
        ]}
    ]}.

notification_text(ScopedURL, ChangeTitle, <<"">>, <<"">>) ->
    chef_utils:to_bin("<~s|~s>", [ScopedURL, ChangeTitle]);
notification_text(ScopedURL, ChangeTitle, Title, <<"">>) ->
    chef_utils:to_bin("<~s|~s>\n~s", [ScopedURL, ChangeTitle, Title]);
notification_text(ScopedURL, ChangeTitle, Title, Text) ->
    chef_utils:to_bin("<~s|~s>\n~s", [ScopedURL, ChangeTitle, chef_utils:join_binaries([Title, Text], <<" ">>)]).


notification_color(green) -> <<"#58B957">>;
notification_color(grey) -> <<"#E3E4E6">>;
notification_color(red) -> <<"#D00000">>.

%% Slack notifications include a list of fields which have the form
%%    ```
%%      Title
%%      value
%%    ```
%% If the event is a comment, we need to handle the fields slightly differently.
build_field_list(Change, ProjName, [{<<"New comment:">>, Content}, {<<"Comment from:">>, CommenterName}]) ->
    [{[
        {<<"title">>, <<"New comment:">>},
        {<<"value">>, Content}
    ]},
    {[
        {<<"title">>, <<"Comment from:">>},
        {<<"value">>, CommenterName}
    ]}] ++ build_field_list(Change, ProjName, []);
%% Project and Submitted By are always the first two fields. Additional fields
%% are appended in the order listed.
build_field_list(Change, ProjName, AdditionalFields) ->
    build_field_list(Change, ProjName, AdditionalFields, []).

build_field_list(Change, ProjName, [], FieldList) ->
    ProjectNameField = {[
        {<<"title">>, <<"Project:">>},
        {<<"value">>, ProjName}
    ]},

    %% This will appear below Project field every time.
    %% Setting short to true even when there are no additional fields
    %% will not affect this positioning.
    SubmittedByField = {[
        {<<"title">>, <<"Change submitted by:">>},
        {<<"value">>, deliv_change:getval(submitted_by, Change)},
        {<<"short">>, true}
    ]},

    [ProjectNameField | [SubmittedByField | FieldList]];
build_field_list(Change, ProjName, [{Title, Key} | RemFields], FieldList) ->
    NewField = {[
        {<<"title">>, Title},
        {<<"value">>, chef_utils:to_bin(deliv_change:getval(Key, Change))},
        {<<"short">>, true}
    ]},

    build_field_list(Change, ProjName, RemFields, [NewField | FieldList]).

%% Slack requires that we escape &, <, and > characters.
%% https://api.slack.com/docs/formatting#how_to_escape_characters
escape_bin(<<>>) ->
    <<"Untitled Change">>;
escape_bin(Binary) ->
    %% Must encode & first, otherwise we encode our encodings.
    EscapeChars = [{<<"&">>, <<"&amp;">>},
                   {<<"<">>, <<"&lt;">>},
                   {<<">">>, <<"&gt;">>},
                   {<<"\n">>, <<" ">>}],

    lists:foldl(
        fun({Pattern, Replacement}, BinaryToEscape) ->
            binary:replace(BinaryToEscape, Pattern, Replacement, [global])
        end,
        Binary,
        EscapeChars
    ).

handle_commenter_email(Ejson, undefined) ->
    Ejson;
handle_commenter_email(Ejson, <<>>) ->
    Ejson;
handle_commenter_email(Ejson, Email) ->
    MD5 = erlang:md5(Email),
    EmailStr = lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= MD5]),
    GravatarURL = chef_utils:to_bin("https://s.gravatar.com/avatar/~s", [EmailStr]),
    ej:set({"attachments", first, "thumb_url"}, Ejson, GravatarURL).
