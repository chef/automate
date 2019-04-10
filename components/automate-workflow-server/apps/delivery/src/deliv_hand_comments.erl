-module(deliv_hand_comments).
-behaviour(deliv_rest).

%% TODO:
%% - a delete endpoint (with what perms? admin can delete all, and anyone can delete their owns?)
%% - as of now, anyone who can post comments on a project can update any comment on this project:
%%   do we want to restrict that to admins, and allow other users to edit only their own comments?

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         init/3,
         rest_init/2,
         handle/2,
         resource_exists/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

%% Required to properly reply a 201
resource_exists(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Method =/= <<"POST">>, Req2, State}.

handle(Req, State) ->
    case do_handle(Req, State) of
        {ok, Req1, Return} ->
            {Return, Req1, State};
        {error, Req1, {ErrCode, ErrIoList}} ->
            ErrMsg = erlang:iolist_to_binary(ErrIoList),
            deliv_web_utils:error_response(ErrCode, ErrMsg, Req1, State)
    end.

do_handle(Req, #handler{ent_name = EntName} = State) ->
    %% for all 3 methods, we need to check that the given change ID
    %% indeed belongs to the given project
    %% the other consistency checks are done directly in the DB
    %% functions against the change ID only
    %% TODO: this part is shared with `deliv_hand_compare' and
    %% should probably be factorized with other change-level
    %% endpoints!
    {Req1, EntName, OrgName, ProjName, ChangeId} = deliv_web_utils:extract_scoping_names(Req),

    case deliv_change:scoping_names(ChangeId) of
        [EntName, OrgName, ProjName, _PipeName] ->
            {Method, Req2} = cowboy_req:method(Req1),
            do_handle(Method, ChangeId, Req2, State);
        Other ->
            chef_log:debug("Unexpected change's scoping names ~p : ~p VS expected ~p",
                            [ChangeId, Other, {EntName, OrgName, ProjName}]),
            {error, Req1, {404, "change not found"}}
    end.

do_handle(<<"GET">>, ChangeId, Req, _State) ->
    case process_qs_vals(Req) of
        {error, Why} ->
            {error, Req, {400, Why}};
        {ok, Req2, ProcessedQsValues} ->
            SeqNumber = proplists:get_value(<<"patchset">>, ProcessedQsValues),
            Type = proplists:get_value(<<"type">>, ProcessedQsValues),
            FilePath = proplists:get_value(<<"file">>, ProcessedQsValues),
            CommentId = proplists:get_value(<<"id">>, ProcessedQsValues),
            IsCommitMsg = proplists:get_value(<<"commit_msg">>, ProcessedQsValues),
            handle_get_comments(deliv_comment:get_comments(
                                  ChangeId, SeqNumber, Type, FilePath, CommentId, IsCommitMsg), Req2)
    end;
do_handle(PostOrPut, ChangeId, Req, State) ->
    SchemaName = case PostOrPut of
                     <<"POST">> -> comment;
                     <<"PUT">> -> update_comment
                 end,
    handle_extract_comment_record(extract_comment_record(
                                    deliv_web_utils:parse_json_req(Req, SchemaName), PostOrPut),
                                  PostOrPut, ChangeId, State).

%% @private
%% @doc We got some dependency between the QS values, so we need to do
%% a little more work on top of `deliv_web_utils:process_qs_vals'
process_qs_vals(Req) ->
    QsKeys = [{<<"patchset">>,null}, {<<"type">>,null}, {<<"file">>, null},
              {<<"commit_msg">>, <<"false">>}, {<<"id">>, null}],
    case deliv_web_utils:process_qs_vals(Req, fun qs_val/2, QsKeys) of
        {error, _Why} = Error ->
            Error;
        {ok, Req2, ProcessedQsValues} ->
            SeqNumber = proplists:get_value(<<"patchset">>, ProcessedQsValues),
            CommentId = proplists:get_value(<<"id">>, ProcessedQsValues),
            ExactlyOneNull = (CommentId =:= null) =/= (SeqNumber =:= null),
            case ExactlyOneNull of
                true -> {ok, Req2, ProcessedQsValues};
                false -> {error, "exactly one of 'patchset' and 'id' must be defined"}
            end
    end.

qs_val(<<"commit_msg">>, BoolBin) -> process_bool(BoolBin);
qs_val(<<"patchset">>, SeqNumberBin) -> process_int_qs_val(<<"patchset">>, SeqNumberBin);
qs_val(<<"id">>, IdBin) -> process_int_qs_val(<<"id">>, IdBin);
qs_val(<<"type">>, <<"patchset">>) -> <<"patchset">>;
qs_val(<<"type">>, <<"line">>) -> <<"line">>;
qs_val(<<"type">>, null) -> null;
qs_val(<<"type">>, _Other) -> {error, "type parameter must be one of 'patchset' or 'line'"};
qs_val(<<"file">>, File) -> File.

%% TODO: that should probably be somewhere else
process_bool(<<"0">>) -> false;
process_bool(<<"false">>) -> false;
process_bool(<<"f">>) -> false;
process_bool(<<"False">>) -> false;
process_bool(_Other) -> true.

%% TODO: that too
process_int_qs_val(_, null) ->
    null;
process_int_qs_val(Key, IntBin) ->
    case chef_utils:to_int(IntBin) of
        {error, not_an_int} -> {error, [Key, " must be an int"]};
        Int -> Int
    end.

handle_get_comments({error, Why}, Req) ->
    translate_db_error(Why, Req);
handle_get_comments(Json, Req) ->
    {ok, Req, chef_json:encode(Json)}.

translate_db_error(Why, Req) ->
    {error, Req, handle_translate_error(deliv_pg:translate_error(Why))}.

handle_translate_error(unrelated_change_and_patchset) ->
    {400, "this patchset does not belong to the provided change"};
handle_translate_error(forbidden_update_on_published_comment) ->
    {400, "published comments cannot be updated"};
handle_translate_error(unknown_comment_comment_parent_id) ->
    {400, "this parent_id is unknown"};
handle_translate_error(unknown_comment) ->
    {404, "comment not found"};
handle_translate_error(unknown_sequence_number_for_change) ->
    {404, "unknown patchset number"};
handle_translate_error(Other) ->
    chef_log:error("Unexpected error from DB: ~p", [Other]),
    {500, "internal server error"}.

handle_extract_comment_record({error, Req, Why}, _PostOrPut, _ChangeId, _State) ->
    {error, Req, {400, Why}};
handle_extract_comment_record({ok, Req, Comment, SeqNumber}, PostOrPut, ChangeId,
                              #handler{ent_name = EntName, user_name = UserName}) ->
    handle_post_or_put_comment(deliv_comment:post_or_put_comment(
                                 EntName, UserName, ChangeId, SeqNumber, Comment), PostOrPut, Req).

handle_post_or_put_comment({error, Why}, _PostOrPut, Req) ->
    translate_db_error(Why, Req);
handle_post_or_put_comment(Json, <<"POST">>, Req) ->
    Req1 = deliv_web_utils:set_json_body(Json, Req),
    {Req2, EntName, OrgName, ProjName, ChangeId} = deliv_web_utils:extract_scoping_names(Req1),
    %% let's build the location header's link
    CommentId = chef_utils:to_bin(ej:get([<<"id">>], Json)),
    Link = deliv_web_utils:href(EntName, <<"/orgs/", OrgName/binary,
                                           "/projects/", ProjName/binary,
                                           "/changes/", ChangeId/binary,
                                           "/comments?id=", CommentId/binary>>),
    {ok, Req2, {true, Link}};
handle_post_or_put_comment(Json, <<"PUT">>, Req) ->
    Req1 = deliv_web_utils:set_json_body(Json, Req),
    {ok, Req1, true}.

%% @doc That fun is only responsible for extracting the `deliv_comment' record
%% from the request - it doesn't check that it's actually related to the change
%% given in the URL or anything; it simply checks that it's a well-formed input
extract_comment_record({{error, _Why}, Req}, _PostOrPut) ->
    %% TODO: do we want to be more precise as to what the issue is?
    {error, Req, "invalid input"};
extract_comment_record({Json, Req}, <<"POST">>) ->
    comment_from_json(Json, fun translate_prop_list_post/2, Req);
extract_comment_record({Json, Req}, <<"PUT">>) ->
    comment_from_json(Json, fun translate_prop_list_put/1, Req).

comment_from_json({PropList} = Json, TranslatingFun, Req) ->
    case deliv_web_utils:translate_proplist(PropList, TranslatingFun) of
        {error, Why} ->
            {error, Req, Why};
        {ok, TranslatedPropList} ->
            SeqNumber = case ej:get([<<"patchset">>], Json) of
                            undefined -> null;
                            Defined -> Defined
                        end,
            Type = ej:get([<<"type">>], Json),
            {ok, Req, deliv_comment:fromlist([{type, Type} | TranslatedPropList]), SeqNumber}
    end.

%% sadly enough, we can't check that the line_range is valid
%% in the schema, the draft 03 doesn't make that possible :-/
translate_prop_list_post(<<"line_range">>, [From, To]) ->
    case From =< To of
        true ->
            %% to make things simpler on the fronted, line_range
            %% have both bounds inclusive in there, but in the backend
            %% the upper bound is exclusive
            {both, {line_range, {From, To + 1}}};
        false ->
            {error, ["invalid line_range"]}
    end;
translate_prop_list_post(<<"id">>, _Value) ->
    ignore;
translate_prop_list_post(<<"datetime">>, _Value) ->
    ignore;
translate_prop_list_post(<<"author">>, _Value) ->
    ignore;
translate_prop_list_post(<<"patchset">>, _Value) ->
    %% that one is actually the sequence number, and treated elsewhere
    ignore;
translate_prop_list_post(<<"file">>, _Value) ->
    {key, file_path};
translate_prop_list_post(Key, _Value) ->
    deliv_web_utils:translate_json_key(Key).

%% @doc For PUT, we ignore all but id, status and content
translate_prop_list_put(Key) ->
    case lists:member(Key, [<<"id">>, <<"status">>, <<"content">>]) of
        true -> deliv_web_utils:translate_json_key(Key);
        false -> ignore
    end.
