-module(deliv_hand_comments_named).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         delete_resource/2,
         from_json/2,
         init/3,
         resource_exists/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"DELETE">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

resource_exists(Req, State) ->
    {[CommentId], Req1} = deliv_web_utils:extract_bindings([comment_id], Req),
    case deliv_comment:fetch(list_to_integer(binary_to_list(CommentId))) of
        {error, not_found} ->
            {false, Req1, State};
        {ok, Comment} ->
            {true, Req1, Comment}
    end.

delete_resource(Req, Comment) ->
    CommentId = deliv_comment:getval(id, Comment),
    case deliv_comment:delete(CommentId) of
        ok ->
            {true, Req, []};
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, Comment)
    end.

from_json(Req, Comment) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, update_comment), Comment).

handle_parse({{error, _Why} = Error, Req}, Comment) ->
    chef_log:failed_call(deliv_web_utils, parse_json_req, [Req, update_comment], Error),
    deliv_web_utils:error_response(400, bad_request, Req, Comment);
handle_parse({Json, Req}, Comment) ->
    NewContent = ej:get({"content"}, Json),
    UpdatedComment = deliv_comment:setvals([{content, NewContent}], Comment),
    case deliv_comment:update_content(UpdatedComment) of
        {ok, UpdatedCommentRec} ->
            {true, Req, UpdatedCommentRec};
        {error, _Why} = Error ->
            chef_log:failed_call(deliv_comment, update, [Req, update_comment], Error),
            deliv_web_utils:error_response(500, internal_server_error, Req, UpdatedComment)
    end.
