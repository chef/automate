-module(deliv_comment).

-include("deliv_types.hrl").

%% DB operations
-export([
        delete/1,
        fetch/1,
        insert/1,
        update/1
       ]).

%% sqerl callbacks
-export([
        '#insert_fields'/0,
        '#update_fields'/0,
        '#statements'/0,
        '#table_name'/0
       ]).

%% others
-export([
        post_or_put_comment/5,
        get_comments/6,
        update_content/1
       ]).

-compile({parse_transform, sqerl_gobot}).

-record(deliv_comment, {
            id             :: db_id(),
            patchset_id    :: db_id(),
            submitter_id   :: db_id(),
            content        :: binary(),
            type           :: d_comment_type(),
            status         :: d_comment_status(),
            last_modif_or_publication_timestamp :: calendar:datetime(),
            line_range     :: d_intrange(),
            file_path      :: binary(),
            parent_id      :: db_id()
         }).

'#insert_fields'() -> [
    patchset_id,
    submitter_id,
    content,
    type,
    status,
    line_range,
    file_path,
    parent_id
].

%% the other fields never need be updated
'#update_fields'() -> [
    content,
    status
].

'#statements'() ->
    [default,

     %% used when POST-ing or PUT-ting comments to the comments endpoint
     {post_or_put_comment,
      "SELECT * "
      "FROM insert_or_update_comment($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"},
     %% used when GET-ting comments from the comments endpoint
     {get_comments,
      "SELECT * "
      "FROM fetch_comments($1, $2, $3, $4, $5, $6)"},
      {update_content,
      "UPDATE comments SET content = $2 WHERE id = $1"}].

'#table_name'() -> "comments".

-spec delete(d_comment() | non_neg_integer())
        -> ok | {error, any()}.
delete(CommentId) when erlang:is_integer(CommentId) ->
    delete(#deliv_comment{id = CommentId});
delete(#deliv_comment{} = Comment) ->
    deliv_db:delete(Comment).

-spec fetch(non_neg_integer()) -> {ok, d_comment()} | {error, not_found | any()}.
fetch(CommentId) ->
    deliv_db:fetch_by_id(?MODULE, CommentId).

-spec insert(proplist(atom(), any()) | d_comment())
        -> [d_comment()] | {error, _Why}.
insert(PropList) when erlang:is_list(PropList)->
    insert(fromlist(PropList));
insert(#deliv_comment{} = Rec) ->
    deliv_db:insert(Rec).

-spec update(d_comment()) -> {error, conflict | any()} |
                             {ok, d_comment()}.
update(Comment) ->
    deliv_db:update(Comment).

update_content(#deliv_comment{id= CommentId, content=Content}) ->
    deliv_db:select(?MODULE, update_content, [CommentId, Content]).

%% @doc Meant to be called by the comment endpoint when POST-ing or PUT-ting
-spec post_or_put_comment(binary(), binary(), binary(), non_neg_integer(), d_comment())
        -> json() | {error, _Why}.
post_or_put_comment(EntName, UserName, ChangeId, SeqNumber, Comment) ->
    ParamsFromComment = [chef_json:undef_to_null(getval(Key, Comment))
                         || Key <- [id, type, content, line_range,
                                    parent_id, status, file_path]],

    NewCommentParams = [EntName, UserName, ChangeId, SeqNumber | ParamsFromComment],

    post_or_put_comment(
        deliv_change:fetch_by_id(ChangeId), NewCommentParams
    ).

%% @private
post_or_put_comment({ok, Change}, NewCommentParams) ->
    handle_upsert(post_or_put_comment(NewCommentParams), Change);
post_or_put_comment({error, _Why} = Error, _NewCommentParams) ->
    Error.

%% @private
post_or_put_comment(Params) ->
    deliv_db:select(?MODULE, post_or_put_comment, Params).

%% @private
handle_upsert({error, _Why} = Error, _Change) ->
    Error;
handle_upsert({ok, [Result]}, Change) ->
    %% the current DB function returns a proplist, rather than the
    %% new comment, so this fetch is necessary to publish the new comment. :(
    %% TODO: update insert_or_update_comment to return actual comment.
    case fetch(proplists:get_value(<<"id">>, Result)) of
        {ok, NewComment} ->
            deliv_event:publish(comment_created, {NewComment, Change}),
            row_to_json(Result);
        {error, _Why} = Error ->
            Error
    end.

%% @doc Meant to be called by the comment endpoint when GET-ting
-spec get_comments(binary(), non_neg_integer(), d_comment_type(), binary(), non_neg_integer(), boolean())
        -> [json()] | {error, _Why}.
get_comments(ChangeId, SeqNumber, Type, FilePath, CommentId, IsCommitMsg) ->
    json_from_query(get_comments, [ChangeId, SeqNumber, Type, FilePath,
                                   CommentId, IsCommitMsg]).

%% @private
%% @doc Meant to be used with the `post_or_put_comment' and
%% `get_comments' queries above that return rows of comments
%% together with their authors
%% This turns the list of such rows as returned by the DB
%% into a list of JSON comments
-spec json_from_query(atom(), [any()]) -> [json()] | {error, _Why}.
json_from_query(Query, Params) ->
    deliv_db:map_query(?MODULE, Query, Params,
                       fun row_to_json/1).

%% @private
-spec row_to_json(proplist(binary(), any())) -> json().
row_to_json(Row) ->
    %% a thing of beauty... :-/
    [{<<"id">>, _} = Id,
     {<<"content">>, _} = Content,
     {<<"type">>, T} = Type,
     {<<"status">>, _} = Status,
     {<<"datetime">>, D},
     {<<"line_range">>, L},
     {<<"file">>, _} = File,
     {<<"parent_id">>, _} = ParentId,
     {<<"user_name">>, UserName},
     {<<"user_first_name">>, UserFirstName},
     {<<"user_last_name">>, UserLastName},
     {<<"patchset">>, _} = Patchset] = Row,
    %% some tedious post processing to do...
    Datetime = {<<"datetime">>, chef_utils:format_timestamp(D)},
    SpecificData = case T of
        <<"patchset">> ->
            [Patchset];
        <<"line">> ->
            %% we give both bounds inclusive in the output,
            %% while the DB has the upper bound exclusive
            {From, To} = L,
            LineRange = {<<"line_range">>, [From, To - 1]},
            [Patchset, File, LineRange];
        <<"comment">> ->
            [ParentId]
    end,
    Author = {<<"author">>, {[{<<"name">>, UserName},
                              {<<"first">>, UserFirstName},
                              {<<"last">>, UserLastName}]}},
    {[Id, Type, Content, Status, Datetime, Author | SpecificData]}.
