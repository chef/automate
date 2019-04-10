%% @doc A few helper functions for creating comments
-module(ct_comment).

-include_lib("eunit/include/eunit.hrl").

-include_lib("delivery/include/deliv_types.hrl").

-export([
         new_random_line_comment/3,
         new_random_patchset_comment/2,
         new_random_comment_comment/2,
         new_random_comment_comment/3,
         generate_comment/2,
         assert_equal/2,
         assert_equal/3,
         to_json/1,
         from_json/3
        ]).

-spec new_random_line_comment(non_neg_integer(), non_neg_integer(), binary())
        -> {CommentId :: non_neg_integer(), d_line_comment()} | not_created.
new_random_line_comment(UserId, PatchSetId, FilePath) ->
    new_random_comment(line, [UserId, PatchSetId, FilePath]).

-spec new_random_patchset_comment(non_neg_integer(), non_neg_integer())
        -> {CommentId :: non_neg_integer(), d_patchset_comment()} | not_created.
new_random_patchset_comment(UserId, PatchSetId) ->
    new_random_comment(patchset, [UserId, PatchSetId]).

-spec new_random_comment_comment(non_neg_integer(), non_neg_integer())
        -> {CommentId :: non_neg_integer(), d_patchset_comment()} | not_created.
new_random_comment_comment(UserId, TargetId) ->
    new_random_comment(comment, [UserId, TargetId]).

-spec new_random_comment_comment(non_neg_integer(), non_neg_integer(), non_neg_integer())
        -> {CommentId :: non_neg_integer(), d_patchset_comment()} | not_created.
new_random_comment_comment(UserId, TargetId, PatchSetId) ->
    new_random_comment(comment, [UserId, TargetId, PatchSetId]).

%% @private
new_random_comment(Type, Args) ->
    case deliv_comment:insert(generate_comment(Type, Args)) of
        {error, _Why} -> not_created;
        [Comment] -> {deliv_comment:getval(id, Comment), Comment}
    end.

%% @doc Generates a new comment, but does not save it in the DB
-spec generate_comment(atom(), [term()]) -> d_comment().
generate_comment(Type, [UserId | OtherArgs]) ->
    Data = generate_generic_comment_data(Type, UserId)
            ++ generate_specific_comment_data(Type, OtherArgs),
    deliv_comment:fromlist(Data).

%% @private
generate_generic_comment_data(Type, UserId) ->
    [{submitter_id, UserId},
     {content, chef_utils:random_string(4)},
     {status, random_status()},
     {type, chef_utils:to_bin(Type)}].

%% @private
generate_specific_comment_data(line, [PatchSetId, FilePath]) ->
    [{patchset_id, PatchSetId},
     {file_path, FilePath},
     {line_range, random_range()}];
generate_specific_comment_data(patchset, [PatchSetId]) ->
    [{patchset_id, PatchSetId}];
generate_specific_comment_data(comment, [TargetId]) ->
    {ok, TargetComment} = deliv_comment:fetch(TargetId),
    PatchSetId = deliv_comment:getval(patchset_id, TargetComment),
    generate_specific_comment_data(comment, [TargetId, PatchSetId]);
generate_specific_comment_data(comment, [TargetId, PatchSetId]) ->
    [{patchset_id, PatchSetId},
     {parent_id, TargetId}].

%% @private
random_status() ->
    case random:uniform(2) of
        1 -> <<"draft">>;
        2 -> <<"published">>
    end.

%% @private
random_range() ->
    Beginning = random:uniform(200),
    Ending = Beginning + random:uniform(20),
    {Beginning, Ending}.

%% @doc The way we retrieve the forests of comments, the records for comment comments
%% end up inheriting fields from their root's tree that are not in the DB for them
%% That's not an issue in general, but becomes one for tests, where the records
%% fetched from the DB directly will have less fields defined than the ones fetched
%% as part of a forest
%% Also, we ignore the `last_modif_or_publication_timestamp' field for unpublished
%% comments, as this field will change on every insert
-spec assert_equal(d_comment() | {ok, d_comment()}, d_comment() | {ok, d_comment()}) -> _.
assert_equal(Expected, Actual) ->
    assert_equal(Expected, Actual, []).

%% @doc Same as `assert_equal/2', but you can give additional fields to ignore
-spec assert_equal(d_comment() | {ok, d_comment()}, d_comment() | {ok, d_comment()}, [atom()]) -> _.
assert_equal({ok, Expected}, {ok, Actual}, AdditionalFieldsToIgnore) ->
    assert_equal(Expected, Actual, AdditionalFieldsToIgnore);
assert_equal(Expected, Actual, AdditionalFieldsToIgnore) ->
    C1 = filter_fields(Expected, AdditionalFieldsToIgnore),
    C2 = filter_fields(Actual, AdditionalFieldsToIgnore),
    ?assertEqual(C1, C2).

%% @private
%% @doc Filters fields from a comment (i.e. sets them to `undefined')
-spec filter_fields(d_comment(), [atom()]) -> d_comment().
filter_fields(Comment, AdditionalFieldsToIgnore) ->
    DateFields = case deliv_comment:getval(status, Comment) of
        <<"published">> -> [];
        <<"draft">> -> [last_modif_or_publication_timestamp]
    end,
    FieldsToFilter = DateFields ++ AdditionalFieldsToIgnore,
    deliv_comment:setvals([{F, undefined} || F <- FieldsToFilter], Comment).

%% @private
%% @doc Should return true iff the comment is a comment comment
-spec is_comment_comment(d_comment()) -> boolean().
is_comment_comment(Comment) ->
    deliv_comment:getval(parent_id, Comment) =/= undefined.

%% @doc Turns a comment into a JSON object ready to be fed to the front-end
-spec to_json(d_comment()) -> json().
to_json(Comment) ->
    SpecificData = case deliv_comment:getval(type, Comment) of
        <<"comment">> ->
            [{<<"parent_id">>, deliv_comment:getval(parent_id, Comment)}];
        PatchsetOrLine ->
            %% we need to retrieve the sequence number
            PatchsetId = deliv_comment:getval(patchset_id, Comment),
            {ok, Patchset} = deliv_patchset:fetch(PatchsetId),
            SeqNumber = deliv_patchset:getval(sequence_number, Patchset),
            case PatchsetOrLine of
                <<"patchset">> ->
                    [{<<"patchset">>, SeqNumber}];
                <<"line">> ->
                    %% we give both bounds inclusive in the output,
                    %% while the DB has the upper bound exclusive
                    {From, To} = deliv_comment:getval(line_range, Comment),
                    [{<<"patchset">>, SeqNumber},
                     {<<"file">>, deliv_comment:getval(file_path, Comment)},
                     {<<"line_range">>, [From, To - 1]}]
            end
    end,
    TimestampStr = case deliv_comment:getval(last_modif_or_publication_timestamp, Comment) of
        undefined -> undefined;
        Timestamp -> chef_utils:format_timestamp(Timestamp)
    end,
    Data = [{<<"id">>, deliv_comment:getval(id, Comment)},
            {<<"type">>, deliv_comment:getval(type, Comment)},
            {<<"content">>, deliv_comment:getval(content, Comment)},
            {<<"status">>, deliv_comment:getval(status, Comment)},
            {<<"datetime">>, TimestampStr},
            {<<"author">>, extract_relevant_author_info(Comment)} | SpecificData],
    {lists:filter(fun({_K, V}) -> V =/= undefined end, Data)}.

%% @private
-spec extract_relevant_author_info(d_comment()) -> json().
extract_relevant_author_info(Comment) ->
    AuthorId = deliv_comment:getval(submitter_id, Comment),
    {ok, Author} = deliv_user:fetch(AuthorId),
    {[{<<"name">>, deliv_user:getval(name, Author)},
      {<<"first">>, deliv_user:getval(first_name, Author)},
      {<<"last">>, deliv_user:getval(last_name, Author)}]}.

%% @doc Rebuilds the record from the given JSON
%% Obviously somewhat close to some of the code in `deliv_hand_comment',
%% but precisely meant to test that very endpoint
%% Plus, it actually parses everything, included the timestamps and all
-spec from_json(json(), binary(), binary()) -> d_comment().
from_json({PropList}, EntName, ChangeId) ->
    {ok, Data} = deliv_web_utils:translate_proplist(
        PropList,
        fun(<<"line_range">>, [From, ToMinusOne]) ->
            {both, {line_range, {From, ToMinusOne + 1}}};
           (<<"datetime">>, BinDate) ->
            {both, {last_modif_or_publication_timestamp,
                    chef_utils:parse_timestamp(BinDate)}};
           (<<"author">>, AuthorObject) ->
            UserName = ej:get([<<"name">>], AuthorObject),
            {ok, User} = deliv_user:fetch(EntName, UserName),
            %% while we're at it, let's also check that
            %% the other info on the user is good
            LastName = ej:get([<<"last">>], AuthorObject),
            ?assertEqual(deliv_user:getval(last_name, User), null_to_undef(LastName)),
            FirstName = ej:get([<<"first">>], AuthorObject),
            ?assertEqual(deliv_user:getval(first_name, User), null_to_undef(FirstName)),
            UserId = deliv_user:getval(id, User),
            {both, {submitter_id, UserId}};
           (<<"file">>, null) ->
            {both, {file_path, undefined}};
           (<<"file">>, _Value) ->
            {key, file_path};
           (<<"patchset">>, SeqNumber) ->
            %% we need to fetch the patchset ID!
            [PatchSet] = deliv_patchset:fetch(ChangeId, SeqNumber),
            {both, {patchset_id, deliv_patchset:getval(id, PatchSet)}};
           (Key, _) ->
            deliv_web_utils:translate_json_key(Key)
        end
    ),
    Comment = deliv_comment:fromlist(Data),
    %% we also need to retrieve the patchset_id if it's a comment comment
    case is_comment_comment(Comment) of
        true ->
            ParentId = deliv_comment:getval(parent_id, Comment),
            {ok, Parent} = deliv_comment:fetch(ParentId),
            PatchSetId = deliv_comment:getval(patchset_id, Parent),
            deliv_comment:setvals([{patchset_id, PatchSetId}], Comment);
        false ->
            Comment
    end.

null_to_undef(null) -> undefined;
null_to_undef(Other) -> Other.
