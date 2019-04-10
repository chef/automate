-module(deliv_comment_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE, setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc).

teardown(_) ->
    application:stop(gproc),
    error_logger:tty(true).

post_or_put_comment_returns_json_and_publishes_event_with_comment() ->
    EntName = <<"HankCo">>,
    UserName = <<"Sgt Hatred">>,
    FirstName = <<"Sgt">>,
    LastName = <<"Hatred">>,
    ChangeId = <<"1">>,
    SeqNumber = 1,
    Id = 2,
    PatchsetId = 3,
    Time = {{2016,4,6},{22,38,11.728086}},
    Type = patchset,
    Range = {44,50},
    Status = published,
    Path = null,
    Content = <<"This is a comment">>,
    Comment = deliv_comment:fromlist([{id, Id},
                                      {patchset_id, PatchsetId},
                                      {type, Type},
                                      {status, Status},
                                      {line_range, Range},
                                      {last_modif_or_publication_timestamp, Time},
                                      {file_path, Path}]),
    CommentPropList =
        [{<<"id">>,Id},
        {<<"content">>,Content},
        {<<"type">>,<<"patchset">>},
        {<<"status">>,Status},
        {<<"datetime">>,Time},
        {<<"line_range">>,null},
        {<<"file">>,Path},
        {<<"parent_id">>,null},
        {<<"user_name">>,UserName},
        {<<"user_first_name">>,FirstName},
        {<<"user_last_name">>,LastName},
        {<<"patchset">>, PatchsetId}],
    CommentEvent = {Comment, some_change},
    ExpectedJson = {[
            {<<"id">>,Id},
            {<<"type">>,<<"patchset">>},
            {<<"content">>,Content},
            {<<"status">>,Status},
            {<<"datetime">>, chef_utils:format_timestamp(Time)},
            {<<"author">>,
                {[
                  {<<"name">>, UserName},
                  {<<"first">>, FirstName},
                  {<<"last">>, LastName}
                ]}
            },
            {<<"patchset">>, PatchsetId}]},

    hoax:mock(deliv_db, [
              ?expect(select,
                      ?withArgs([deliv_comment,
                                 post_or_put_comment,
                                 [EntName, UserName, ChangeId, SeqNumber, Id, Type, null, Range, null, Status, Path]]),
                      ?andReturn({ok, [CommentPropList]})),
              ?expect(fetch_by_id,
                      ?withArgs([deliv_comment,
                                 Id]),
                      ?andReturn({ok, Comment}))
              ]),

    hoax:mock(deliv_change,
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, some_change}))),

    hoax:mock(deliv_event,
              ?expect(publish,
                      ?withArgs([comment_created, CommentEvent]))),

    Json = deliv_comment:post_or_put_comment(EntName, UserName, ChangeId, SeqNumber, Comment),

    ?assertEqual(ExpectedJson, Json),
    ?verifyAll.

post_or_put_comment_forwards_error_if_change_is_found_but_post_or_put_errors() ->
    EntName = <<"HankCo">>,
    UserName = <<"Sgt Hatred">>,
    ChangeId = <<"1">>,
    SeqNumber = 1,
    Id = 2,
    PatchsetId = 3,
    Time = {{2016,4,6},{22,38,11.728086}},
    Type = patchset,
    Range = {44,50},
    Status = published,
    Path = null,
    Comment = deliv_comment:fromlist([{id, Id},
                                      {patchset_id, PatchsetId},
                                      {type, Type},
                                      {status, Status},
                                      {line_range, Range},
                                      {last_modif_or_publication_timestamp, Time},
                                      {file_path, Path}]),

    hoax:mock(deliv_db,
              ?expect(select,
                      ?withArgs([deliv_comment,
                                 post_or_put_comment,
                                 [EntName, UserName, ChangeId, SeqNumber, Id, Type, null, Range, null, Status, Path]]),
                      ?andReturn({error, some_failure}))),

    hoax:mock(deliv_change,
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, some_change}))),


    Result = deliv_comment:post_or_put_comment(EntName, UserName, ChangeId, SeqNumber, Comment),

    ?assertEqual({error, some_failure}, Result),
    ?verifyAll.

post_or_put_comment_forwards_error_if_change_is_not_found() ->
    EntName = <<"HankCo">>,
    UserName = <<"Sgt Hatred">>,
    ChangeId = <<"1">>,
    SeqNumber = 1,
    Id = 2,
    PatchsetId = 3,
    Time = {{2016,4,6},{22,38,11.728086}},
    Type = patchset,
    Range = {44,50},
    Status = published,
    Path = null,
    Comment = deliv_comment:fromlist([{id, Id},
                                      {patchset_id, PatchsetId},
                                      {type, Type},
                                      {status, Status},
                                      {line_range, Range},
                                      {last_modif_or_publication_timestamp, Time},
                                      {file_path, Path}]),

    hoax:mock(deliv_change,
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({error, not_found}))),

    Json = deliv_comment:post_or_put_comment(EntName, UserName, ChangeId, SeqNumber, Comment),

    ?assertEqual({error, not_found}, Json),
    ?verifyAll.

post_or_put_comment_forwards_error_when_fetch_comment_fails() ->
    EntName = <<"HankCo">>,
    UserName = <<"Sgt Hatred">>,
    ChangeId = <<"1">>,
    SeqNumber = 1,
    Id = 2,
    PatchsetId = 3,
    Time = {{2016,4,6},{22,38,11.728086}},
    Type = patchset,
    Range = {44,50},
    Status = published,
    Path = null,
    Comment = deliv_comment:fromlist([{id, Id},
                                      {patchset_id, PatchsetId},
                                      {type, Type},
                                      {status, Status},
                                      {line_range, Range},
                                      {last_modif_or_publication_timestamp, Time},
                                      {file_path, Path}]),

    hoax:mock(deliv_db,
              ?expect(select,
                      ?withArgs([deliv_comment,
                                 post_or_put_comment,
                                 [EntName, UserName, ChangeId, SeqNumber, Id, Type, null, Range, null, Status, Path]]),
                      ?andReturn({ok, [[{<<"id">>, Id}]]}))),

    hoax:mock(deliv_change,
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, change}))),

    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([deliv_comment, Id]),
                      ?andReturn({error, something_bad}))),

    Json = deliv_comment:post_or_put_comment(EntName, UserName, ChangeId, SeqNumber, Comment),

    ?assertEqual({error, something_bad}, Json),
    ?verifyAll.
