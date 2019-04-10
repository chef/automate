-module(chef_json_tests).

-include_lib("eunit/include/eunit.hrl").

%% We only test the basic stuff here, this is by no means extensive, we trust jiffy's tests to do their job!
basic_test_() ->
    {String, Json} = example_json_and_string(),
    EncodedString = chef_json:encode(Json),
    DecodedJson = chef_json:decode(String),
    [
        %% encoding
        ?_assert(erlang:is_binary(EncodedString)),
        ?_assertEqual(String, EncodedString),
        %% and decoding
        ?_assertEqual(Json, DecodedJson),
        %% returns {error, invalid_json} when decoding an invalid input
        ?_assertEqual({error, invalid_json}, chef_json:decode(<<"i'm not a json!">>))
    ].

undef_to_null_test_() ->
    [
        ?_assertEqual(
            <<"[null,\"wk\",{\"lvl1\":{\"lvl2\":null},"
              "\"list\":[null,42,null]},null]">>,
            chef_json:encode([
                undefined,
                <<"wk">>,
                {[
                    {<<"lvl1">>, {[{<<"lvl2">>, undefined}]}},
                    {<<"list">>, [null, 42, undefined]}
                ]},
                null])
        )
    ].

timestamp_to_bin_test_() ->
    Timestamp = {{2017,4,18},{12,22,23.012345}},
    [
        ?_assertEqual(
            <<"[\"2017-04-18T12:22:23Z\",\"wk\",{\"lvl1\":{\"lvl2\":\"2017-04-18T12:22:23Z\"},"
              "\"list\":[\"2017-04-18T12:22:23Z\",42,\"2017-04-18T12:22:23Z\"]},\"2017-04-18T12:22:23Z\"]">>,
            chef_json:encode([
                Timestamp,
                <<"wk">>,
                {[
                    {<<"lvl1">>, {[{<<"lvl2">>, Timestamp}]}},
                    {<<"list">>, [Timestamp, 42, Timestamp]}
                ]},
                Timestamp])
        )
    ].

validate_json_test() ->
    %% loads from .eunit
    FixturesDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "test/fixtures/schemas"]),
    chef_json:init_schemas(FixturesDir),

    GoodChangePasswd = {[{<<"old_password">>, <<"1st password">>},
                         {<<"new_password">>, <<"2nd password">>}]},
    ?assertEqual(ok, chef_json:validate(example, GoodChangePasswd)),

    BadChangePasswd = {[{<<"new_password">>, <<"2nd password">>}]},
    ?assertMatch({error,[{data_invalid,
                          _,
                          {missing_required_property, <<"old_password">>},
                          _,
                          _}]},
                 chef_json:validate(example, BadChangePasswd)).

example_json_and_string() ->
    {
        <<"{\"bin\":\"wk\",\"bool\":true,\"list\":[42,\"ok\"]}">>,
        {[{<<"bin">>, <<"wk">>}, {<<"bool">>, true}, {<<"list">>, [42, <<"ok">>]}]}
    }.
