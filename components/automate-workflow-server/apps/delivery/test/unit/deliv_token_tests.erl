%% Copyright 2014 Opscode, Inc. All Rights Reserved.
-module(deliv_token_tests).

-include_lib("hoax/include/hoax.hrl").

deliv_token_test_() ->
    {
      setup,
      fun()  -> app_test_helpers:ensure_app_running(crypto) end,
      fun(_) -> app_test_helpers:stop_app(crypto) end,
      [
       new_tests(),
       to_candidate_tests(),
       verify_against_candidates_tests(),
       verify_token_tests()
      ]
    }.

new_tests() ->
    Token = deliv_token:new(),
    [
     {"Tokens are binary!",
      ?_assert(erlang:is_binary(Token))},
     {"Tokens are 43 bytes long!",
      ?_assertEqual(43, byte_size(Token))}
    ].

to_candidate_tests() ->
    Token         = deliv_token:new(),

    DbBday        = {{2014,6,19},{16,10,32.123456}},
    ValidBday     = {{2014,6,19},{16,10,32}},

    DbProplist    = [{<<"token">>, Token},
                     {<<"birthday">>, DbBday}],
    ValidProplist = [{<<"token">>, Token},
                     {<<"birthday">>, ValidBday}],
    [
     {"Normalize weird microseconds from the database",
      ?_assertEqual({Token, ValidBday},
                   deliv_token:to_candidate(DbProplist))},
     {"Still works if we ever properly fix the microseconds thing",
      ?_assertEqual({Token, ValidBday},
                    deliv_token:to_candidate(ValidProplist))}
    ].

extract_token_info_returns_values_uri_decoded_test() ->
    hoax:test(fun() ->
        UriEnt = <<"%3F">>,
        UriToken = <<"token%21">>,
        UriUsername = <<"user%40user.com">>,
        hoax:expect(receive
                        cowboy_req:binding(ent_name, req) -> {UriEnt, req1};
                        cowboy_req:header(<<"chef-delivery-user">>, req1) -> {UriUsername, req2};
                        cowboy_req:header(<<"chef-delivery-token">>, req2) -> {UriToken, req3}
                    end),
        ?assertEqual({<<"?">>, <<"user@user.com">>, <<"token!">>, req3}, deliv_token:extract_token_info(req)),
        ?verifyAll
    end).

extract_token_info_returns_values_uri_decoded_when_enterprise_missing_test() ->
    hoax:test(fun() ->
        UriEnt = <<"%3F">>,
        UriToken = <<"token%21">>,
        UriUsername = <<"user%40user.com">>,
        hoax:expect(receive
                        cowboy_req:binding(ent_name, req) -> {undefined, req1};
                        cowboy_req:header(<<"chef-delivery-enterprise">>, req1) -> {UriEnt, req2};
                        cowboy_req:header(<<"chef-delivery-user">>, req2) -> {UriUsername, req3};
                        cowboy_req:header(<<"chef-delivery-token">>, req3) -> {UriToken, req4}
                    end),
        ?assertEqual({<<"?">>, <<"user@user.com">>, <<"token!">>, req4}, deliv_token:extract_token_info(req)),
        ?verifyAll
    end).

extract_undefined_token_info_returns_undefined_test() ->
    hoax:test(fun() ->
        UriEnt = <<"%3F">>,
        hoax:expect(receive
                        cowboy_req:binding(ent_name, req) -> {UriEnt, req1};
                        cowboy_req:header(<<"chef-delivery-user">>, req1) -> {undefined, req2};
                        cowboy_req:cookie(<<"chef-delivery-user">>, req2) -> {undefined, req3};
                        cowboy_req:qs_val(<<"chef-delivery-user">>,req3) -> {undefined, req4};
                        cowboy_req:header(<<"chef-delivery-token">>, req4) -> {undefined, req5};
                        cowboy_req:cookie(<<"chef-delivery-token">>, req5) -> {undefined, req6};
                        cowboy_req:qs_val(<<"chef-delivery-token">>,req6) -> {undefined, req7}
                    end),
        ?assertEqual({<<"?">>, undefined, undefined, req7}, deliv_token:extract_token_info(req)),
        ?verifyAll
    end).

verify_against_candidates_tests() ->
    UserToken   = <<"iSOm7ZPYia5nu83tK47I1tAdV1+hxLk3qU0XeEEisw4=">>,
    OtherToken1 = <<"iSOm5IHmmV2w1b29EwU+E1lPoLtVw789SX2qPogyTa8=">>,
    OtherToken2 = <<"iSOmCp4noEvnlnQX3CwyxKXYkQEVTbX1tVFIDnm+5YI=">>,

    Ttl = 60 * 60 * 24 * 7, %% 1 week
    Now = calendar:now_to_universal_time(os:timestamp()),

    ExpiredBirthday = expired_time(Now),

    TestSpecs = [
                 {"Matches a single valid token", token_ok, [{UserToken, Now}]},
                 {"Matches a valid token among other candidates", token_ok, [{OtherToken1, Now},
                                                                             {OtherToken2, Now},
                                                                             {UserToken,   Now}]},
                 {"An expired token is expired", token_expired, [{OtherToken1, Now},
                                                                 {OtherToken2, Now},
                                                                 {UserToken,   ExpiredBirthday}]},
                 {"No matches = token_denied", token_denied, [{OtherToken1, Now},
                                                              {OtherToken2, Now}]},
                 {"No candidates = token_denied", token_denied, []}
                ],
    [{Label,
      ?_assertEqual(Expected,
                   deliv_token:verify_against_candidates(UserToken, Ttl, Candidates))} ||
        {Label, Expected, Candidates} <- TestSpecs].

verify_token_tests() ->
    EnterpriseName = <<"MonkeyPants, Inc.">>,
    UserToken      = <<"iSOm7ZPYia5nu83tK47I1tAdV1+hxLk3qU0XeEEisw4=">>,

    Ttl = 60 * 60 * 24 * 7, %% 1 week
    Now = calendar:now_to_universal_time(os:timestamp()),


    ExpiredBirthday = expired_time(Now),

    {setup,
     fun() ->
             meck:new(deliv_db, [passthrough]),
             meck:expect(deliv_db, select,
                         fun(deliv_user, candidate_tokens, [_, U, T]) ->
                              case U of
                                  <<"no_token_user">> ->
                                      {ok, []};
                                  <<"one_token_match_user">> ->
                                      {ok, [[{<<"token">>, T},
                                             {<<"birthday">>, Now}]]};
                                  <<"expired_token_user">> ->
                                      {ok, [[{<<"token">>, T},
                                             {<<"birthday">>, ExpiredBirthday}]]};
                                  <<"no_match_user">> ->
                                      {ok, [[{<<"token">>, deliv_token:new()},
                                             {<<"birthday">>, Now}]]}
                              end
                         end)
     end,
     fun(_) ->
             ct_meck:unload(deliv_db)
     end,
     [
      [{Label,
        ?_assertEqual(Expected,
                      deliv_token:verify_token(EnterpriseName,
                                               UserName,
                                               Token,
                                               Ttl))} ||
          {Label, Expected, UserName, Token} <- [{"No tokens found", token_denied,  <<"no_token_user">>,        UserToken},
                                                 {"Matching token",  token_ok,      <<"one_token_match_user">>, UserToken},
                                                 {"Expired token",   token_expired, <<"expired_token_user">>,   UserToken},
                                                 {"No matches",      token_denied,  <<"no_match_user">>,        UserToken},

                                                 {"undefined token = token_denied",      token_denied, <<"User">>, undefined},
                                                 {"undefined user = token_denied",       token_denied, undefined,  UserToken},
                                                 {"undefined everything = token_denied", token_denied, undefined,  undefined}
                                                ]]
     ]}.

%% Calculate a token birthday in the past
%% leap years need to be accounted for in case the previous year's day doesn't exists.
expired_time({{Y, 2, 29}, Time}) ->
 {{Y - 1, 2, 28}, Time};
expired_time({{Y, M, D}, Time}) ->
 {{Y - 1, M, D}, Time}.
