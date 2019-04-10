-module(deliv_hand_stash_webhook_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

init_returns_correct_tuple_test() ->
    Actual = deliv_hand_stash_webhook:init(ignored, req, state),

    Expected = {upgrade, protocol, cowboy_rest, req, state},

    ?assertEqual(Expected, Actual).

rest_init_returns_correct_tuple_test() ->
    Actual = deliv_hand_stash_webhook:rest_init(req, state),

    Expected = {ok, req, state},

    ?assertEqual(Expected, Actual).

allowed_methods_allows_only_POST_test() ->
    ?assertEqual({[<<"POST">>], req, state},
                 deliv_hand_stash_webhook:allowed_methods(req, state)).

content_types_accepted_accepts_json_test() ->
    hoax:test(fun() ->
                      hoax:mock(deliv_web_utils,
                                ?expect(content_type_json_map,
                                        ?withArgs([handle]),
                                        ?andReturn(expected_map))),

                      Actual = deliv_hand_stash_webhook:content_types_accepted(req, state),

                      ?assertEqual({expected_map, req, state}, Actual),

                      ?verifyAll
              end).

handle_returns_true() ->
    Actual = deliv_hand_stash_webhook:handle(req, state),
    ?assertEqual({true, req, state}, Actual),
    ?verifyAll.

