-module(vis_json_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

vis_json_test_() ->
    [
     hoax:fixture(?MODULE, "validate_")
    ].

validate_returns_the_json_when_the_json_is_valid_and_it_passes_endpoint_validation() ->
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, schema) -> {json, req1}
                end),
    ?assertEqual({json, req1, state},
                 vis_json:validate_and_return_json_or_400_with_proper_message(req,
                                                                              state,
                                                                              schema)),
    ?verifyAll.

validate_returns_404_with_general_message_when_json_is_invalid() ->
    Message = <<"The server could not parse the request JSON you sent.">>,
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, schema) -> {{error, invalid_json}, req1};
                    deliv_web_utils:error_response(400, bad_request, Message, req1, state) -> error_response
                end),
    ?assertEqual(error_response,
                 vis_json:validate_and_return_json_or_400_with_proper_message(req,
                                                                              state,
                                                                              schema)),
    ?verifyAll.

validate_returns_404_with_specific_message_when_json_fails_endpoint_validation() ->
    Message = <<"The request JSON you sent was valid JSON but was invalid for this request. Please see the docs for more info.">>,
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, schema) -> {{error, {[{data_invalid, a, reason, b, c}], ejson}}, req1};
                    deliv_web_utils:error_response(400, bad_request, Message, req1, state) -> error_response
                end),
    ?assertEqual(error_response,
                 vis_json:validate_and_return_json_or_400_with_proper_message(req,
                                                                              state,
                                                                              schema)),
    ?verifyAll.
