-module(vis_elasticsearch_status_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

vis_elasticsearch_status_test_() ->
    [
     hoax:fixture(?MODULE, "ping_")
    ].

valid_response_json(Status) ->
    Ejson = {[
              {<<"status">>, Status},
              {<<"indices">>, 1}
             ]},
    chef_json:encode(Ejson).

ping_with_successful_json_returns_pong_and_attrs() ->
    hoax:mock(envy,
              ?expect(get,
                      ?withArgs([visibility, enabled, false, atom]),
                      ?andReturn(true))),
    hoax:expect(receive
                    envy:get(_,_,_) -> <<"http://elasticsearch">>;
                    deliv_http:req(get, _) ->
                        {ok, 200, [], valid_response_json(<<"green">>)}
                end),
    Md = vis_elasticsearch_status:ping(),
    ?assertEqual(pong, Md#status_metadata.status),
    ?assertEqual(1, proplists:get_value(indices, Md#status_metadata.additional_attributes)).

ping_with_red_json_returns_fail() ->
    hoax:mock(envy,
              ?expect(get,
                      ?withArgs([visibility, enabled, false, atom]),
                      ?andReturn(true))),
    hoax:expect(receive
                    envy:get(_,_,_) -> <<"http://elasticsearch">>;
                    deliv_http:req(get, _) ->
                        {ok, 200, [], valid_response_json(<<"red">>)}
                end),
    Md = vis_elasticsearch_status:ping(),
    ?assertEqual(fail, Md#status_metadata.status).

ping_with_non_200_returns_fail_with_response_code() ->
    hoax:mock(envy,
              ?expect(get,
                      ?withArgs([visibility, enabled, false, atom]),
                      ?andReturn(true))),
    hoax:expect(receive
                    envy:get(_,_,_) -> <<"http://elasticsearch">>;
                    deliv_http:req(get, _) ->
                        {ok, 400, [], valid_response_json(<<"green">>)}
                end),
    Md = vis_elasticsearch_status:ping(),
    ?assertEqual(fail, Md#status_metadata.status),
    ?assertEqual(400, proplists:get_value(reponse_code, Md#status_metadata.additional_attributes)).

ping_with_req_error_returns_fail_with_request_failed() ->
    hoax:mock(envy,
              ?expect(get,
                      ?withArgs([visibility, enabled, false, atom]),
                      ?andReturn(true))),
    hoax:expect(receive
                    envy:get(_,_,_) -> <<"http://elasticsearch">>;
                    deliv_http:req(get, _) ->
                        {error, something_is_wrong}
                end),
    Md = vis_elasticsearch_status:ping(),
    ?assertEqual(fail, Md#status_metadata.status),
    ?assertEqual(request_failed, proplists:get_value(error, Md#status_metadata.additional_attributes)).

ping_with_bad_json_returns_fail() ->
    hoax:mock(envy,
              ?expect(get,
                      ?withArgs([visibility, enabled, false, atom]),
                      ?andReturn(true))),
    hoax:expect(receive
                    envy:get(_,_,_) -> <<"http://elasticsearch">>;
                    deliv_http:req(get, _) ->
                        {ok, 200, [], <<"<this>is no json<this>">>}
                end),
    Md = vis_elasticsearch_status:ping(),
    ?assertEqual(fail, Md#status_metadata.status).
