-module(vis_elasticsearch_status).

-include_lib("delivery/include/deliv_types.hrl").

-export([
         ping/0
        ]).

-spec ping() -> #status_metadata{}.
ping() ->
    case visibility_enabled() of
        true ->
            {Status, Attributes} = fetch_elasticsearch_status(),
            #status_metadata{
                service = <<"elasticsearch">>,
                status = Status,
                additional_attributes = Attributes};
        false ->
            #status_metadata{
                service = <<"elasticsearch">>,
                status = not_running,
                additional_attributes = [{<<"description">>, <<"elasticsearch is disabled">>}]}
    end.

-spec fetch_elasticsearch_status() -> {atom(), proplist()}.
fetch_elasticsearch_status() ->
    ESUrl = envy:get(visibility, elasticsearch_url, binary),
    RequestUrl = <<ESUrl/binary, "/_cluster/health">>,
    case deliv_http:req(get, RequestUrl) of
        {ok, 200, _Headers, Body} ->
            Ejson = chef_json:decode(Body),
            Status = parse_status_from_response(Ejson),
            Attributes = extract_attributes_from_response(Ejson),
            {Status, Attributes};
        {ok, Code, _Headers, _Body} ->
            {fail, [{reponse_code, Code}]};
        {error, Why} ->
            chef_log:error("Failed to ping elasticsearch: ~p", [Why]),
            {fail, [{error, request_failed}]}
    end.

-spec parse_status_from_response(json() | {error, invalid_json}) -> atom().
parse_status_from_response({error, invalid_json}) ->
    fail;
parse_status_from_response(Ejson) ->
    case ej:get({"status"}, Ejson) of
        <<"green">> -> pong;
        <<"yellow">> -> pong;
        _ -> fail
    end.

-spec extract_attributes_from_response(json() | {error, invalid_json}) -> proplist().
extract_attributes_from_response({error, invalid_json}) ->
    [{error, invalid_json}];
extract_attributes_from_response({Props}) ->
    [ {list_to_atom(binary_to_list(K)), V} || {K, V} <- Props ].

-spec visibility_enabled() -> true | false.
visibility_enabled() ->
    envy:get(visibility, enabled, false, atom).
