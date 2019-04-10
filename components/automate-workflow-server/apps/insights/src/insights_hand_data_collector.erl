-module(insights_hand_data_collector).
-behaviour(deliv_rest).

-include_lib("delivery/include/deliv_types.hrl").

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         from_json/2,
         to_json/2,
         rest_init/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

from_json(Req, State) ->
    {ExchangeName, _} = cowboy_req:binding(exchange, Req),
    case insights_rabbitmq_utils:exchange_map_lookup(ExchangeName) of
        {ok, BunnyServerName} ->
            insights_data_collector_auth:insights_auth_check_and_publish(Req, State, BunnyServerName);
        {error, _} ->
            deliv_web_utils:error_response(404, exchange_not_found, Req, State)
    end.

to_json(Req, State) ->
    #status_metadata{service = RabbitService, status = RabbitStatus, additional_attributes = RabbitAttributes} = insights_rabbitmq_status:ping(),
    RabbitStatus1 = chef_utils:to_bin(RabbitStatus),
    Json = {[{<<"status">>, RabbitStatus1},
             {<<"upstreams">>, {[{RabbitService, {[{<<"status">>, RabbitStatus1} | RabbitAttributes]}}]}}]},
    case RabbitStatus of
        pong ->
            deliv_web_utils:content(Json, Req, State);
        _ ->
            Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
            {ok, Req2} = cowboy_req:reply(500, [], chef_json:encode(Json), Req1),
            {halt, Req2, State}
    end.
