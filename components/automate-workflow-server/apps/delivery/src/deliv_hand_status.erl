-module(deliv_hand_status).
-behaviour(deliv_rest).

-include("deliv_types.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         to_json/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
    DisasterRecoveryMode = delivery_app:get_env(disaster_recovery_mode),
    A2Mode = delivery_app:get_env(a2_mode),
    Pings = health_checks(DisasterRecoveryMode),
    Status = overall_status(Pings),
    FipsMode = delivery_app:get_env(deliv_fips_mode, false),
    Json = {[{<<"status">>, chef_utils:to_bin(Status)},
            {<<"configuration_mode">>,
            chef_utils:to_bin(DisasterRecoveryMode)},
            {<<"fips_mode">>, FipsMode},
            {<<"upstreams">>, status_to_ejson(Pings)},
            {<<"a2_mode">>, A2Mode}]},
    respond(Req, State, Status, Json).

respond(Req, State, pong, Json) ->
  deliv_web_utils:content(Json, Req, State);
respond(Req, _, fail, Json) ->
  Req1 = cowboy_req:set_resp_header(<<"content-type">>,
                                    <<"application/json">>, Req),
  cowboy_req:reply(500, [], chef_json:encode(Json), Req1).

status_to_ejson(#status_metadata{service=Service, status=Status, additional_attributes=Attributes}) ->
    {Service,
     {[
       {<<"status">>, chef_utils:to_bin(Status)} | Attributes
      ]}};
status_to_ejson(Pings) ->
    [{
      [status_to_ejson(Ping) || Ping <- Pings]
     }].

overall_status([H|T]) ->
    overall_status(H, T);
overall_status([]) ->
    pong.

overall_status(#status_metadata{status=pong}, T) ->
    overall_status(T);
overall_status(#status_metadata{service = <<"rabbitmq">>, status = not_running}, T) ->
    case envy:get(insights, enabled, false, atom) of
        true ->
            fail;
        false ->
            overall_status(T)
    end;
overall_status(#status_metadata{status=not_running}, T) ->
    overall_status(T);
overall_status(#status_metadata{status=_}, _) ->
    fail.

health_checks(DisasterRecoveryMode) ->
    [
     deliv_db_status:ping(DisasterRecoveryMode),
     deliv_lsyncd_status:ping(DisasterRecoveryMode),
     vis_elasticsearch_status:ping(),
     insights_rabbitmq_status:ping()
    ].
