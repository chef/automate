-module(insights_data_collector_auth).

-include_lib("delivery/include/deliv_types.hrl").

-export([insights_auth_check_and_publish/3]).
insights_auth_check_and_publish(Req, State, BunnyServerName) ->
    {ok, Token} = application:get_env(insights, data_collector_token),
    case cowboy_req:header(<<"x-data-collector-token">>, Req) of
        {undefined, Req} ->
            chef_log:info("Data Collector request made without access token: ~p", [Req]),
            deliv_web_utils:error_response(401, not_authorized, Req, State);
        {Token, Req2} ->
            case deliv_web_utils:read_body(Req2) of
                {error, Why}->
                    chef_log:failed_call(deliv_web_utils, read_body, [Req2], Why),
                    deliv_web_utils:error_response(500, internal_server_error, Req2, State);
                {Body, Req3} ->
                    insights_ingester:publish(Body, <<"data-collector">>, BunnyServerName),
                    {halt, Req3, State}
            end;
        {_, Req2} ->
            deliv_web_utils:error_response(401, not_authorized, Req2, State)
    end.
