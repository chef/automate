%% @doc Cowboy handler for incoming payloads from Github webhooks.
-module(deliv_hand_github_webhook).
-behaviour(deliv_rest).

-include_lib("delivery/include/deliv_types.hrl").
-include("deliv_github_event_types.hrl").

%% Cowboy API
-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2
        ]).

%% @see cowboy_http_handler:init/3
init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

%% @see cowboy_http_handler:resit_init/2
rest_init(Req, State) ->
    {ok, Req, State}.

%% @see cowboy_http_handler:allowed_methods/2
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% @see cowboy_http_handler:content_types_accepted/2
content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

%% @see cowboy_http_handler:handle/2
%%
%% TODO: Send back the ChangeId and error body to Github for logging/debugging purposes.
handle(Req, State) ->
    case validate_request(Req) of
        {ok, Event, OutReq} ->
            %% TODO: Handle error results
            deliv_github_event:handle(Event),
            {true, OutReq, State};
        {ignored, OutReq} ->
            {true, OutReq, State};
        {error, {Code, Reason, OutReq}} ->
            deliv_web_utils:error_response(Code, Reason, OutReq, State)
    end.

%% @private
%% @doc Determine if the event type/action pair is a recognized github event type
%% Returns an event if it is and an error otherwise.
build_event({ok, EventType, Payload, OutReq}) ->
    {Coordinates, Req1} = deliv_web_utils:extract_proj_coordinates(OutReq),
    Action = ej:get([<<"action">>], Payload),
    build_event_by_type(EventType, Action, Coordinates, Payload, Req1);
build_event({error, Why}) ->
    {error, Why}.

%% @private
build_event_by_type(<<"pull_request">>, <<"opened">>, Coordinates, Payload, OutReq) ->
    Event = #github_pr{action = opened, proj_coordinates = Coordinates, payload = Payload},
    {ok, Event, OutReq};
build_event_by_type(<<"pull_request">>, <<"synchronize">>, Coordinates, Payload, OutReq) ->
    Event = #github_pr{action = synchronized, proj_coordinates = Coordinates, payload = Payload},
    {ok, Event, OutReq};
build_event_by_type(<<"pull_request">>, <<"closed">>, Coordinates, Payload, OutReq) ->
    Event = #github_pr{action = closed, proj_coordinates = Coordinates, payload = Payload},
    {ok, Event, OutReq};
build_event_by_type(<<"pull_request">>, Action, Coordinates, _Payload, OutReq) ->
    chef_log:info("unhandled action ~p for event type github pull request received for ~p", [Action, Coordinates]),
    {ignored, OutReq};
build_event_by_type(<<"issue_comment">>, <<"created">>, Coordinates, Payload, OutReq) ->
    Event = #github_comment{action = created, proj_coordinates = Coordinates, payload = Payload},
    {ok, Event, OutReq};
build_event_by_type(<<"ping">>, undefined, Coordinates, _Payload, OutReq) ->
    chef_log:debug("Received ping from Github for ~p", [Coordinates]),
    {ignored, OutReq};
build_event_by_type(EventType, Action, Coordinates, _Payload, OutReq) ->
    chef_log:info("unhandled event action ~s/~s received for ~p", [EventType, Action, Coordinates]),
    {error, {422, undefined_event_type, OutReq}}.



%% @private
%% @doc Determine whether the incoming request is a valid payload from GHE. This
%% is done by checking for the `x-github-event` header and a valid JSON body.
%%
%% TODO: Return more detailed errors in responses. Github logs the responses so
%% returning error details could help in debugging without having to dig through
%% server logs.
%%
%% TODO: Validate that scoped names exists.
validate_request(Req) ->
    build_event(validate_body(validate_header(Req))).

%% @private
%% @doc Validate the request header of the incoming request to make sure it has
%% the required header that identifies it as coming from a Github Webhook.
validate_header(Req) ->
    case cowboy_req:header(<<"x-github-event">>, Req) of
        {undefined, Req1} ->
            %% TODO: More detailed logging data?
            {error, {422, missing_header, Req1}};
        {_, _} = Ans ->
            {ok, Ans}
    end.

%% @private
%% @doc Validate the body of the Github Webhook request to ensure that it is
%% valid, readable JSON.
validate_body({ok, {EventType, Req}}) ->
    {Body, Req1} = deliv_web_utils:read_body(Req),
    case chef_json:decode(Body) of
        {error, Reason} ->
            %% More detailed logging data?
            {error, {422, Reason, Req1}};
        Payload ->
            {ok, EventType, Payload, Req1}
    end;
validate_body({error, _} = Err) ->
    Err.
