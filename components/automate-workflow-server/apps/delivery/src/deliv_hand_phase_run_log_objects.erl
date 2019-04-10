-module(deliv_hand_phase_run_log_objects).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").
-include("deliv_types.hrl").
-include("deliv_phase_run_log.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         handle/2,
         init/3,
         rest_init/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

from_json(Req, State) ->
    handle_parse(
        deliv_web_utils:read_body(Req),
        Req,
        State
    ).

handle(Req, State) ->
    {RunId, Req1} = cowboy_req:binding(run_id, Req),

    case deliv_phase_run_log:fetch(RunId) of
        {ok, []} ->
            deliv_web_utils:error_response(404, log_not_found, Req1, State);
        {ok, List} ->
            EJson = to_ejson(List),
            deliv_web_utils:content(EJson, Req1, State);
        {error, Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Why, Req1, State)
    end.

%% PRIVATE

handle_parse({error, Why}, Req, State) ->
    chef_log:failed_call(?MODULE, from_json, [Req], Why),
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Data, Req}, _OldReq, State) ->
    {RunId, Req1} = cowboy_req:binding(run_id, Req),

    case deliv_phase_run_log:save(RunId, Data) of
        {error, Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Why, Req1, State);
        _DelivPhaseRunLog ->
            {true, Req1, State}
    end.

to_ejson(Result) ->
    to_ejson(Result, []).

to_ejson([H | T], Acc) ->
    to_ejson(T, [convert_result_to_ejson(H) | Acc]);
to_ejson([], Acc) ->
    lists:reverse(Acc).

convert_result_to_ejson(#deliv_phase_run_log{
                          run_id = RunId,
                          data = Data}) ->
    {[
      {<<"run_id">>,    integer_to_binary(RunId)},
      {<<"data">>,      Data}
    ]}.
