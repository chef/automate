-module(jobs_hand_runners).
-behaviour(deliv_rest).

-include("jobs_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         resource_exists/2,
         get_runners/2,
         create_or_return_runner/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]},
        {jobs_rest, [init/3, rest_init/2]}]).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(create_or_return_runner), Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(get_runners), Req, State}.

get_runners(Req, State) ->
    Runners = jobs_runners_state:list(),
    deliv_web_utils:content(jobs_runner_json:to_json(Runners), Req, State).

%% Required to properly reply a 201
resource_exists(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Method =/= <<"POST">>, Req2, State}.

create_or_return_runner(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, job_runner), State).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, State = #handler{ent_name = EntName}) ->
    Hostname = ej:get([<<"hostname">>], Ejson),
    Os = ej:get([<<"os">>], Ejson),
    PlatformVersion = ej:get([<<"platform_version">>], Ejson),
    PlatformFamily = ej:get([<<"platform_family">>], Ejson),
    Platform = ej:get([<<"platform">>], Ejson),
    case jobs_runner_registry:register(EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion) of
        {error, Why} ->
            chef_log:error("Failed to create runner. Reason: ~p", [Why]),
            deliv_web_utils:error_response(500, internal_error, Req, State);
        {ok, Runner} ->
            {Body, Req2, State2} = deliv_web_utils:content(jobs_runner_json:to_json(Runner), Req, State),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {true, Req3, State2}
    end.
