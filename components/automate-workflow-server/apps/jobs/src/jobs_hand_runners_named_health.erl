-module(jobs_hand_runners_named_health).
-behaviour(deliv_rest).

-include("jobs_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         allow_missing_post/2,
         content_types_accepted/2,
         process_post/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]},
        {jobs_rest, [resource_exists/2, init/3, rest_init/2]}]).

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% resource_exists looks up the runner part, and we don't create one if it does
%% not exist -- so those POSTs must stop processing
allow_missing_post(Req, State) ->
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(process_post), Req, State}.

process_post(Req, State) ->
    NewRunner = jobs_runner_registry:health(State),
    Body = chef_json:encode(jobs_runner_json:to_json(NewRunner)),
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {true, Req1, NewRunner}.
