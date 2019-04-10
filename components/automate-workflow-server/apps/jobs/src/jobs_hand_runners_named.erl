-module(jobs_hand_runners_named).
-behaviour(deliv_rest).

-include("jobs_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         delete_resource/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]},
        {jobs_rest, [resource_exists/2, init/3, rest_init/2]}]).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State = #runner{}) ->
    {chef_json:encode(jobs_runner_json:to_json(State)), Req, State}.

delete_resource(Req, State = #runner{}) ->
    case jobs_runner_registry:delete(State) of
        ok -> {true, Req, State};
        {error, Reason} ->
            chef_log:failed_call(jobs_runner_registry, delete, [State], Reason),
            {false, Req, State}
    end.
