-module(jobs_handler).
-behaviour(deliv_rest).

-include("jobs_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]},
        {jobs_rest, [init/3, rest_init/2]}]).

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
    EJson = jobs_job_json:to_json(jobs_queue:fetch_state()),
    {chef_json:encode(EJson), Req, State}.
