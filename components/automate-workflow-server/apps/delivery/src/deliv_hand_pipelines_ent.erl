-module(deliv_hand_pipelines_ent).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, #handler{ent_name = EntName}=State) ->
    case deliv_pipeline_common:get_pipelines_stats_as_ejson(EntName) of
        {ok, Ejson} ->
            deliv_web_utils:content(Ejson, Req, State);
        {error, Error} ->
            chef_log:log(error, "Error is ~p", [Error]),
            %% TODO: be more intelligent about what the error is
            deliv_web_utils:error_response(500, system_error, <<>>, Req, State)
    end.
