-module(deliv_hand_scm_providers).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

%%TODO: add test for this fuction
to_json(Req, #handler{ent_name = EntName} = State) ->
    BBConfigs = case scm_basic_auth:load_basic_auth_credentials(EntName, <<"bitbucket">>) of
        {ok, BBAuth} -> [scm_basic_auth:to_ejson_with_self_hal(EntName, BBAuth)];
        {error, not_found} -> []
    end,

    GHConfigs = case scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>) of
        {ok, GHAuth} -> [scm_basic_auth:to_ejson_with_self_hal(EntName, GHAuth)];
        {error, not_found} -> []
    end,

    Providers = [
        {[
            {<<"type">>, <<"local">>},
            {<<"name">>, <<"Chef Automate">>},
            {<<"projectCreateUri">>, <<"/projects">>},
            {<<"scmSetupConfigs">>, [true]}
        ]},
        {[
            {<<"type">>, <<"github">>},
            {<<"name">>, <<"GitHub">>},
            {<<"verify_ssl">>, true},
            {<<"projectCreateUri">>, <<"/github-projects">>},
            {<<"scmSetupConfigs">>, GHConfigs}
        ]},
        {[
            {<<"type">>, <<"bitbucket">>},
            {<<"name">>, <<"Bitbucket">>},
            {<<"projectCreateUri">>, <<"/bitbucket-projects">>},
            {<<"scmSetupConfigs">>, BBConfigs}
        ]}
    ],

    deliv_web_utils:content(Providers, Req, State).
