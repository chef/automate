-module(deliv_hand_enterprise).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

-define(ROOT_EJSON,
        {[{<<"_links">>,
           {[
             {<<"self">>, {[{<<"href">>, <<"/">>}]}},
             {<<"create-internal-user">>,
              {[{<<"href">>, <<"/internal-users">>}]}},
             {<<"create-external-user">>,
              {[{<<"href">>, <<"/external-users">>}]}},
             {<<"create-saml-user">>,
              {[{<<"href">>, <<"/saml-users">>}]}},
             {<<"users">>, {[{<<"href">>, <<"/users">>}]}},
             {<<"orgs">>, {[{<<"href">>, <<"/orgs">>}]}},
             {<<"admin">>, {[{<<"href">>, <<"/admin">>}]}},
             {<<"searches">>, {[{<<"href">>, <<"/searches">>}]}},
             {<<"get-user-token">>,
              {[
                {<<"href">>, <<"/users/{user_name}/get-token">>},
                {<<"templated">>, true}
               ]}
             },
             {<<"revoke-user-token">>,
              {[
                {<<"href">>, <<"/users/{user_name}/revoke-token">>},
                {<<"templated">>, true}
               ]}}
            ]}
          }]}).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

to_json(Req, State) ->
    {chef_json:encode(?ROOT_EJSON), Req, State}.
