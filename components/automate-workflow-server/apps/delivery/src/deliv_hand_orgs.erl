-module(deliv_hand_orgs).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         from_json/2,
         to_json/2,
         resource_exists/2
        ]).

-include("deliv_types.hrl").

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req1} ->
            {false, Req1, State};
        {<<"GET">>, Req1} ->
            {true, Req1, State}
    end.

to_json(Req, #handler{ent_name = EntName} = State) ->
    case deliv_organization:fetch_for_ent(EntName) of
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_error, Req, State);
        OrgsListJson ->
            RespData = {[{<<"orgs">>, OrgsListJson}]},
            OrgLink = case OrgsListJson of
                        [] -> [];
                        _ -> [{show_org,
                               {t, deliv_web_utils:href(EntName,
                                                        "/orgs/{org_name}")},
                               [], ignore_authz}]
                      end,
            HalLinks = [{create_org,
                         deliv_web_utils:href(EntName, "/orgs"), []} |
                        OrgLink],
            Resp = deliv_hal:add_authorized_hal(RespData,
                                                HalLinks,
                                                Req,
                                                State),
            deliv_web_utils:content(Resp, Req, State)
    end.

from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, new_org_jesse_spec()), State).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req},
             #handler{ent_name = EntName, user_name = UserName} = State) ->
    OrgName = ej:get([<<"name">>], Ejson),
    case deliv_organization:insert(EntName, OrgName) of
        {error, conflict} ->
            deliv_web_utils:error_response(409, conflict, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_error, Req, State);
        [_Org] ->
            deliv_event:publish({orgs, crud},
                                {ejson,
                                 {[{<<"event">>, <<"org_created">>},
                                   {<<"name">>, OrgName},
                                   {<<"created_by">>, UserName},
                                   {<<"created_at">>, cowboy_clock:rfc1123()}
                                  ]}}),
            FullLink = deliv_web_utils:href(EntName, ["/orgs/", OrgName]),
            Links = [{<<"full">>, FullLink},
                     {<<"create-project">>,
                      deliv_web_utils:href(EntName,
                                           ["/orgs/", OrgName, "/projects"])}],
            HAL = deliv_web_utils:make_hal(Links),
            BodyEjson = {[{<<"_links">>, HAL}]},
            Req1 = deliv_web_utils:set_json_body(BodyEjson, Req),
            {{true, FullLink}, Req1, State}
    end.

new_org_jesse_spec() ->
    chef_json:simple_string_dict_spec([<<"name">>]).
