-module(auth_hand_saml_users).
-behaviour(deliv_rest).

-export([
         init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         from_json/2,
         resource_exists/2
        ]).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

resource_exists(Req, State) ->
    %% See comment on deliv_hand_external_users:resource_exists/2.
    {false, Req, State}.

from_json(Req, State) ->
    case deliv_web_utils:parse_json_req(Req) of
        {{error, _}, Req1} ->
            deliv_web_utils:error_response(400, bad_request, Req1, State);
        {Body, Req1} ->
            UpdatedBody = ej:set({"user_type"}, Body, <<"saml">>),
            handle_json_input(
                {deliv_user_json:validate(UpdatedBody), Req1},
                State
             )
    end.

handle_json_input({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_json_input({{List}, Req}, State) ->
    save_saml_user(
      deliv_web_utils:translate_proplist(List, fun deliv_user_json:translate_json_key/1),
      Req, State
    ).

save_saml_user({ok, PropList}, Req, #handler{ent_name = EntName} = State) ->
    case deliv_user:insert(EntName, PropList) of
        [User] ->
            Username = deliv_user:getval(name, User),
            Link = deliv_web_utils:href(EntName, <<"/users/", Username/binary>>),
            HAL = deliv_web_utils:make_hal([{<<"full">>, Link}]),
            Req1 = deliv_web_utils:set_json_body({[{<<"_links">>, HAL}]}, Req),
            {{true, Link}, Req1, State};
        {error, conflict} ->
            deliv_web_utils:error_response(409, conflict, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.
