-module(deliv_hand_intern_users).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         from_json/2,
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
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

from_json(Req, State) ->
    %% TODO refactor deliv_hand_{exernal,intern,saml}_users:from_json/2
    case deliv_web_utils:parse_json_req(Req) of
        {{error, _}, Req1} ->
            deliv_web_utils:error_response(400, bad_request, Req1, State);
        {Body, Req1} ->
            UpdatedBody = ej:set({"user_type"}, Body, <<"internal">>),
            handle_json_input(
                {deliv_user_json:validate(UpdatedBody), Req1},
                State
            )
    end.

handle_json_input({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_json_input({{List}, Req}, State) ->
    save_intern_user(
        deliv_web_utils:translate_proplist(List, fun deliv_user_json:translate_json_key/1),
        Req, State
    ).

save_intern_user({ok, List}, Req, #handler{ent_name = EntName} = State) ->
    List2 = [{user_type, <<"internal">>}, {hash_type, <<"bcrypt">>} | List],
    case deliv_intern_user:insert(EntName, List2) of
        {error, conflict} ->
            deliv_web_utils:error_response(409, conflict, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_error, Req, State);
        [InternUser] ->
            Username = deliv_intern_user:getval(name, InternUser),
            Link = deliv_web_utils:href(EntName, ["/users/", Username]),
            Links = [{full, Link},
                     {'change-password',
                      deliv_web_utils:href(EntName, ["/internal-users/",
                                                     Username,
                                                     "/change-password"])}],
            HAL = deliv_web_utils:make_hal(Links),
            Req1 = deliv_web_utils:set_json_body({[{<<"_links">>, HAL}]}, Req),
            {{true, Link}, Req1, State}
    end.
