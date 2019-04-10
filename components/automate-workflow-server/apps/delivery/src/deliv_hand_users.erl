-module(deliv_hand_users).
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

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

to_json(Req, #handler{ent_name=EntName}=State) ->
    case deliv_user:fetch_user_names(EntName) of
        UserNames when erlang:is_list(UserNames) ->
            Body = {[{<<"users">>, UserNames},
                     {<<"_links">>, hal(EntName)}]},
            deliv_web_utils:content(Body, Req, State);
        {error, Reason} ->
            chef_log:error("Error fetching user names: ~p", [Reason]),
            deliv_web_utils:error_response(500, internal_server_error,
                                           Req, State)
    end.

hal(EntName) ->
    Links = [{self, deliv_web_utils:href(EntName, "/users")},
             {user, {t, deliv_web_utils:href(EntName, "/users/{user_name}")}}],
    deliv_web_utils:make_hal(Links).
