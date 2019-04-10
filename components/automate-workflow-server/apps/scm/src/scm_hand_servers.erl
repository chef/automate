%% @doc cowboy handler for the bitbucket-servers endpoint. This endpoint is
%% used to post basic auth configuration when setting up bitbucket integrations.
-module(scm_hand_servers).
-behaviour(deliv_rest).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         init/3,
         rest_init/2,
         from_json/2,
         to_json/2,
         resource_exists/2,
         save_auth_credentials/5
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, #handler{} = State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, #handler{} = State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, #handler{} = State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

content_types_provided(Req, #handler{} = State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

%% Required to properly reply a 201
resource_exists(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Method =/= <<"POST">>, Req2, State}.

%% @doc will create a new bitbucket server configuration with the provided params.
from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, bitbucket_server), State).

%% @doc will return the configured bitbucket servers currently only ever one.
%% We return an array because soon this will be multiple.
to_json(Req, #handler{ent_name = EntName} = State) ->
    {ScmType, _Req1} = cowboy_req:binding(scm_type, Req),
    case scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) of
        {ok, BasicAuth} ->
            Ejson = scm_basic_auth:to_ejson_with_self_hal(EntName, BasicAuth),
            deliv_web_utils:content([Ejson], Req, State);
        {error, not_found} ->
            deliv_web_utils:content([], Req, State);
        {error, Why} ->
            chef_log:error("Could not load auth credentials: ~p", [Why]),
            deliv_web_utils:error_response(500, internal_error, Req, State)
    end.

-spec save_auth_credentials(binary(), binary(), binary(), cowboy_req(), any()) -> {atom() | {atom(), binary()}, cowboy_req(), any()}.
save_auth_credentials(Url, Username, Password, Req, #handler{ent_name = EntName} = State) ->
    {ok, Ent} = deliv_enterprise:fetch(EntName),
    EntId = deliv_enterprise:getval(id, Ent),
    {ScmType, _Req1} = cowboy_req:binding(scm_type, Req),
    case scm_basic_auth:save_basic_auth_credentials(Url, Username,
                                                          Password, EntId, ScmType) of
        {ok, BasicAuth} ->
            RootApiUrl = deliv_basic_auth_application:getval(root_api_url, BasicAuth),
            EncUrl = deliv_web_utils:encode_url(RootApiUrl),
            SelfLink = erlang:iolist_to_binary([deliv_web_utils:make_api_url_prefix(EntName),
                                                "scm/", ScmType, "/servers/",
                                                EncUrl]),
            {{true, SelfLink}, Req, State};
        {error, conflict} ->
            deliv_web_utils:error_response(409, conflict, Req, State);
        {error, Why} ->
            chef_log:error("Could not save auth credentials: ~p", [Why]),
            deliv_web_utils:error_response(500, internal_error, "There was a problem setting up your SCM.", Req, State)
    end.

%% @private
handle_parse({{error, Why}, Req}, State) ->
    chef_log:error("~p", [Why]),
    deliv_web_utils:error_response(400, bad_request, Req, State);

handle_parse({Ejson, Req}, State) ->
    Url = ej:get([<<"root_api_url">>], Ejson),
    Username = ej:get([<<"user_id">>], Ejson),
    Password = ej:get([<<"password">>], Ejson),
    {ScmType, _Req1} = cowboy_req:binding(scm_type, Req),
    %% Save the auth credentials for the scm_type if the url is valid, well-formed and reachable.
    scm_cred_validation:validate_auth_creds(fun scm_hand_servers:save_auth_credentials/5, Url, Username, Password, ScmType, Req, State).
