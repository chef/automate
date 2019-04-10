%% @doc cowboy handler for the {scm_type} servers endpoint. This endpoint is
%% used to post basic auth configuration when setting up scm-backed integrations.
-module(scm_hand_servers_named).
-behaviour(deliv_rest).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         delete_resource/2,
         from_json/2,
         init/3,
         rest_init/2,
         update_auth_credentials/5
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, #handler{} = State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, #handler{} = State) ->
    {[<<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, #handler{} = State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

%% @doc will delete the named configured scm_type server.
delete_resource(Req, #handler{ent_name = EntName} = State) ->
    {Url, Req1} = cowboy_req:binding(server_url, Req),
    {ScmType, Req2} = cowboy_req:binding(scm_type, Req1),
    FormattedScmType = format_scm_type(ScmType),

    case scm_basic_auth:delete_basic_auth_credentials(EntName, Url, ScmType) of
        ok ->
            {true, Req2, State};
        {error, not_found} ->
            ResponseMsg = <<"The ", FormattedScmType/binary, " SCM link you're trying to remove",
                            " could not be found.">>,
            deliv_web_utils:error_response(404, not_found, ResponseMsg, Req2, State);
        {error, projects_exist} ->
            ResponseMsg =  <<"Some Automate projects are connected to ", FormattedScmType/binary,
                           " repositories. Please remove those links before removing",
                           " the SCM link to ", FormattedScmType/binary, ".">>,
            deliv_web_utils:error_response(412, precondition_failed, ResponseMsg, Req2, State);
        {error, Why} ->
            chef_log:failed_call(?MODULE, scm_basic_auth, [EntName, Url], Why),
            ResponseMsg = <<"There was a problem removing the ", FormattedScmType/binary,
                            " SCM link.">>,
            deliv_web_utils:error_response(500, internal_server_error, ResponseMsg, Req2, State)
    end.

%% @doc will update a new scm-type server configuration with the provided params.
from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, bitbucket_server), State).

%% @doc will get called when all the URL and credential validation succeeds.
-spec update_auth_credentials(binary(), binary(), binary(), cowboy_req(), any()) -> {atom() | {atom(), binary()}, cowboy_req(), any()}.
update_auth_credentials(Url, Username, Password, Req, #handler{ent_name = EntName} = State) ->
    EntDBResp = deliv_enterprise:fetch(EntName),
    handle_enterprise_fetch(EntDBResp, Url, Username, Password, Req, State).

%% @private
handle_parse({{error, Why}, Req}, State) ->
    chef_log:error("Failed to parse request: ~p", [Why]),
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, State) ->
    Url = ej:get([<<"root_api_url">>], Ejson),
    Username = ej:get([<<"user_id">>], Ejson),
    Password = ej:get([<<"password">>], Ejson),
    {ScmType, Req2} = cowboy_req:binding(scm_type, Req),
    scm_cred_validation:validate_auth_creds(fun scm_hand_servers_named:update_auth_credentials/5,
                                                  Url, Username, Password, ScmType, Req2, State).

handle_enterprise_fetch({ok, Ent}, Url, Username, Password, Req, State) ->
    EntId = deliv_enterprise:getval(id, Ent),
    {ScmType, Req2} = cowboy_req:binding(scm_type, Req),
    UpdateResult = scm_basic_auth:update_basic_auth_credentials(Url, Username,
                                                                      Password, EntId, ScmType),
    handle_update_basic_auth_credentials(UpdateResult, Req2, State);
handle_enterprise_fetch({error, not_found}, _Url, _Username, _Password, Req, State) ->
    deliv_web_utils:error_response(404, not_found, Req, State);
handle_enterprise_fetch({error, Why}, _Url, _Username, _Password, Req, State) ->
    chef_log:error("Could not find enterprise when saving auth credentials: ~p", [Why]),
    deliv_web_utils:error_response(500, internal_error, Req, State).

handle_update_basic_auth_credentials({ok, BasicAuth}, Req, #handler{ent_name = EntName} = State) ->
    RespEjson = scm_basic_auth:to_ejson_with_self_hal(EntName, BasicAuth),
    Req1 = deliv_web_utils:set_json_body(RespEjson, Req),
    {true, Req1, State};
handle_update_basic_auth_credentials({error, not_found}, Req, State) ->
    deliv_web_utils:error_response(404, not_found, Req, State);
handle_update_basic_auth_credentials({error, Why}, Req, State) ->
    chef_log:error("Could not save auth credentials: ~p", [Why]),
    deliv_web_utils:error_response(500, internal_error, "There was a problem updating your SCM.", Req, State).

format_scm_type(<<"bitbucket">>) -> <<"Bitbucket">>;
format_scm_type(<<"github">>) -> <<"GitHub">>.
