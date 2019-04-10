%% @doc cowboy handler for the github-projects endpoint. This endpoint is
%% for internal use only at the momemnt, and may change without notice.
-module(github_hand_projects).
-behaviour(deliv_rest).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         from_json/2,
         resource_exists/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, #handler{} = State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, #handler{} = State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, #handler{} = State) ->
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req1} ->
            {false, Req1, State}
    end.

%% @doc will create a new github backed project with the provided params.
from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, github_project), State).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, #handler{ent_name = EntName} = State) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    ProjName = ej:get([<<"name">>], Ejson),
    RepoOwner = ej:get([<<"scm">>, <<"organization">>], Ejson),
    RepoName = ej:get([<<"scm">>, <<"project">>], Ejson),
    PipeName = ej:get([<<"scm">>, <<"branch">>], Ejson),
    VerifySsl = handle_verify_ssl(ej:get([<<"scm">>, <<"verify_ssl">>], Ejson)),
    create_project(EntName, OrgName, ProjName, PipeName, RepoOwner, RepoName, VerifySsl, Req1, State).

create_project(_, _, <<"">>, _PipeName, _RepoOwner, _RepoName, _VerifySsl, Req, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
create_project(EntName, OrgName, ProjName, PipeName, RepoOwner, RepoName, VerifySsl, Req, State) ->
    case github_repo:configure(deliv_project:new(EntName, OrgName, ProjName, PipeName, deliv_scm_github, RepoOwner, RepoName), VerifySsl) of
        {error, _} ->
            deliv_web_utils:error_response(500, internal_error, Req, State);
        {ok, _} ->
            FullLink = deliv_web_utils:href(EntName, ["/orgs/", OrgName, "/projects/", ProjName]),
            Links = [
                {<<"full">>, FullLink},
                {<<"branches">>, <<FullLink/binary, "/branches">>}
            ],
            HAL = deliv_web_utils:make_hal(Links),
            BodyEjson = {[{<<"_links">>, HAL}]},
            Req2 = deliv_web_utils:set_json_body(BodyEjson, Req),
            {{true, FullLink}, Req2, State}
    end.

%% @private
%% This should be extracted into a different module so we are not
%% dealing with the specifics of the github API in the cowboy handler.
handle_verify_ssl(false) -> no_verify_ssl;
handle_verify_ssl(true) -> verify_ssl.
