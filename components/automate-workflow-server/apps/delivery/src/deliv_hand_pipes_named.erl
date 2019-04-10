-module(deliv_hand_pipes_named).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2,
         delete_resource/2,
         delete_completed/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

delete_resource(Req, State) ->
    {Req1, {EntName, ProjName, OrgName, PipeName}} = scoping_names(Req, State),
    %% that param controls whether we're deleting the branch as well or not
    {DeleteBranch, Req2} = cowboy_req:qs_val(<<"delete_branch">>, Req1),
    DeleteFunName = case DeleteBranch =:= <<"true">> of
        true -> delete_with_branch;
        false -> delete
    end,
    case deliv_pipeline:DeleteFunName(EntName, OrgName, ProjName, PipeName) of
        ok ->
            {true, Req2, State};
        _Error ->
            deliv_web_utils:error_response(500, internal_server_error, Req2, State)
    end.

delete_completed(Req, State) ->
    {true, Req, State}.

handle(Req, State) ->
    {Req1, {EntName, ProjName, OrgName, PipeName} = Scope} = scoping_names(Req, State),
    case deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) of
        {error, not_found} ->
            deliv_web_utils:error_response(404, not_found, Req1, State);
        {ok, Pipe} ->
            {Method, Req2} = cowboy_req:method(Req1),
            handle(Method, Pipe, Scope, Req2, State)
    end.

handle(<<"GET">>, _Pipe, {EntName, ProjName, OrgName, PipeName}, Req, State) ->
    case deliv_pipeline_common:get_pipeline_as_ejson(EntName, OrgName, ProjName, PipeName) of
        {ok, Ejson} ->
            deliv_web_utils:content(Ejson, Req, State);
        {error, Error} ->
            chef_log:log(error, "Error is ~p", [Error]),
            %% TODO: be more intelligent about what the error is
            deliv_web_utils:error_response(500, system_error, <<>>, Req, State)
    end;
handle(<<"PUT">>, Pipe, {EntName, ProjName, OrgName, _PipeName}, Req, State) ->
    handle_json_input(
        deliv_web_utils:parse_json_req(Req, update_pipe_jesse_spec()),
        Pipe, EntName, ProjName, OrgName, State
    ).

handle_json_input({{error, _Why}, Req}, _Pipe, _EntName, _ProjName, _OrgName, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_json_input({Json, Req}, Pipe, EntName, ProjName, OrgName, State) ->
    NewPipeName = ej:get([<<"name">>], Json),
    case deliv_pipeline:rename(Pipe, NewPipeName) of
        {ok, _RenamedPipe} ->
            Location = deliv_web_utils:href(EntName, <<"/orgs/", OrgName/binary,
                                                       "/projects/", ProjName/binary,
                                                       "/pipelines/", NewPipeName/binary>>),
            deliv_web_utils:redirect_301(Location, Req, State);
        {error, conflict} ->
            deliv_web_utils:error_response(409, conflict, <<"pipeline already exists">>, Req, State);
        {error, branch_exists} ->
            deliv_web_utils:error_response(409, conflict, <<"branch already exists">>, Req, State);
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

update_pipe_jesse_spec() ->
    chef_json:simple_string_dict_spec([<<"name">>]).

scoping_names(Req, #handler{ent_name = EntName}) ->
    {OrgName, Req1} = cowboy_req:binding(org_name, Req),
    {ProjName, Req2} = cowboy_req:binding(proj_name, Req1),
    {PipeName, Req3} = cowboy_req:binding(pipe_name, Req2),
    {Req3, {EntName, ProjName, OrgName, PipeName}}.
