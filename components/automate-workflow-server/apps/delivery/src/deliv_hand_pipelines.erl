-module(deliv_hand_pipelines).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         init/3,
         resource_exists/2,
         rest_init/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    %% TODO: Extract org and project, stuff into state?
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
    {OrgName, Req}  = cowboy_req:binding(org_name, Req),
    {ProjName, Req} = cowboy_req:binding(proj_name, Req),

    case deliv_pipeline:fetch_names(EntName, OrgName, ProjName) of
        PipeNames when erlang:is_list(PipeNames) ->
            RespData = {[{<<"pipelines">>, PipeNames}]},
            Resp = add_hal([EntName, OrgName, ProjName],
                           RespData, Req, State),
            deliv_web_utils:content(Resp, Req, State);
        {error, Why} ->
            chef_log:error("deliv_pipeline:fetch_names failed: ~p",
                            [Why]),
            deliv_web_utils:error_response(500, internal_error, Req, State)
    end.

from_json(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, new_pipeline), State).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Ejson, Req}, #handler{ent_name = EntName} = State) ->
    {OrgName, Req}  = cowboy_req:binding(org_name, Req),
    {ProjName, Req} = cowboy_req:binding(proj_name, Req),

    %% Extract delicious, delicious information from the JSON
    PipeName = ej:get([<<"name">>], Ejson),

    %% We treat the special case when a 'base' is provided that is
    %% exactly the same as the given 'name' exactly as though no base
    %% were provided at all. In both cases, the user is indicating
    %% "This branch already exists; make a pipeline for it, please".
    %%
    %% Causing that to be an error was seen as being overly pedantic,
    %% and required additional unwanted special-casing on the front
    %% end.
    %%
    %% We'll treat a 'null' value in the same way. Note that a
    %% JavaScript 'null' is translated to the 'null' atom in Erlang;
    %% actually specifying the string "null" (i.e., the binary
    %% <<"null">> in Erlang terms) means you (presumably) have an
    %% actual branch named "null" (and also that you are a masochist)
    Base = case ej:get([<<"base">>], Ejson) of
               PipeName ->
                   undefined;
               null ->
                   undefined;
               OtherValue ->
                   OtherValue
           end,

    {ok, RepoPath} = deliv_project:repo_path(EntName, OrgName, ProjName),

    case deliv_pipeline:create(Base, EntName, OrgName, ProjName, PipeName, RepoPath) of
        ok ->
            FullLink = pipeline_link(EntName, OrgName, ProjName, PipeName),
            Resp = add_hal([EntName, OrgName, ProjName, PipeName],
                           {[]}, %% HAL will be the only thing in the body
                           Req, State),
            {{true, FullLink},
             deliv_web_utils:set_json_body(Resp, Req),
             State};
        {error, system_error} ->
            deliv_web_utils:error_response(500, system_error, Req, State);
        {error, {early, EReason}} ->
            %% TODO: might be nice to inform the user they can try
            %% again without specifying "base"
            deliv_web_utils:error_response(400, EReason, Req, State);
        {error, Reason} ->
            case Reason of
                conflict ->
                    deliv_web_utils:error_response(409, conflict, Req, State);
                Reason ->
                    chef_log:log(error, "Pipeline insert for ~p/~p/~p/~p failed: ~p", [EntName, OrgName, ProjName, PipeName, Reason]),
                    deliv_web_utils:error_response(500, internal_error, Req, State)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HAL-Related Functions                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: Some of these can / should be extracted and generalized for broader
%% usage in other endpoints
%%
%% TODO: Commented code should be uncommented when a
%% deliv_hand_named_pipelines module is created. Otherwise, there
%% won't be anything that can handle the links that are generated.

%% @doc Generate a proplist of HAL link information for passing into
%% deliv_hal:add_authorized_hal/4. Passing in a list of 3 items
%% implies HAL links for GET responses; a list of 4 items implies POST
%% responses.
hal_links([EntName, OrgName, ProjName]) ->
    %% These are GET links
    Base = base_link(EntName, OrgName, ProjName),
    %% TemplatedLink = pipeline_link([EntName, OrgName, ProjName, template]),
    AuthzBindings = [OrgName, ProjName],
    [
     {full, Base, AuthzBindings},
     {create_pipeline, Base, AuthzBindings}
     %% TODO: Will we want to include this HAL link if there are no
     %% pipelines present for the given project?
     %%
     %%{pipeline, {t, TemplatedLink}, AuthzBindings}
    ];
hal_links([EntName, OrgName, ProjName, _PipeName]) ->
    %% These are POST links
    Base = base_link(EntName, OrgName, ProjName),
    %% Individual = pipeline_link(EntName, OrgName, ProjName, PipeName),
    AuthzBindings = [OrgName, ProjName],
    [
     %% {full, Individual, AuthzBindings ++ [PipeName]},
     {list_pipelines, Base, AuthzBindings}
    ].

%% @doc Adds HAL links to an EJson response body. `AllBindings' is a
%% list of `[EntName, OrgName, ProjName]' or `[EntName, OrgName,
%% ProjName, PipeName]', depending on which HTTP method the request is
%% handling (GET or POST, respectively).
%%
%% NOTE: Modifies neither `Req' nor `State'! Only returns a
%% "HAL-enabled" EJson response; you'll need to add it to your
%% outgoing `Req' yourself
add_hal(AllBindings, ResponseData, Req, State) ->
    Links = hal_links(AllBindings),
    deliv_hal:add_authorized_hal(ResponseData,
                                 Links,
                                 Req,
                                 State).

%% These next two functions are just because I got tired of typing the
%% same thing

%% @doc Generate the path to the "pipelines" resource of a given project
base_link(EntName, OrgName, ProjName) ->
    deliv_web_utils:href(EntName, ["/orgs/", OrgName,
                                   "/projects/", ProjName,
                                   "/pipelines"]).

%% @doc Generate the path to a specific pipeline resource. If
%% `PipeName' is the atom `template', generates a "templated" URL
%% path, usable in a templated HAL link.
pipeline_link(EntName, OrgName, ProjName, PipeName) ->
    PipeInsert = case PipeName of
                     %% TODO: Uncomment when we have a named_pipeline handler
                     %% template ->
                     %%     "{pipe}";
                     _ ->
                         PipeName
                 end,
    deliv_web_utils:href(EntName, ["/orgs/", OrgName,
                                   "/projects/", ProjName,
                                   "/pipelines/", PipeInsert]).
