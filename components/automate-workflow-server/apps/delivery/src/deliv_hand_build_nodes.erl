%% @doc Endpoint handler for build node registration.
%%
%% This module provides a starting point for more seamless build node
%% registration and key distribution. The idea is that build nodes
%% will hit this endpoint during their CCR sending along their name
%% and the URL of the Chef Server that manages them. Delivery can then
%% check the Chef Server URL against a white-list (not yet
%% implemented) and attempt to fetch the client from the Chef
%% Server. If found, create/update a build node identity (currently an
%% ugly hack of using users) and populate the git ssh public key using
%% the Chef sourced public key (the client's public key).
%%
%% For now, clients are stored using a name mangling approach of
%% NAME@CHEF_URL. This way we will be properly name spaced when we
%% start adding support for multiple chef servers.
-module(deliv_hand_build_nodes).
-behaviour(deliv_rest).

-include("deliv_types.hrl").

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         from_json/2,
         rest_init/2
        ]).

%% An internal record for build node registration
-record(builder_reg, {
          name,
          chef_server,
          ent
         }).

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

from_json(Req, State) ->
    handle_parse_req(parse_req(Req), State).

handle_parse_req({ok, BuilderReg, Req}, State) ->
    #builder_reg{ent = Ent, name = Name,
                 chef_server = ChefServer} = BuilderReg,
    PubKey = deliv_chef_api:pub_key_for_client(Name),
    case register_build_node(Ent, Name, PubKey, ChefServer) of
        {ok, _} ->
            {true, Req, State};
        {error, _} ->
            deliv_web_utils:error_response(500, internal_error, Req, State)
    end;
handle_parse_req({{error, bad_request}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State).

parse_req(Req) ->
    case deliv_web_utils:parse_json_req(Req, build_node_reg_spec()) of
        {{error, _Why}, Req1} ->
            {{error, bad_request}, Req1};
        {Ejson, Req1} ->
            extract_req_params(Ejson, Req1)
    end.

extract_req_params(Json, Req) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {ok, #builder_reg{name = ej:get([<<"name">>], Json),
                      chef_server = ej:get([<<"chef_server">>], Json),
                      ent = EntName}, Req1}.

build_node_reg_spec() ->
    chef_json:simple_string_dict_spec([<<"name">>, <<"chef_server">>]).

%% register a build node with Delivery. For now, this abuses what we
%% have and creates an internal user for use as a build node identity.
%%
%% This will belong in a separate module but leaving here for now to
%% get this going end-to-end first.
-spec register_build_node(Ent, Name,
                          PubKey, ChefServer) -> Result when
      Ent        :: binary(),
      Name       :: binary(),
      PubKey     :: {ok, binary()} | {error, _},
      ChefServer :: binary(),
      Result     :: {ok, Action} | {error, _},
      Action     :: registered | reregistered.
register_build_node(Ent, Name, {error, Why} = Error, ChefServer) ->
    chef_log:error("ent=~s; event=build_node_get_client_failure;"
                    " name=~s; chef_server=~s; reason=~p",
                    [Ent, Name, ChefServer, Why]),
    Error;
register_build_node(Ent, Name, {ok, PubKey}, ChefServer) ->
    Email = <<Name/binary, "@", ChefServer/binary>>,
    Password = user_password:hash(user_password:random(10)),
    UserData = [{hash_type, <<"bcrypt">>},
                %% Use Name + ChefServer as the unique identifier to
                %% name space build nodes within Chef Servers.
                {name, Email},
                {first_name, Name},
                {last_name, <<"Build Node: ", ChefServer/binary>>},
                {ssh_pub_key, PubKey},
                {email, Email},
                {hashed_pass, Password}],
    Result = insert_or_update_build_node(Ent, ChefServer, UserData),
    set_authz_roles(Result, Ent, Email).

insert_or_update_build_node(Ent, ChefServer, UserData) ->
    {email, Email} = lists:keyfind(email, 1, UserData),
    {first_name, Name} = lists:keyfind(first_name, 1, UserData),
    InsertData = [{user_type, <<"internal">>} | UserData],
    case deliv_intern_user:insert(Ent, InsertData) of
        {error, conflict} ->
            %% Note: we fetch using Email as the unique name (see
            %% above).
            {ok, BNode} = deliv_intern_user:fetch(Ent, Email),
            UpdatedBNode = deliv_intern_user:setvals(UserData, BNode),
            {ok, _} = deliv_intern_user:update(UpdatedBNode),
            chef_log:info("ent=~s; event=build_node_rereg; name=~s;"
                           " chef_server=~s",
                           [Ent, Name, ChefServer]),
            {ok, reregistered};
        {error, Why} ->
            chef_log:error("ent=~s; event=build_node_user_insert_failure;"
                            " name=~s; reason=~p", [Ent, Name, Why]),
            {error, Why};
        [_InternUser] ->
            chef_log:info("ent=~s; event=build_node_reg; name=~s;"
                           " chef_server=~s",
                           [Ent, Name, ChefServer]),
            {ok, registered}
    end.

set_authz_roles({ok, _} = Result, Ent, Name) ->
    ok = deliv_authz:assign_roles(Ent, Name, [<<"admin">>]),
    Result;
set_authz_roles({error, _} = Error, _, _) ->
    Error.

