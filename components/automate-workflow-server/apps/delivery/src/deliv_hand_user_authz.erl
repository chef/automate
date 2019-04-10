-module(deliv_hand_user_authz).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
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
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(from_json), Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

to_json(Req, #handler{authz={Scope, _}}=State) ->
    %% Here, we want the user name taken from the URL, not the request
    Parameters = deliv_authz:authz_sql_query_parameters(Req, State, url),
    [EntName, UserName | _] = Parameters,
    chef_log:log(info, "checking authorization for ~p/~p at ~p scope", [EntName, UserName, Scope]),

    case deliv_user:scoped_roles(Scope, Parameters) of
        {error, {not_found, Missing}} ->
            chef_log:log(error, "Scope role ~p not found", [Missing]),
            %% We return the generic not_found error, as at this point
            %% we not have a good reason to distinguish that the call
            %% failed because the authz could not be found vs the object
            %% itself, since we have strong constraints in place that
            %% makes it impossible for role
            deliv_web_utils:error_response(404, not_found, Req, State);
        ScopedRoles ->
            {chef_json:encode(ScopedRoles), Req, State}
    end.

from_json(Req, State) ->
    handle_json_input(deliv_web_utils:parse_json_req(Req, authz_modify_user_roles),
                      State).

handle_json_input({{error, _Why}, Req}, State) ->
    %% TODO: would be nice to say why it's invalid
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_json_input({{[{OpBin, Roles}]}, Req}, #handler{authz={Scope, _}}=State) ->
    %% (We can get away with this pattern matching because we only allow a
    %% one-key hash as a valid body, but see below)

    %% Also, this is safe to do because we've constrained the value of
    %% `OpBin' in the JSON schema.
    Op = erlang:binary_to_existing_atom(OpBin, utf8),
    Parameters = deliv_authz:authz_sql_query_parameters(Req, State, url),

    case deliv_user:edit_roles(Op, Scope, Parameters ++ [Roles]) of
        {error, {not_found, Missing}} ->
            deliv_web_utils:error_response(404, Missing, Req, State);
        ScopedRoles ->
            {true,
             deliv_web_utils:set_json_body(ScopedRoles, Req),
             State}
    end;
handle_json_input({_, Req}, State) ->
    %% If we get here, then we've got a valid bit of JSON, but it's
    %% either got too few or too many properties set. We only allow
    %% users to specify one of "grant", "revoke", or "set" properties;
    %% zero or multiple in a request is an error. Since we only allow
    %% one, we can pattern match on that case above. Anything else
    %% will fall through here.
    %%
    %% When Jesse conforms to the JSON Schema draft 4 spec, then we
    %% can add minProperties and maxProperties to the schema and
    %% remove this function head altogether.
    deliv_web_utils:error_response(400, bad_request, Req, State).
