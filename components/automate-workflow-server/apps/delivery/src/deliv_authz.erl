-module(deliv_authz).

-include("deliv_types.hrl").

-export([
         authorize/5,
         authz_sql_query_parameters/3,
         assign_roles/3,
         authz_rules/0,
         authz_rules_for/2,
         authz_rules_for/3,
         forbidden/2,
         forbidden_for_editing_user/2,
         forbidden_for_change_action/2,
         forbidden_none/2,
         roles_match/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(AUTHZ_RULE_FILENAME, "authz_rules").

-define(FORBIDDEN, true).
-define(ALLOWED, false).

%% @doc Read in the system-wide authorization rules. Rules are written
%% using only atoms, but they are translated to binaries for easier
%% interaction with Cowboy and Sqerl.
-spec authz_rules() -> deliv_authz_rules().
authz_rules() ->
    Priv = chef_utils:priv_dir_for_mod(?MODULE),
    RulesFile = filename:join(Priv, ?AUTHZ_RULE_FILENAME),
    case file:consult(RulesFile) of
        {ok, Rules} ->
            convert_rules(Rules);
        {error, Why} ->
            erlang:error({authz_rules_file, Why, RulesFile})
    end.

-spec authz_rules_for(module(), deliv_authz_rules()) -> deliv_authz().
authz_rules_for(Module, Rules) ->
    case lists:keyfind(Module, 1, Rules) of
        {Module, Authz} ->
            %% Otherwise, have fun and knock yourself out
            Authz;
        false ->
            %% If you're looking for authorization rules, but didn't
            %% define any, we're gonna crash, yo.
            erlang:error({undefined_authz, Module})
    end.

authz_rules_for(Module, Scope, Rules) ->
    case lists:keyfind(Module, 1, Rules) of
        {Module, ScopeRules} ->
            case lists:keyfind(Scope, 1, ScopeRules) of
                false ->
                    erlang:error({undefined_authz, Module, Scope});
                ScopeRule ->
                    ScopeRule
            end;
        false ->
            %% If you're looking for authorization rules, but didn't
            %% define any, we're gonna crash, yo.
            erlang:error({undefined_authz, Module})
    end.

%% @doc Cowboy callback for checking the authorization of a
%% request. Return `false' if the request is authorized, `true' if it ins't,
%% and will halt the query with a 500 if something goes wrong.
-spec forbidden(cowboy_req(), req_handler()) -> {false | true | halt,
                                                 cowboy_req(),
                                                 req_handler()}.
forbidden(Req, #handler{authz={Scope, _Mappings}}=State) ->
    Bindings = extract_bindings(Req, scope_bindings(Scope)),
    forbidden(Req, State, Bindings).

%% @doc Do not restrict this request based upon authorization.
-spec forbidden_none(cowboy_req(), req_handler()) ->
    {false, cowboy_req(), req_handler()}.
forbidden_none(Req, State) ->
    {false, Req, State}.

%% @doc Permissions for endpoints allowing to see or update users do not fit
%% well with the usual scope system we use elsewhere: any user can see or
%% update herself
-spec forbidden_for_editing_user(cowboy_req(), req_handler()) ->
    {false | true | halt, cowboy_req(), req_handler()}.
forbidden_for_editing_user(Req, #handler{user_name=UserName} = State) ->
    {TargetUserName, Req1} = cowboy_req:binding(user_name, Req),
    case TargetUserName =:= UserName of
        true ->
            %% we begin by this case to save a DB call if possible
            {?ALLOWED, Req1, State};
        false ->
            %% vanilla authz
            forbidden(Req, State)
    end.

%% @doc The API route for updating a change set does not include
%% the pipeline, although modifying a change is scoped at the
%% the pipeline level, so we need to get access to the appropiate
%% pipeline name before we can do a full authz check.
-spec forbidden_for_change_action(cowboy_req(), req_handler()) ->
                                         {false | true | halt,
                                          cowboy_req(),
                                          req_handler()}.
forbidden_for_change_action(Req, State) ->
    {Req1, EntName, OrgName, ProjName, ChangeId}
        = deliv_web_utils:extract_scoping_names(Req),
    case deliv_change:scoping_names(ChangeId) of
        [EntName, OrgName, ProjName, PipeName] ->
            forbidden(Req1, State, [OrgName, ProjName, PipeName]);
        _Other ->
            chef_log:debug("Invalid scope names ~p for ChangeId ~p",
                            [[EntName, OrgName, ProjName], ChangeId]),
            deliv_web_utils:error_response(404, invalid_scope, Req1, State)
    end.

%% @doc This method is used to check authz authorizations outside of cowboy
%% request processing. As such you have to pass all of the data that would normally come from the request in.
-spec authorize(http_method_bin(), deliv_authz(), cowboy_req(),
                req_handler(), [binary()]) -> allow | forbid.
authorize(HttpMethod, Authz, Req, State, Bindings) ->
    case do_authorize(HttpMethod, Authz, Req, State, Bindings) of
        {allow, _Req, _State} -> allow;
        _Else -> forbid
    end.

%% @doc Assign a set of roles to a given user at the enterprise
%% scope. `Roles' is taken to be all the roles the user should have;
%% any others will be removed.
-spec assign_roles(EnterpriseName, UserName, Roles) -> ok | {error, term()} when
      EnterpriseName :: binary(),
      UserName :: binary(),
      Roles :: [deliv_role()]. %% binaries!
assign_roles(EnterpriseName, UserName, Roles) ->
    case deliv_db:select(deliv_user, assign_roles_at_enterprise,
                         [EnterpriseName, UserName, Roles]) of
        {ok, [[{<<"assign_roles_at_enterprise">>,<<>>}]]} ->
            ok;
        Error ->
            Error
    end.

%% @doc Extract authorization-related information to pass as
%% parameters to authorization-related database functions.
%%
%% In all cases, the enterprise name and a user name (more on this
%% later) will be extracted. Depending on the authorization scope (as
%% specified in the handler state), more parameters will be extracted:
%%
%%   enterprise   -> enterprise name, user name
%%   organization -> same as 'enterprise', but with organization name also
%%   project      -> same as 'organization', but with project name also
%%   pipeline     -> same as 'project', but with pipeline name also
%%
%% The `UserSource' determines where the user name is taken from; see
%% user_name/3 for more.
%%
%% An array of parameter values is thus constructed, ready to be
%% passed to a sqerl call. Naturally, all database calls should be
%% expecting the appropriate parameters, and in the correct order.
%%
%% (This function is defined in this module, rather than deliv_user,
%% in order to try and isolate Cowboy and HTTP-server-aware code, as
%% much as possible.)
-spec authz_sql_query_parameters(cowboy_req(), req_handler(),
                                 request | url) -> [binary()].
authz_sql_query_parameters(Req,
                           #handler{authz={Scope, _},
                                    ent_name = EntName}=State,
                           UserSource) ->
    UserName = user_name(Req, State, UserSource),
    [EntName, UserName | extract_bindings(Req, scope_bindings(Scope))].

%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc Extracts the list of allowed roles for this resource from the
%% handler state, based on the HTTP method used.
%%
%% Throws an error if no mappings are found
allowed_roles(Method, Mappings) when erlang:is_binary(Method) ->
    case lists:keyfind(Method, 1, Mappings) of
        {Method, Roles} ->
            Roles;
        false ->
            erlang:error({undefined_roles, Method})
    end.

%% @private
-spec do_authorize(http_method_bin(), deliv_authz(), cowboy_req(),
                   req_handler(), [binary()]) -> {allow | forbid | {error, _Why},
                                                  cowboy_req(),
                                                  req_handler()}.
do_authorize(HttpMethod, {Scope, Mappings}, Req,
        #handler{ent_name=EntName}=State, Bindings) ->
    AllowedRoles = allowed_roles(HttpMethod, Mappings),
    case AllowedRoles =:= all of
        true ->
            %% anyone allowed!
            {allow, Req, State};
        false ->
            UserName = user_name(Req, State, request),
            Parameters = [EntName, UserName | Bindings],
            %% NOTE: In the future when we want to delegate to A2, we will want to do something different here.
            case deliv_user:effective_roles(Scope, Parameters) of
                {ok, EffectiveRoles} ->
                    {roles_match(EffectiveRoles, AllowedRoles),
                        Req,
                        State#handler{effective_roles=EffectiveRoles}};
                {error, Why} = Error ->
                    chef_log:log(error, "Authorization check failed: ~p", [Why]),
                    {Error, Req, State}
            end
    end.

%% @private
%% @doc Compares two lists of roles, and returns `allow' if those
%% two lists have at least one role in common, `forbid' otherwise
-spec roles_match([deliv_role()], [deliv_role()]) -> allow | forbid.
roles_match(EffectiveRoles, AllowedRoles) ->
    case sets:is_disjoint(sets:from_list(EffectiveRoles),
                          sets:from_list(AllowedRoles)) of
        true  -> forbid;
        false -> allow
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Authorization Rule Conversion Functions

%% @private
%% @doc Convert a atom-based representation of authorization rules to
%% a binary-based one.
convert_rules(Rules) ->
    [handler_rule(Rule) || Rule <- Rules].

handler_rule({Module, ScopeRules}) when erlang:is_list(ScopeRules) ->
    {Module, [scope_rule(Rule) || Rule <- ScopeRules]};
handler_rule({Module, ScopeRule}) when is_tuple(ScopeRule)->
    {Module, scope_rule(ScopeRule)}.

scope_rule({Scope, MethodRules}) ->
    {Scope, [method_rule(Rule) || Rule <- MethodRules]}.

method_rule({Method, Roles}) ->
    {deliv_web_utils:bin_method(Method),
     translate_roles(Roles)}.

translate_roles(all) -> all;
translate_roles(Roles) -> [chef_utils:to_bin(Role) || Role <- Roles].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc Extract a user name from a request / state pair. To use
%% the name specified in the authentication headers (i.e., the user
%% making the request), use `request'; to use a url binding instead
%% (i.e., asking about some user other than "yourself"), use `url'.
user_name(_Req, #handler{user_name=UserName}, request) ->
    UserName;
user_name(Req, _State, url) ->
    [UserName] = extract_bindings(Req, [user_name]),
    UserName.

%% @private
%% @doc URL bindings for extracting additional information from a
%% request needed to satisfy an authorization query, based on a scope.
scope_bindings(enterprise)   -> [];
scope_bindings(organization) -> [org_name];
scope_bindings(project)      -> [org_name, proj_name];
scope_bindings(pipeline)     -> [org_name, proj_name, pipe_name].

%% @private
%% @doc Extract values for an arbitrary number of URL `Bindings' from
%% a request.
-spec extract_bindings(cowboy_req(), [atom()]) -> [binary()].
extract_bindings(Req, Bindings) ->
    [begin
         {Value, Req} = cowboy_req:binding(Binding, Req),
         Value
     end || Binding <- Bindings].

%% @private
%% @doc Similar to forbidden/2, but takes explicit list of Parameters to
%% validate.
-spec forbidden(cowboy_req(), req_handler(), [binary()])
    -> {false | true | halt, cowboy_req(), req_handler()}.
forbidden(Req, #handler{authz=Authz}=State, Bindings) ->
    {Method, Req} = cowboy_req:method(Req),
    handle_do_authorize(do_authorize(Method, Authz, Req, State, Bindings)).

%% @private
handle_do_authorize({allow, Req, State}) ->
    {?ALLOWED, Req, State};
handle_do_authorize({forbid, Req, State}) ->
    {?FORBIDDEN, Req, State};
handle_do_authorize({{error, Why}, Req, State}) ->
    case deliv_pg:translate_error(Why) of
        {not_found, Missing} ->
            chef_log:log(error, "Effective role ~p not found", [Missing]),
            %% We return the generic not_found error, as at this point
            %% we not have a good reason to distinguish that the call
            %% failed because the authz could not be found vs the object
            %% itself, since we have strong constraints in place that
            %% makes it impossible for role
            deliv_web_utils:error_response(404, not_found, Req, State);
        _Other ->
            deliv_web_utils:error_response(500, <<"system error">>, Req, State)
    end.
