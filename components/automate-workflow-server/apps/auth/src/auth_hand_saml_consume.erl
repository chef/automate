-module(auth_hand_saml_consume).
-behaviour(cowboy_http_handler).

-export ([
          init/3,
          handle/2,
          terminate/3
         ]).

-include("auth_types.hrl").
-include_lib("esaml/include/esaml.hrl").

init(_Transport, Req, State) ->
    {ok, Req, State}.

handle(Req, State) ->
    {[EntName], Req1} = deliv_web_utils:extract_bindings([ent_name], Req),
    {SAMLEncoding, SAMLResponse, Relay, Req2} = auth_saml_cowboy:extract_saml_params(Req1),

    % Ensure SP and IdP are configured
    case configure_service_provider(EntName, auth_saml_config:identity_provider(EntName)) of
      {ok, SP} -> handle_post(SAMLEncoding, SAMLResponse, SP, EntName, Relay, Req2, State);
      {error, _} -> redirect_error_demux(server_error, Relay, EntName, <<"SAML configuration invalid, login failed!">>, Req2, State)
    end.

handle_post(SAMLEncoding, SAMLResponse, SP, EntName, Relay, Req, State) ->
    case auth_saml_cowboy:validate_assertion(SP, SAMLEncoding, SAMLResponse) of
        %% Confirm that the UID matches an Authn request we sent previously
        %% Confirm that the UID matches a valid Delivery user
        {ok, Assertion} ->
            NameId = Assertion#esaml_assertion.subject#esaml_subject.name,
            UserName = erlang:list_to_binary(NameId),
            process_user(EntName, UserName, Relay, Req, State,
                handle_user_attributes(Assertion#esaml_assertion.attributes, EntName, UserName));
        {error, Reason} ->
            friendly_assertion_error(Reason),
            redirect_error_demux(access_denied, Relay, EntName, <<"SAML login failed!">>,  Req, State)
    end.

process_user(EntName, UserName, Relay, Req, State, {error, wrong_type})->
    chef_log:error("User attempted SAML login is not a SAML user type. Enterprise: ~p Username: ~p", [EntName, UserName]),
    redirect_error_demux(access_denied, Relay, EntName, <<"SAML login failed. Please sign in with username and password.">>, Req, State);
process_user(EntName, UserName, Relay, Req, State, {error, Why})->
    chef_log:error("Unable to update user metadata ~p ~p ~p", [Why, EntName, UserName]),
    redirect_error_demux(access_denied, Relay, EntName, <<"Invalid user, login failed!">>, Req, State);
process_user(EntName, _, [{oidc_redirect, _}|_] = Relay, Req, State, {ok, User})->
    auth_oidc_utils:redirect_code_request(Relay, EntName, User, Req, State);
process_user(EntName, UserName, Relay, Req, State, {ok, _})->
    case deliv_token:assign_token(EntName, UserName) of
        {ok, Token} ->
            chef_log:info("Successful user login via SAML with username: ~p", [UserName]),
            auth_saml_utils:redirect_with_session(EntName, UserName, Token, Relay, Req, State);
        {error, Reason} ->
            chef_log:failed_call(deliv_token, assign_token, [EntName, UserName], Reason),
            redirect_saml_error(EntName, <<"SAML login failed!">>, Req, State)
    end.

redirect_error_demux(Reason, [{oidc_redirect, RedirectUri}|_] = Relay, _, _Msg, Req, State)
  when is_binary(RedirectUri) ->
    ClientState = auth_oidc_utils:extract_client_state(Relay),
    ClientId = proplists:get_value(oidc_client_id, Relay),
    auth_oidc_utils:redirect_error_response(ClientId, RedirectUri, Reason, ClientState, Req, State);
redirect_error_demux(_, _, EntName, Message, Req, State) ->
    redirect_saml_error(EntName, Message, Req, State).

redirect_saml_error(EntName, Message, Req, State) ->
    Target = auth_saml_utils:login_url_with_error_param(EntName, Message),
    deliv_web_utils:redirect_302(Target, Req, State).

configure_service_provider(EntName, {ok, #esaml_idp_metadata{certificates = Certs}, _, _}) ->
    auth_saml_config:service_provider(EntName, Certs);
configure_service_provider(_, Error) ->
    Error.

handle_user_attributes(Attributes, EntName, UserName) ->
    case check_user_type(deliv_user:fetch(EntName, UserName)) of
        {ok, User} ->
            update_user(Attributes, User);
        {error, not_found} ->
            chef_log:info("Creating new saml user ~p", [UserName]),
            create_saml_user(digest_user_attributes(Attributes), EntName, UserName);
        Error -> Error
    end.

create_saml_user(Attributes, EntName, UserName) ->
    BaseUserProps = [{name, UserName}, {user_type, <<"saml">>}],
    UserPropList = lists:append(BaseUserProps, Attributes),
    case deliv_user:insert(EntName, UserPropList) of
        [User] ->
            DefaultRoles = fetch_default_roles(EntName),
            Parameters = [EntName, UserName, DefaultRoles],
            case deliv_user:edit_roles(grant, enterprise, Parameters) of
                {error, _} = Error -> Error;
                _Ejson -> {ok, User}
            end;
        {error, _} = Error -> Error
    end.

fetch_default_roles(EntName) ->
    {ok, Config} = auth_saml_config:fetch(EntName),
    Config#saml_config.default_roles.

check_user_type({ok, User}) ->
    case deliv_user:getval(user_type, User) of
        <<"saml">> ->
            {ok, User};
        _ -> {error, wrong_type}
    end;
check_user_type(Error) ->
    Error.

update_user([], User) ->
    {ok, User};
update_user(Attributes, User) ->
    Proplist = digest_user_attributes(Attributes),
    UpdatedUser = deliv_user:setvals(Proplist, User),
    deliv_user:update(UpdatedUser).

% this allows for further mangling
digest_user_attribute({firstName, Val})    -> {first_name, Val};
digest_user_attribute({lastName, Val})     -> {last_name, Val};
digest_user_attribute({email, Val})        -> {email, Val};
digest_user_attribute({givenname, Val})    -> {first_name, Val};
digest_user_attribute({surname, Val})      -> {last_name, Val};
digest_user_attribute({emailaddress, Val}) -> {email, Val};
digest_user_attribute(_)                   -> false.

digest_user_attributes(Attrs) ->
    lists:filtermap(fun(X) -> case digest_user_attribute(X) of
                                  false -> false;
                                  Y -> {true, Y}
                              end
                    end,
                    Attrs).

friendly_assertion_error(bad_in_response_to) ->
    chef_log:error("Invalid assertion due to Response and Request ID mismatch.");
friendly_assertion_error(bad_version) ->
    chef_log:error("Invalid assertion due to invalid SAML version from IdP, must be 2.0.");
friendly_assertion_error(bad_recipient) ->
    chef_log:error("Invalid assertion due to invalid recipient in subject confirmation.");
friendly_assertion_error(bad_audience) ->
    chef_log:error("Invalid assertion due to Bad Audience, check your configuration.");
friendly_assertion_error(stale_assertion) ->
    chef_log:error("Invalid assertion due to assertion arriving after notafter time.");
friendly_assertion_error(early_assertion) ->
    chef_log:error("Invalid assertion due to assertion arriving prior to notbefore time.");
friendly_assertion_error(duplicate) ->
    chef_log:error("Invalid assertion due to duplicate assertion, suspected replay attack.");
friendly_assertion_error(Reason) ->
    chef_log:error("Invalid assertion due to ~p", [Reason]).

terminate(_Reason, _Req, _State) -> ok.
