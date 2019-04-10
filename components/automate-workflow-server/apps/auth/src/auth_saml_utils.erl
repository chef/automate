-module(auth_saml_utils).

-include("auth_types.hrl").
-include_lib("esaml/include/esaml.hrl").

-export([
         calculate_fingerprint/1,
         decode_relaystate/1,
         duplicate_assertion_check/2,
         encode_relaystate/1,
         login_url_with_error_param/2,
         make_redirect_target/1,
         make_redirect_target/2,
         metadata_too_big_error_message/0,
         redirect_binding/4,
         redirect_with_session/6,
         relay_url/1
        ]).

-spec redirect_binding(#esaml_sp{}, string(), binary(), binary()) -> binary().
redirect_binding(SP, SSoLoginUri, RelayState, NameIdFormat) ->
    NameId = case NameIdFormat of
                 <<"default">> -> undefined;
                 X -> X
             end,
    Xml = esaml_sp:generate_authn_request(SSoLoginUri, NameId, fun auth_saml_assertion_timer:generate_and_track_unique_id/0, SP),
    esaml_binding:encode_http_redirect(SSoLoginUri, Xml, RelayState).

-spec login_url_with_error_param(binary(), binary()) -> binary().
login_url_with_error_param(EntName, Message) ->
    LoginUrl = deliv_web_utils:make_web_url_for_login(EntName),
    QueryParams = make_encoded_query_param(Message),
    <<LoginUrl/binary, QueryParams/binary>>.

make_encoded_query_param(Message) ->
    EncodedMessage = base64:encode(Message),
    <<"?error=", EncodedMessage/binary>>.

%% @doc Calculates the SHA256 of binary data, prefixed with "SHA256:"
%% Note that it does not validate that the passed binary data is a syntactically
%% correct certificate; if base64-decoding failes, it assumes the passed data
%% has already been decoded.
-spec calculate_fingerprint(binary()) -> string().
calculate_fingerprint(Cert) ->
    CertBin = try base64:decode(Cert) of
                  Bin -> Bin
              catch
                  error:_ -> Cert
              end,
    Hash = crypto:hash(sha256, CertBin),
    "SHA256:" ++ base64:encode_to_string(Hash).

%% @doc Encode relaystate for SAML authn request. Note that this is only to be
%% used for information we don't mind having someone tamper with, since no
%% measures for ensuring integrity/authenticity are in place.
-spec encode_relaystate(term()) -> binary().
encode_relaystate(Term) -> base64:encode(erlang:term_to_binary(Term)).

%% @doc Decode received relaystate. Note that is untrusted input, so using
%% erlang:binary_to_term/2 with safe option.
-spec decode_relaystate(binary()) -> {ok, term()} | {error, badarg}.
decode_relaystate(Base64) ->
    safe_binary_decode(try base64:decode(Base64)
                       catch
                           error:_ -> error
                       end).

-spec safe_binary_decode(binary() | error) -> {error, badarg} | term().
safe_binary_decode(error) -> {error, badarg};
safe_binary_decode(Binary) ->
    case try erlang:binary_to_term(Binary, [safe])
         catch
             error:_ -> error
         end of
        error -> {error, badarg};
        Terms -> error_unless_proplist(Terms)
    end.

-spec error_unless_proplist([term()]) -> {error, badarg} |
                                          {ok, proplists:proplist()}.
error_unless_proplist(Terms) ->
    case is_proplist(Terms) of
        true -> {ok, Terms};
        _ -> {error, badarg}
    end.

is_proplist(List) ->
    is_list(List) andalso
    lists:all(fun({X, _}) when is_atom(X) -> true;
                 (X) when is_atom(X) -> true;
                 (_) -> false
              end,
              List).

-spec make_redirect_target(binary(), binary()) -> binary().
make_redirect_target(EntName, RelayState) ->
    make_redirect_target(auth_saml_config:identity_provider(EntName),
                         EntName, RelayState).

-spec make_redirect_target(binary()) -> binary().
make_redirect_target(RelayState) ->
    {ok, EntName} = deliv_enterprise:get_canonical_enterprise(),
    make_redirect_target(auth_saml_config:identity_provider(EntName),
                         EntName, RelayState).

%% All replies are redirects, in the case of an error the user will get
%% redirected to the login page with an error query param and a base64 encoded message
make_redirect_target({error, _}, EntName, _) ->
    login_url_with_error_param(EntName, <<"SAML is not configured">>);
make_redirect_target({ok, #esaml_idp_metadata{login_location_redirect = SSoLoginUri, certificates = Certs}, <<"HTTP-Redirect">>, NameIdFormat}, EntName, RelayState) ->
    case auth_saml_config:service_provider(EntName, Certs) of
        {ok, ServiceProvider} ->
            redirect_binding(ServiceProvider, SSoLoginUri, RelayState, NameIdFormat);
        {error, Why} ->
            chef_log:error("Unable to load SAML Service Provider configuration ~p", [Why]),
            login_url_with_error_param(EntName, <<"SAML is not configured">>)
    end;
%% This matches a different binding than HTTP-Redirect
make_redirect_target({ok, _, Binding, _}, EntName, _) ->
    Message = <<"SAML login with ",
                Binding/binary,
                " binding not yet supported">>,
    login_url_with_error_param(EntName, Message).

-spec relay_url(binary() | undefined | true) -> binary().
relay_url(Url) when is_binary(Url) ->
    encode_relaystate([{return_url, Url}]);
relay_url(_) -> <<"">>.

-spec redirect_with_session(binary(), binary(), binary(), proplists:proplist(),
                            cowboy_req(), req_handler()) ->
    {ok, cowboy_req(), req_handler()}.
redirect_with_session(EntName, UserName, Token, RelayPropList, Req, State = #handler{read_ttl = TTL}) ->
    LoginURL = deliv_web_utils:make_web_url_for_login(EntName),
    Target = case proplists:get_value(return_url, RelayPropList) of
                 undefined -> LoginURL;
                 ReturnUrl -> <<LoginURL/binary, "?returnUrl=", ReturnUrl/binary>>
             end,
    Secure = deliv_web_utils:protocol() == "https",
    {ok, Req2} = deliv_web_utils:set_cookie(<<"saml-chef-delivery-token">>, Token, Secure, Req),
    {ok, Req3} = deliv_web_utils:set_cookie(<<"saml-chef-delivery-user">>, UserName, Secure, Req2),
    {ok, Req4} = deliv_web_utils:set_cookie(<<"saml-chef-delivery-ttl">>, list_to_binary(integer_to_list(TTL)), Secure, Req3),
    deliv_web_utils:redirect_302(Target, Req4, State).

-spec metadata_too_big_error_message() -> binary().
metadata_too_big_error_message() ->
    <<"metadata file exceeded limit: ",
      (chef_utils:to_bin(?MAXSIZE_METADATA_XML))/binary,
      " bytes">>.

-spec duplicate_assertion_check(#esaml_assertion{}, binary()) -> ok | {error, duplicate_assertion}.
duplicate_assertion_check(Assertion, Digest) ->
    Stale = esaml:stale_time(Assertion),
    Now = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(os:timestamp())),
    KeepSecs = Stale - Now + 1,
    case auth_saml_assertion_timer:check_and_set_digest(Digest, timer:seconds(KeepSecs)) of
        true ->
            chef_log:error("Recieved duplicate assertion: ~p", [Assertion]),
            {error, duplicate_assertion};
        _ ->
            ok
    end.
