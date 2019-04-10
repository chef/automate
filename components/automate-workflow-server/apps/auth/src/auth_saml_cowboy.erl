-module(auth_saml_cowboy).

-include_lib("delivery/include/deliv_types.hrl").

-export([
          extract_saml_params/1,
          validate_assertion/3
        ]).

-spec extract_saml_params(cowboy_req()) -> {binary(), binary(), proplists:proplist(), cowboy_req()}.
extract_saml_params(Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    SAMLEncoding = proplists:get_value(<<"SAMLEncoding">>, PostVals),
    SAMLResponse = proplists:get_value(<<"SAMLResponse">>, PostVals),
    RelayState = case auth_saml_utils:decode_relaystate(
                        proplists:get_value(<<"RelayState">>, PostVals)) of
        {ok, PropList} -> PropList;
        _ -> []
    end,
    {SAMLEncoding, SAMLResponse, RelayState, Req2}.

%% This is almost the function from esaml, but without having it consume the
%% body of the cowboy_req, so it can be used before, and passed into this one.
%% This is necessary for acting on the passed RelayState before validation of
%% the assertions, or after their failing validation.
%%
%% https://github.com/arekinath/esaml/blob/c4d8ecff351432c42d0c6a3b35a3f20fd735dd2a/src/esaml_cowboy.erl#L146-L172
validate_assertion(SP, SAMLEncoding, SAMLResponse) ->
    DuplicateFun = fun auth_saml_utils:duplicate_assertion_check/2,
    CheckKnownIdFun = fun auth_saml_assertion_timer:check_known_id/1,
    case (catch esaml_binding:decode_response(SAMLEncoding, SAMLResponse)) of
        {'EXIT', Reason} ->
            chef_log:error("Unable to decode SAML assertion: ~p", [Reason]),
            {error, {bad_decode, Reason}};
        Xml ->
            esaml_sp:validate_assertion(Xml, DuplicateFun, CheckKnownIdFun, SP)
    end.
