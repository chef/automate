%% @doc A Cowboy Handler for callback requests from Github Applications. Currently
%% the only supported use for this callback is to handle OAuth (fetching and saving
%% OAuth tokens).
-module(deliv_hand_github_auth_callback).
-behaviour(deliv_rest).

-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    init/3,
    rest_init/2,
    handle/2]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(handle), Req, State}.

handle(Req, State) ->
    case do_handle(Req) of
        {ok, RedirectUrl, Req1} ->
            deliv_web_utils:redirect_301(RedirectUrl, Req1, State);
        {error, {Req1, RespCode, Reason}} ->
            deliv_web_utils:error_response(RespCode, Reason, Req1, State)
    end.

%% @private
%% @doc Confirm that the incoming request is valid. If so, fetch and save the
%% token from Github and then redirect the user to the project page.
do_handle(Req) ->
    case validate_request(Req) of
        {ok, Req1, Code, OauthRecord} ->
            case fetch_and_save_token(Code, OauthRecord) of
                {ok, RedirectUrl} ->
                    {ok, RedirectUrl, Req1};
                {error, {RespCode, Reason}} ->
                    {error, {Req1, RespCode, Reason}}
            end;
        {error, _Why} = Error ->
            Error
    end.

%% @private
%% After the user approves the authentication request via the Github UI, they
%% are redirected to our auth_callback with two values: the state param we
%% provided in our original request and a code. Make sure that we have both of
%% these values and that the state is valid.
validate_request(Req) ->
    QsKeys = [<<"code">>, <<"state">>],
    case deliv_web_utils:process_qs_vals(Req, fun process_qs_val/2, QsKeys) of
        {ok, Req1, PropList} ->
            {<<"code">>, Code} = lists:keyfind(<<"code">>, 1, PropList),
            {<<"state">>, OauthRecord} = lists:keyfind(<<"state">>, 1, PropList),
            {ok, Req1, Code, OauthRecord};
        {error, Reason} ->
            {error, {Req, 400, Reason}}
    end.

%% @private
process_qs_val(Key, undefined) ->
    {error, <<"missing ", Key/binary>>};
process_qs_val(<<"code">>, Code) ->
    %% TODO: maybe validate the length?
    Code;
process_qs_val(<<"state">>, State) ->
    case deliv_oauth_token:fetch_by_state(State) of
        {ok, OauthRecord} ->
            OauthRecord;
        {error, _Why} = Error ->
            Error
    end.

%% @private
%% Exchange the code that Github gave us after the user was redirect to this
%% handler for a token. Save that token to the database.
fetch_and_save_token(Code, OauthRecord) ->
    Result = deliv_oauth_token:fetch_token(Code, OauthRecord),
    generate_redirect_url(add_key_to_user(save_token(Result, OauthRecord))).

%% @private
%% Save the token into the DB
save_token({error, _} = Error, _) ->
    Error;
save_token({ok, Token}, OauthRecord) ->
    deliv_oauth_token:save_token(Token, OauthRecord).

%% @private
%% Make the call to add the builder user's public SSH key to github
add_key_to_user({error, _} = Error) ->
    Error;
add_key_to_user({ok, OauthRecord}) ->
    EntId = deliv_oauth_token:getval(scope_id, OauthRecord),
    {ok, Enterprise} = deliv_enterprise:fetch_by_id(EntId),
    EntName = deliv_enterprise:getval(name, Enterprise),
    case github_repo:add_builder_pub_key(EntName) of
        {ok, _PubKeyId} ->
            {ok, OauthRecord};
        {error, _Why} = Error ->
            Error
    end.

%% @private
%% Generate the proper redirect url for the scope of the oauth token.
generate_redirect_url({ok, OauthRecord}) ->
    EntId = deliv_oauth_token:getval(scope_id, OauthRecord),
    {ok, EntRec} = deliv_enterprise:fetch_by_id(EntId),
    EntName = deliv_enterprise:getval(name, EntRec),
    {ok, deliv_web_utils:make_web_url_for_dashboard(EntName)};
generate_redirect_url({error, Why}) ->
    {error, {500, Why}}.
