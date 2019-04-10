%% @doc This module is the Github implementation of deliv_oauth_application. It's
%% responsibility is to facilitate the OAuth communication with Github.
-module(deliv_github_oauth).
-behaviour(deliv_oauth_application).

-include_lib("delivery/include/deliv_types.hrl").

%% API
-export([
         authorize_url/2,
         access_token_url/2,
         fetch_token/2,
         save_token/2,
         fetch_app_by_enterprise/1
        ]).

%% @doc Return the authorize url used to request a code.
-spec authorize_url(binary(), d_oauth_application()) -> iolist().
authorize_url(State, Application) ->
    chef_utils:iodata_to_str(
      [deliv_oauth_application:getval(root_url, Application),
       "/login/oauth/authorize",
       "?client_id=", deliv_oauth_application:getval(client_id, Application),
       "&scope=", deliv_web_utils:encode_url(<<"admin:repo_hook,repo,write:public_key">>),
       "&state=", State]).

%% @doc Returns the URL the server should use to get the token.
-spec access_token_url(binary(), d_oauth_application()) -> iolist().
access_token_url(Code, Application) ->
    [deliv_oauth_application:getval(root_url, Application),
     "/login/oauth/access_token",
     "?client_id=", deliv_oauth_application:getval(client_id, Application),
     "&client_secret=", deliv_oauth_application:getval(client_secret, Application),
     "&code=", Code].

%% @doc Call out to Github and fetch the token.
-spec fetch_token(binary(), d_oauth_application()) -> {ok, Token :: binary()} | {error, {integer(), binary()}}.
fetch_token(Code, Application) ->
    case deliv_http:req(post,
                        access_token_url(Code, Application),
                        <<>>,
                        [{"Accept", <<"application/json">>}]) of
        {ok, 200, _RespHeaders, RespBody} ->
            extract_token_from_body(RespBody);
        {ok, ErrorCode, _RespHeaders, RespBody} ->
            chef_log:error("HTTP error when fetching Github OAuth token: ~b. Body: ~p",
                            [ErrorCode, RespBody]),
            {error, translate_fetch_token_error({http_request_failed, ErrorCode})};
        {error, Why} ->
            {error, translate_fetch_token_error(Why)}
    end.

%% @doc Save the token into the DB as well as changing the project type to Github.
-spec save_token(binary(), d_oauth_token()) -> {ok, d_oauth_token()} | {error, atom()}.
save_token(Token, OauthRecord) ->
    OauthRecordWithToken = deliv_oauth_token:setvals([{token, Token}], OauthRecord),
    deliv_oauth_token:update(OauthRecordWithToken).

%% @doc Fetch the Github Oauth application by the enterprise name. We can do this because
%% we enforce a 1:1 relationship between an enterprise and a github token.
%% TODO: we should create a prepared statement that can perform this action in a single query
-spec fetch_app_by_enterprise(binary()) -> {ok, d_oauth_application()} | {error, atom()}.
fetch_app_by_enterprise(EntName) ->
    case deliv_enterprise:fetch(EntName) of
        {ok, Enterprise} ->
            EnterpriseId = deliv_enterprise:getval(id, Enterprise),
            deliv_oauth_application:fetch_by_token_details(enterprise, EnterpriseId, deliv_github_oauth);
        {error, _Why} = Err ->
            Err
    end.

%% @private
%% @doc Pull the token out of the response JSON
-spec extract_token_from_body(binary()) -> {ok, binary()} |
                                           {error, no_token_found}.
extract_token_from_body(ReqBody) ->
    case ej:get([<<"access_token">>], chef_json:decode(ReqBody)) of
        Token when is_binary(Token)-> {ok, Token};
        undefined -> {error, no_token_found}
    end.

%% @private
%% @doc Handle various errors that could occur when trying to exchange the code
%% for the token.
translate_fetch_token_error({http_request_failed, ErrorCode})
        when ErrorCode >= 400 andalso ErrorCode < 500 ->
    {401, <<"invalid code">>};
translate_fetch_token_error({http_request_failed, _OtherCode}) ->
    {503, <<"could not fetch token from the provided code">>};
translate_fetch_token_error(_Other) ->
    {500, <<"internal server error on fetch token">>}.
