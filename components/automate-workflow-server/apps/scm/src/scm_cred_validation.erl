%% @doc This module performs validation on bitbucket URLs and credentials.
-module(scm_cred_validation).

-include_lib("delivery/include/deliv_types.hrl").

-export([validate_auth_creds/7]).

%% @doc will accept a URL, username and password and check to ensure that the URL is well formed,
%% the HTTP server on the other end is responding, and the credentials that are given actually work.
%% The passed in function will get executed at the very end of all of the checks, if there were no
%% errors or timeouts.
-spec validate_auth_creds(fun(), binary(), binary(), binary(), binary(), cowboy_req(), any()) -> {halt | true | {true, binary()}, cowboy_req(), any()}.
validate_auth_creds(Fun, Url, Username, undefined, ScmType, Req, #handler{ent_name = EntName} = State) ->
    {ok, BasicAuth} = scm_basic_auth:load_basic_auth_credentials(EntName, ScmType),
    handle_password(Fun, Url, Username, deliv_basic_auth_application:getval(password, BasicAuth), ScmType, Req, State);
validate_auth_creds(Fun, Url, Username, Password, ScmType, Req, State) ->
    IsUrlValid = deliv_web_utils:validate_url_is_well_formed(Url),
    handle_is_url_well_formed(Fun, Url, Username, Password, ScmType, Req, State, IsUrlValid).

%% IsUrlValid can be error or ok, so we check those first
handle_is_url_well_formed(_Fun, _Url, _Username, _Password, _ScmType, Req, State, error) ->
    deliv_web_utils:error_response(400, bad_request, "Invalid URL", Req, State);
handle_is_url_well_formed(Fun, Url, Username, Password, <<"bitbucket">>, Req, State, ok) ->
    Reachable = scm_bitbucket_rest_api:check_reachability(Url, Username, Password),
    handle_check_reachability(Fun, Url, Username, Password, <<"bitbucket">>, Req, State, Reachable);
handle_is_url_well_formed(Fun, Url, Username, Token, <<"github">>, Req, State, ok) ->
    Reachable = scm_github_rest_api:check_reachability(Url, Token),
    handle_check_reachability(Fun, Url, Username, Token, <<"github">>, Req, State, Reachable).

%% Reachable can be {ok, authenticated}, or {error, X} which we check next
handle_check_reachability(Fun, Url, Username, Password, _ScmType, Req, State, {ok, authenticated}) ->
    Fun(Url, Username, Password, Req, State);
handle_check_reachability(_Fun, _Url, _Username, _Password, _ScmType, Req, State, {error, unauthorized}) ->
    deliv_web_utils:error_response(400, bad_request, "Invalid credentials", Req, State);
handle_check_reachability(_Fun, _Url, _Useranme, _Password, ScmType, Req, State, {error, {redirect, Location}}) ->
    RedirectMessage = redirect_message(ScmType, Location),
    deliv_web_utils:error_response(400, bad_request, RedirectMessage, Req, State);
handle_check_reachability(_Fun, _Url, _Useranme, _Password, ScmType, Req, State, {error, not_found}) ->
    NotFoundMessage = not_found_message(ScmType),
    deliv_web_utils:error_response(400, bad_request, NotFoundMessage, Req, State);
handle_check_reachability(_Fun, _Url, _Username, _Password, ScmType, Req, State, {error, {conn_failed, _}}) ->
    ConnectionMessage = connection_message(ScmType),
    deliv_web_utils:error_response(400, bad_request, ConnectionMessage, Req, State);
handle_check_reachability(_Fun, _Url, _Username, _Password, _ScmType, Req, State, {error, _Why}) ->
    deliv_web_utils:error_response(500, internal_server_error, "Internal server error", Req, State).

handle_password(_Fun, _Url, _Username, undefined, _ScmType, Req, State) ->
    deliv_web_utils:error_response(400, bad_request, "Invalid Password", Req, State);
handle_password(Fun, Url, Username, Password, ScmType, Req, State) ->
    validate_auth_creds(Fun, Url, Username, Password, ScmType, Req, State).

redirect_message(<<"bitbucket">>, Location) ->
    lists:flatten(["Bitbucket Instance URL results in redirect. Please use '",
                   Location,
                   "'."]);
redirect_message(<<"github">>, Location) ->
    lists:flatten(["GitHub URL results in redirect. Please use '",
                   Location,
                   "'."]).

connection_message(<<"bitbucket">>) -> "Could not connect to Bitbucket Instance URL.";
connection_message(<<"github">>) -> "Could not connect to GitHub URL.".

not_found_message(<<"bitbucket">>) -> "Bitbucket Instance URL: 404 Not Found.";
not_found_message(<<"github">>) -> "GitHub URL: 404 Not Found.".
