-module(eu_http).

-compile([export_all]).

-define(CONTENT_TYPE_HEADER, "content-type").
-define(JSON_CONTENT_TYPE, "application/json").

-define(TOKEN_HEADER, "chef-delivery-token").
-define(USER_HEADER,  "chef-delivery-user").

token() ->
    <<"fake-token-0xbeefbeefbeef">>.

user() ->
    <<"crusher">>.

enterprise() ->
    <<"acme">>.

mock_is_authorized() ->
    [Ent, User, Token] = [enterprise(), user(), token()],
    When = calendar:now_to_datetime(os:timestamp()),
    meck:expect(sqerl, select,
                fun(deliv_user_candidate_tokens, Params = [_E, _U, _T]) ->
                        case Params of
                            [Ent, User, Token] ->
                                Rows = [[{<<"token">>, Token},
                                         {<<"birthday">>, When}]],
                                {ok, Rows};
                            _ ->
                                {error, {unxpected, Params}}
                        end
                end).

mock_forbidden(EffectiveRoles) ->
    meck:expect(deliv_user, effective_roles,
                fun(_Scope, _Params) ->
                        {ok, EffectiveRoles}
                end).

mock_mods() ->
    [deliv_user, sqerl].

start_delivery() ->
    setup_app_env(),
    delivery_app:start().

setup_app_env() ->
    DelivEnv = [{listen_ip, "127.0.0.1"},
                {listen_port, listen_port()},
                {read_ttl_secs, 3600},
                {write_ttl_secs, 3600},

                %% FIXME: obsolete - delete at the same time as deliv_authzed_keys_server
                {deliv_git_ssh_authorized_keys_path, "/tmp/authorized_keys"},
                {deliv_git_ssh_base_command, <<"/opt/delivery/embedded/bin/delivery-git">>},

                {deliv_git_repos_root, <<"/tmp/test_git_repos">>},
                {deliv_ssh_git_port, 8990},
                {deliv_ssh_git_server_keys_path, "/var/opt/delivery/delivery/etc/ssh_git_server_keys"},
                {deliv_git_repo_template, app_test_helpers:project_path(?MODULE, "priv/git_repo_template")},
                {deliv_chef_config, app_test_helpers:project_path(?MODULE, "test/unit/chef_req.config")}
               ],
    [ ok = application:set_env(delivery, Key, Val)
      || {Key, Val} <- DelivEnv ],
    ok.

listen_port() ->
    10830.

%% @doc Make an authenticated request with the embedded test user and
%% token.
auth_req(Method, Route, ReqBody) ->
    auth_req({user(), token()}, Method, Route, ReqBody).

auth_req({User, Token}, Method, Route, ReqBody) ->
    ReqHeaders0 = add_auth_headers(User, Token, []),
    ReqHeaders = maybe_add_content_type(Method, ReqHeaders0),
    Url = url(Route),
    req(Url, Method, ReqBody, ReqHeaders).

maybe_add_content_type(Method, Headers) when Method =:= post;
                                             Method =:= put ->
    lists:keystore(<<"content-type">>, 1, Headers,
                   {<<"content-type">>, <<"application/json">>});
maybe_add_content_type(_, Headers) ->
    Headers.

req(Url, Method, ReqBody, ReqHeaders) ->
    {ok, StrStatus, RespHeaders, RespBody} =
        ibrowse:send_req(Url, ibrowse_headers(ReqHeaders),
                         Method, ReqBody,
                         [{response_format, binary}]),
    Status = erlang:list_to_integer(StrStatus),
    {Status, RespHeaders, RespBody}.

url(Route) when erlang:is_binary(Route) ->
    url(erlang:binary_to_list(Route));
url(Route) when erlang:is_list(Route) ->
    Port = listen_port(),
    StrPort = erlang:integer_to_list(Port),
    "http://localhost:" ++  StrPort ++ Route.

add_auth_headers(UserName, Token, Headers) ->
    Headers1 = lists:keystore(?TOKEN_HEADER, 1, Headers, {?TOKEN_HEADER, Token}),
    lists:keystore(?USER_HEADER, 1, Headers1, {?USER_HEADER, UserName}).

ibrowse_headers(Headers) ->
    [ {to_str(K), to_str(V)} || {K, V} <- Headers ].

to_str(S) when erlang:is_list(S) ->
    S;
to_str(B) when erlang:is_binary(B) ->
    erlang:binary_to_list(B).
