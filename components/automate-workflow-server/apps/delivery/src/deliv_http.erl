%% @doc A simple HTTP client - actually just a thin wrapper on top of ibrowse
-module(deliv_http).

-export([req/1,
         req/2,
         req/3,
         req/4,
         req/5]).

-include("deliv_types.hrl").

-type url() :: string().
-type hostname() :: string().

%% @doc Makes a GET request to `Url'
-spec req(iodata())
        -> {ok, http_status(), http_headers(str_or_binary()), binary()} | {error, _Why}.
req(Url) ->
    req(get, Url, <<>>, [], []).

-spec req(http_method(), iodata())
        -> {ok, http_status(), http_headers(str_or_binary()), binary()} | {error, _Why}.
req(Method, Url) ->
    req(Method, Url, <<>>, [], []).

-spec req(http_method(), iodata(), binary() | json())
        -> {ok, http_status(), http_headers(str_or_binary()), binary()} | {error, _Why}.
req(Method, Url, ReqBody) ->
    req(Method, Url, ReqBody, [], []).

-spec req(http_method(), iodata(), binary() | json(), http_headers(str_or_binary()))
        -> {ok, http_status(), http_headers(str_or_binary()), binary()} | {error, _Why}.
req(Method, Url, ReqBody, ReqHeaders) ->
    req(Method, Url, ReqBody, ReqHeaders, []).

%% @doc Automatically turns SSL verification on for HTTPS addresses
%% Also worth noting that it encodes data for you if you pass it as JSON (and
%% also automatically adds the right 'Content-Type' header)
-spec req(Method, Url, ReqBody, ReqHeaders, IbrowseOptions)
        -> {ok, StatusCode, RespHeaders, RespBody} | {error, _Why} when
    Method :: http_method(),
    Url :: iodata(),
    ReqBody :: binary() | json(),
    ReqHeaders :: http_headers(str_or_binary()),
    IbrowseOptions :: [{atom(), any()}],
    StatusCode :: http_status(),
    RespHeaders :: http_headers(str_or_binary()),
    RespBody :: binary().
req(Method, Url, ReqJson, ReqHeaders, IbrowseOptions)
        when erlang:is_tuple(ReqJson) orelse erlang:is_list(ReqJson) ->
    NewHeaders = [ {?CONTENT_TYPE_HEADER, ?JSON_CONTENT_TYPE} | ReqHeaders ],
    req(Method, Url, chef_json:encode(ReqJson), NewHeaders, IbrowseOptions);
req(Method, Url, ReqBody, ReqHeaders, IbrowseOptions)
        when erlang:is_binary(ReqBody)->
    UrlStr = chef_utils:iodata_to_str(Url),
    case ibrowse:send_req(UrlStr,
                          ibrowse_headers(ReqHeaders),
                          Method,
                          ReqBody,
                          IbrowseOptions ++ default_ibrowse_options(UrlStr)) of
        {ok, StrStatus, RespHeaders, RespBody} ->
            Status = erlang:list_to_integer(StrStatus),
            {ok, Status, RespHeaders, RespBody};
        {error, Why} = Error ->
            chef_log:error("Network error when sending a ~s request to ~s: ~p",
                            [Method, UrlStr, Why]),
            Error
    end.

%% @private
%% @doc Ibrowse expects its headers in string format
-spec ibrowse_headers(http_headers(str_or_binary())) -> http_headers(string()).
ibrowse_headers(Headers) ->
    [ {chef_utils:to_str(K), chef_utils:to_str(V)} || {K, V} <- Headers ].

%% @private
-spec default_ibrowse_options(url()) -> [{atom(), any()}].
default_ibrowse_options(Url) ->
    [{response_format, binary}
     | ssl_ibrowse_options(Url) ++ proxy_ibrowse_options(Url)].

-spec ssl_ibrowse_options(url()) -> [{atom(), any()}].
ssl_ibrowse_options("https://" ++ _Rest = Url) ->
    CaCertFile = delivery_app:get_env(trusted_certificates_file),
    CertDepth = delivery_app:get_env(ca_cert_chain_depth),
    TlsOptions = case is_on_no_verify_list(Url) of
                     true ->
                         chef_log:warning("TLS verification disabled for ~s", [Url]),
                         [
                          {verify, verify_none}
                         ];
                     false ->
                         [
                          {verify_fun, {fun deliv_ssl_verify_functions:verify_selfsigned_cert/3, CaCertFile}},
                          {cacertfile, CaCertFile},
                          {depth, CertDepth}
                         ]
                 end,
    [
     {is_ssl, true},
     {ssl_options, TlsOptions}
    ];
ssl_ibrowse_options(_NotHttps) ->
    [{is_ssl, false}].

-spec is_on_no_verify_list(url()) -> true | false.
is_on_no_verify_list(Url) ->
    NoVerifyHostnames = delivery_app:get_env(no_ssl_verification, []),
    case extract_hostname(Url) of
        {ok, Hostname} ->
            lists:member(Hostname, NoVerifyHostnames);
        {error, Reason} ->
            chef_log:info("Unable to detect host for url ~s. Skipping no_proxy check: ~p", [Url, Reason]),
            false
    end.

-spec extract_hostname(url()) -> {ok, hostname()} | {error, term()}.
extract_hostname(Url) ->
    case http_uri:parse(Url) of
        {ok, {_, _, ParsedHost, _, _, _}} ->
            {ok, string:to_lower(ParsedHost)};
        {error, _} = Error ->
            Error
    end.

-spec proxy_ibrowse_options(url()) -> [{atom(), any()}].
proxy_ibrowse_options(Url) ->
    Host = delivery_app:get_env(proxy_host, ""),
    Port = delivery_app:get_env(proxy_port, 0),
    User = delivery_app:get_env(proxy_user, ""),
    Password = delivery_app:get_env(proxy_password, ""),
    NoProxy = [ string:to_lower(H) || H <- delivery_app:get_env(no_proxy, []) ],
    UrlHost = case extract_hostname(Url) of
                  {ok, Hostname} ->
                      Hostname;
                  {error, Why} ->
                      chef_log:info("Unable to detect host for url ~s. Skipping no_proxy check: ~p",
                                    [Url, Why]),
                      ""
              end,
    HostPort = case {Host, Port, lists:member(UrlHost, NoProxy)} of
            {"", _, _} -> % No proxy host. Ignore all config options.
                [];
            {_, _, true} -> % Url provided matches no_proxy blacklist. Ignore.
                [];
            {_, "", false} -> % No port provided - just use host. ibrowse infers a default port.
                [{proxy_host, Host}];
            {_, 0, false} -> % No port provided (we're just being paranoid about types) - just use host.
                [{proxy_host, Host}];
            {_, _, false} -> % Proxy host and port available.
                [{proxy_host, Host}, {proxy_port, Port}]
        end,
    case {HostPort, User} of
        {[], _} -> % No proxy setup above.
            [];
        {_, ""} -> % No extra proxy authentication information.
            HostPort;
        {_, _} -> % A username was given - pass the password along as well.
            HostPort ++ [{proxy_user, User}, {proxy_password, Password}]
    end.
