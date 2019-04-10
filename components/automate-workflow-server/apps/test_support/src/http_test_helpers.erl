-module(http_test_helpers).

%% @doc Provides a few helper methods to write HTTP tests

-export([
        base_ent_route/1,
        req/2,
        req/3,
        req/4,
        add_auth_headers/3,
        auth_req/3,
        auth_req/4,
        auth_req/5,
        test_json_response/2,
        test_empty_response/2,
        test_empty_204/1,
        test_status_and_headers/2,
        add_qs_to_route/2
       ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-define(TOKEN_HEADER, "chef-delivery-token").
-define(USER_HEADER,  "chef-delivery-user").

-spec req(http_method(), str_or_binary()) -> {http_status(), http_headers(), binary()}.
req(Method, Route) ->
    req(Method, Route, <<>>, []).

-spec req(http_method(), str_or_binary(), binary() | json()) -> {http_status(), http_headers(), binary()}.
req(Method, Route, ReqBody) ->
    req(Method, Route, ReqBody, []).

%% @doc Worth noting that it encodes data for you if you pass it as JSON (and
%% also automatically adds the right 'Content-Type' header)
-spec req(http_method(), Route, ReqBody, http_headers(str_or_binary())) -> {http_status(), http_headers(string()), RespBody} when
    Route :: str_or_binary(),
    ReqBody :: binary() | json(),
    RespBody :: binary().
req(Method, Route, ReqBody, ReqHeaders) ->
    %% start ibrowse if necessary
    IbrowseStart = ibrowse:start(),
    Url = url(Route),
    ct:pal("ibrowse ARGS: ~p", [{Url, ReqHeaders, Method, ReqBody}]),
    %% we disable SSL verification for tests since we run them against
    %% boxes with self-signed SSL certs (would maybe be better to add
    %% said certs to the trusted certs on the relevant machines?)
    IbrowseOptions = [{ssl_options, [{verify, verify_none}]}],
    {ok, Status, RespHeaders, RespBody} = deliv_http:req(Method, Url, ReqBody,
                                                         ReqHeaders, IbrowseOptions),
    %% pretty useful when debugging tests
    ct:pal("Response to {~p, ~p, ~p, ~p} => {~p, ~p, ~p}",
           [Method, Route,
            ReqBody, ReqHeaders,
            Status, RespHeaders, RespBody]),
    %% stop ibrowse if it was started in the 1st place
    case IbrowseStart of
        ok -> ibrowse:stop();
        _ -> ok
    end,
    {Status, RespHeaders, RespBody}.

%% @private
url(Route) when erlang:is_list(Route) ->
    Proto = app_test_helpers:proto(),
    Host = app_test_helpers:host(),
    Port = chef_utils:to_str(app_test_helpers:listen_port()),
    Proto ++ "://" ++ Host ++ ":" ++ Port ++ Route.

%% @private
%% @doc Tests a JSON response; more specifically, checks that the response status
%% is the one expected, checks that the response headers do include the right
%% `content-type' header, and checks the response against the given `JsonValidation'
%% which can call either jesse or ej
%% If all is valid, returns the parsed JSON (otherwise fails).
-spec test_json_response({http_status(), JsonValidation},
    {http_status(), http_headers(), binary()}) -> json() when
    JsonValidation :: {ej, json_spec()} | {exact, json()}.
test_json_response({ExpectedStatus, JsonValidation}, {Status, RespHeaders, RespBody}) ->
    %% status
    ?assertEqual({status, ExpectedStatus}, {status, Status}),
    %% content-type
    ?assertEqual(
       {content_type_header, ?JSON_CONTENT_TYPE},
       {content_type_header, deliv_web_utils:extract_header(?CONTENT_TYPE_HEADER, RespHeaders)}
      ),
    %% syntaxically valid JSON?
    Json = case chef_json:decode(RespBody) of
               {error, invalid_json} -> error(invalid_json);
               Valid -> Valid
    end,
    %% matches the spec?
    case JsonValidation of
        {ej, EjSpec} ->
            ct:pal("EjSpec: ~p, Json: ~p, ej: ~p", [EjSpec, Json, ej:valid(EjSpec, Json)]),
            ?assertEqual({valid_json, ok}, {valid_json, ej:valid(EjSpec, Json)});
        {exact, ExpectedJson} ->
            ?assertEqual({exact_json, ExpectedJson}, {exact_json, Json})
    end,
    Json.

%% @doc Tests for an empty response, with said status
-spec test_empty_response(http_status(), {http_status(), http_headers(), binary()}) -> ok.
test_empty_response(ExpectedStatus, {Status, _RespHeaders, RespBody}) ->
    %% status
    ?assertEqual({status, ExpectedStatus}, {status, Status}),
    %% and empty body
    ?assertEqual({empty_body, <<>>}, {empty_body, RespBody}),
    ok.

%% @doc Shortcut for `test_empty_response/2' with status 204
-spec test_empty_204({http_status(), http_headers(), binary()}) -> ok.
test_empty_204(Response) ->
    test_empty_response(204, Response).

%% @doc Tests that the status is as expected, and that the response headers
%% do contain the expected ones
-spec test_status_and_headers({http_status(), http_headers(string())}, {http_status(), http_headers(string()), binary()}) -> ok.
test_status_and_headers({ExpectedStatus, ExpectedHeaders}, {Status, RespHeaders, _RespBody}) ->
    %% status
    ?assertEqual({status, ExpectedStatus}, {status, Status}),
    %% and headers!
    [] = lists:dropwhile(
        fun({ExpectedHeaderName, ExpectedHeaderValue}) ->
            ?assertEqual(
                {header, ExpectedHeaderName, ExpectedHeaderValue},
                {header, ExpectedHeaderName, proplists:get_value(ExpectedHeaderName, RespHeaders)}
            ),
            true
        end,
        ExpectedHeaders
    ),
    ok.

%% @doc Returns the base route for the given `EntName'
-spec base_ent_route(binary()) -> list().
base_ent_route(EntName) ->
    deliv_web_utils:api_prefix() ++ erlang:binary_to_list(EntName).

%% @doc Makes an authorized call using the provided token and user or username
-spec auth_req({d_user() | binary(), binary()}, http_method(),
               str_or_binary()) -> {http_status(), http_headers(), binary()}.
auth_req({UserOrUserName, Token}, Method, Route) ->
    auth_req({UserOrUserName, Token}, Method, Route, <<>>, []).

%% @doc Same as auth_req/3 above, with a body
-spec auth_req({d_user() | binary(), binary()}, http_method(),
               str_or_binary(), binary() | json()) ->
    {http_status(), http_headers(), binary()}.
auth_req({UserOrUserName, Token}, Method, Route, ReqBody) ->
    auth_req({UserOrUserName, Token}, Method, Route, ReqBody, []).

%% @doc Same as auth_req/4 above, with additional headers
-spec auth_req({d_user() | binary(), binary()}, http_method(),
               str_or_binary(), binary() | json(),
               http_headers(str_or_binary())) ->
    {http_status(), http_headers(string()), binary()}.
auth_req({User, Token}, Method, Route, ReqBody, ReqHeaders) when is_tuple(User) ->
    auth_req({deliv_user:getval(name, User), Token}, Method, Route, ReqBody, ReqHeaders);
auth_req({UserName, Token}, Method, Route, ReqBody, ReqHeaders) when erlang:is_binary(UserName)->
    req(Method, Route, ReqBody, add_auth_headers(UserName, Token, ReqHeaders)).

%% @doc Return a proplist with HTTP token headers for the given
%% user. The given token will be used, and the auth headers will be
%% added to / replaced in the given header list as appropriate.
-spec add_auth_headers(binary(), binary(), http_headers(str_or_binary())) -> http_headers(str_or_binary()).
add_auth_headers(UserName, Token, Headers) ->
    Headers1 = lists:keystore(?TOKEN_HEADER, 1, Headers, {?TOKEN_HEADER, Token}),
    lists:keystore(?USER_HEADER, 1, Headers1, {?USER_HEADER, UserName}).

%% @doc Adds query string parameters to a route
%% Will replace pre-existing values if any were defined
%% for the same keys
-spec add_qs_to_route(Route, QsParams) -> UpdatedRoute when
    Route :: str_or_binary(),
    %% `true', as for cowboy, means the key is there, but without
    %% any associated value
    QsParams :: [{str_or_binary(), str_or_binary() | integer() | true}],
    UpdatedRoute :: string().
add_qs_to_route(Route, QsParams) ->
    {RawRoute, RawQs} = extract_raw_qs(Route),
    PreviousQs = cow_qs:parse_qs(RawQs),
    NewQs = lists:foldl(
        fun({Key, Val}, Qs) ->
            {K, V} = {qs_to_bin(Key), qs_to_bin(Val)},
            lists:keystore(K, 1, Qs, {K, V})
        end,
        PreviousQs,
        QsParams
    ),
    chef_utils:to_str(case cow_qs:qs(NewQs) of
        <<>> -> RawRoute;
        NewQsBin -> <<RawRoute/binary, "?", NewQsBin/binary>>
    end).

%% @private
qs_to_bin(true) -> true;
qs_to_bin(Other) -> chef_utils:to_bin(Other).

%% @private
%% @doc Splits an URI into the actual route part and the query string
-spec extract_raw_qs(str_or_binary()) -> {binary(), binary()}.
extract_raw_qs(RouteWithQs) ->
    RouteBin = chef_utils:to_bin(RouteWithQs),
    case binary:split(RouteBin, <<"?">>) of
        [RouteBin] -> {RouteBin, <<>>};
        [Route, RawQs] -> {Route, RawQs}
    end.
