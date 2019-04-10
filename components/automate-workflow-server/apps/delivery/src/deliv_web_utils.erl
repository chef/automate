%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc Provides a few helper functions to help handling cowboy requests.

-module(deliv_web_utils).

-export([
         api_url_for/2,
         relative_href_for/2,
         route_prefix/0,
         api_prefix/0,
         api_base/0,
         bin_method/1,
         content/3,
         content_type_json_map/1,
         content_type_json_or_any_map/1,
         content_type_plain_text/1,
         decode_url/1,
         encode_url/1,
         encode_url_parameters/1,
         encode_url_rfc1738/1,
         error_response/4,
         error_response/5,
         send_error_response/4,
         extract_header/2,
         hostname/0,
         href/2,
         make_api_url_prefix/0,
         make_api_url_prefix/1,
         make_hal/1,
         make_web_url_for_change/4,
         make_web_url_for_base/1,
         make_web_url_for_login/1,
         make_web_url_for_project/3,
         make_web_url_for_dashboard/1,
         parse_json_req/1,
         parse_json_req/2,
         process_qs_vals/2,
         process_qs_vals/3,
         protocol/0,
         redirect_301/3,
         redirect_302/3,
         set_cookie/4,
         set_json_body/2,
         status_to_atom/1,
         translate_json_key/1,
         translate_proplist/2,
         extract_bindings/2,
         extract_scoping_names/1,
         read_body/1,
         reply_unauthorized/2,
         http_post_retry/3,
         extract_proj_coordinates/1,
         validate_url_is_well_formed/1
        ]).

-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

-define(URI_PREFIX, "/workflow"). %% in the non-a2-system this is empty
-define(API_PREFIX, "/api").

-spec content(json(), cowboy_req(), req_handler()) -> {binary(), cowboy_req(), req_handler()}.
content(Ejson, Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"content-type">>,
                                      <<"application/json">>, Req),
    Body = chef_json:encode(Ejson),
    {Body, Req1, State}.

-spec redirect_301(str_or_binary(), cowboy_req(), req_handler()) -> {ok, cowboy_req(), req_handler()}.
redirect_301(Location, Req, State) ->
    {ok, Reply} = cowboy_req:reply(
                    301,
                    [{<<"location">>, chef_utils:to_bin(Location)}],
                    Req
                   ),
    {ok, Reply, State}.

-spec redirect_302(str_or_binary(), cowboy_req(), req_handler()) -> {ok, cowboy_req(), req_handler()}.
redirect_302(Location, Req, State) ->
    {ok, Reply} = cowboy_req:reply(
                    302,
                    [{<<"cache-control">>, <<"no-cache">>},
                     {<<"pragma">>, <<"no-cache">>},
                     {<<"location">>, chef_utils:to_bin(Location)}],
                    Req
                   ),
    {ok, Reply, State}.

-spec set_cookie(str_or_binary(), str_or_binary(), atom(), cowboy_req()) -> {ok, cowboy_req()}.
set_cookie(Key, Value, Secure, Req) ->
    % cowlib 1.0.0 will crash if we pass in {secure, false}
    Opts = case Secure of
        true -> [{path, <<"/">>}, {secure, true}];
        false -> [{path, <<"/">>}]
    end,
    Req2 = cowboy_req:set_resp_cookie(Key, Value, Opts, Req),
    {ok, Req2}.

-spec set_json_body(json(), cowboy_req()) -> cowboy_req().
set_json_body(Ejson, Req) ->
    Req1 = cowboy_req:set_resp_header(<<"content-type">>,
                                      <<"application/json">>, Req),
    cowboy_req:set_resp_body(chef_json:encode(Ejson), Req1).

%% @doc Parses request body as JSON. Ignores content-type headers.
-spec parse_json_req(cowboy_req()) -> {{error, _}, cowboy_req()}
                                          | {json(), cowboy_req()}.

parse_json_req(Req) ->
    {BodyJson, Req1} = read_body(Req),
    {chef_json:decode(BodyJson), Req1}.

%% @doc Reads the entire cowboy body
-spec read_body(cowboy_req()) -> {binary(), cowboy_req()} | {error, _}.
%% TODO errors
read_body(Req) -> read_body(cowboy_req:body(Req), <<>>).

read_body({ok, Data, Req}, Acc) ->
    {<<Acc/binary, Data/binary>>, Req};
read_body({more, Data, Req}, Acc) ->
    read_body(cowboy_req:body(Req), <<Acc/binary, Data/binary>>);
read_body({error, Why}, _) -> {error, Why}.

%% @doc Same as `parse_json_req/1', but takes a jesse JSON spec object
%% to check that the body meets our expectations.  Returns the same as
%% `parse_json_req' if no JSON is found
-spec parse_json_req(cowboy_req(), atom() | json()) -> {{error, _}, cowboy_req()}
                                                           | {json(), cowboy_req()}.
parse_json_req(Req, JsonSpec) ->
    case parse_json_req(Req) of
        {{error, _}, _Req} = Error ->
            Error;
        {Ejson, Req1} ->
            case chef_json:validate(JsonSpec, Ejson) of
                ok ->
                    {Ejson, Req1};
                {error, Why} ->
                    {{error, {Why, Ejson}}, Req1}
            end
    end.

%% @doc That function takes a proplist where the keys (and/or the values) are
%% replaced with whatever `TranslatingFun' returns for them
%% `TranslatingFun' should be either of arity 1 (in which case it will be passed only
%% the key's name) or of arity 2 (in which case it will take both the key and value)
%% If it returns `ignore', that field is removed from the final result
%% If it returns `{error, _Why}', `translate_proplist' stops processing the rest of the
%% input, and just returns the error as is
%% If it returns `{key, Key}', the key gets replaced with `Key', the value stays as is
%% Same idea for `{value, Value}' and `{both, {Key, Value}}'
-spec translate_proplist(PropList, TranslatingFun) -> {ok, TranslatedPropList} | {error, _Why} when
      PropList :: proplist(str_or_binary() | atom(), any()),
      TranslatedPropList :: proplist(str_or_binary() | atom(), any()),
      TranslatingFun :: fun((Key) -> FunResult) | fun((Key, _Value) -> FunResult),
      FunResult :: {key, _NewKey} | {value, _NewValue} | {both, {_NewKey, _NewValue}}
                 | ignore | {error, _Why},
      Key :: str_or_binary() | atom().
translate_proplist(PropList, TranslatingFun) ->
    Translate = case erlang:fun_info(TranslatingFun, arity) of
                    {arity, 1} -> fun(Key, _Value) -> TranslatingFun(Key) end;
                    {arity, 2} -> fun(Key, Value) -> TranslatingFun(Key, Value) end
                end,
    Result = lists:foldl(
               fun({_Key, _Value}, {error, _Why} = Error) ->
                       Error;
                  ({Key, Value}, TranslatedList) ->
                       case Translate(Key, Value) of
                           ignore ->
                               TranslatedList;
                           {error, _Why} = Error ->
                               Error;
                           Other ->
                               NewEntry = case Other of
                                              {key, NewKey} -> {NewKey, Value};
                                              {value, NewValue} -> {Key, NewValue};
                                              {both, {_, _} = NewKeyAndValue} -> NewKeyAndValue
                                          end,
                               [NewEntry | TranslatedList]
                       end
               end,
               [],
               PropList
              ),
    case Result of
        {error, _Why} = Error -> Error;
        TranslatedList -> {ok, lists:reverse(TranslatedList)}
    end.

%% @doc Useful to translate a json's keys into atoms
%% Meant to be used with `translate_proplist/2' above
-spec translate_json_key(binary()) -> {key, atom()} | ignore.
translate_json_key(<<"_links">>) -> ignore;
translate_json_key(Key) -> {key, erlang:binary_to_existing_atom(Key, utf8)}.

%% @doc Send an error response and halt cowboy_rest processing.
-spec error_response(Status, Reason, InputReq, State) -> {halt, OutputReq, State} when
      Status :: http_status(),
      Reason :: str_or_binary() | atom(),
      InputReq :: cowboy_req(),
      OutputReq :: cowboy_req(),
      State :: any().
error_response(Status, Reason, Req, State) ->
    error_response(Status, Reason, <<>>, Req, State).

%% @doc Same as `error_response/4', but allows you to give a feedback message
-spec error_response(Status, Reason, Message, InputReq, State) -> {halt, OutputReq, State} when
      Status :: http_status(),
      Reason :: str_or_binary() | atom(),
      Message :: str_or_binary() | atom(),
      InputReq :: cowboy_req(),
      OutputReq :: cowboy_req(),
      State :: any().
error_response(Status, Reason, Message, Req, State) ->
    Req2 = send_error_response(Status, Reason, Message, Req),
    {halt, Req2, State}.

%% @doc Sends the error response for the *error_response* funs above
-spec send_error_response(Status, Reason, Message, cowboy_req()) -> cowboy_req() when
      Status :: http_status(),
      Reason :: str_or_binary() | atom(),
      Message :: str_or_binary() | atom().
send_error_response(Status, Reason, Message, Req) ->
    Req1 = cowboy_req:set_resp_header(<<"content-type">>,
                                      <<"application/json">>, Req),
    Json1 = {[{<<"error">>, chef_utils:to_bin(Reason)}]},
    Json2 = case chef_utils:to_bin(Message) of
                <<>> ->
                    Json1;
                BinMsg ->
                    ej:set([<<"message">>], Json1, BinMsg)
            end,
    Body = chef_json:encode(Json2),
    {ok, Req2} = cowboy_req:reply(Status, [], Body, Req1),
    Req2.

%% @doc Header names are not case-sensitive
%% Note that you don't need that to get headers from cowboy requests, since cowboy automatically
%% stores header names as lower-case; this function is meant to be used if you need to parse an HTTP
%% response from another service.
-spec extract_header(str_or_binary(), http_headers(str_or_binary())) -> str_or_binary() | not_found.
extract_header(HeaderName, Headers) ->
    LowerCaseHeaderName = string:to_lower(str(HeaderName)),
    case lists:dropwhile(fun({Name, _Value}) -> LowerCaseHeaderName =/= string:to_lower(str(Name)) end, Headers) of
        [] ->
            not_found;
        [ {_Name, Value} | _T ] ->
            Value
    end.

%% I want to escape a URL/URI-type thing. Which function do I use?
%%
%% Good Question. It seems in automate we have 4 options:
%%
%%  deliv_web_utils:encode_url
%%  deliv_web_utils:encode_url_rfc1738
%%  deliv_web_utils:encode_url_parameters
%%  deliv_encode:encode
%%
%% Two of these have analogous decode options:
%%
%%  deliv_web_utils:decode_url
%%  deliv_encode:decode
%%
%% However, the encode functions all call either ibrowse_lib or http_uri:
%%
%%  deliv_web_utils:encode_url            -> http_uri:encode
%%  deliv_web_utils:encode_url_rfc1738    -> ibrowse_lib:url_encode
%%  deliv_web_utils:encode_url_parameters -> http_uri:encode (inserts = and & appropriately)
%%  deliv_encode:encode                   -> http_uri:encode
%%
%% These libraries are similar but behave a bit differently. The
%% source code for them can be found here:
%%
%%   https://github.com/cmullaparthi/ibrowse/blob/master/src/ibrowse_lib.erl
%%   https://github.com/erlang/otp/blob/master/lib/inets/src/http_lib/http_uri.erl
%%
%% - http_uri only encodes "reserved" characters. However, its
%%   reserved set differs from the RFC3986 and RFC1738 definitions of
%%   reserved.
%%
%%    RFC 3986 defines reserved as:
%%
%%      reserved    = gen-delims / sub-delims
%%      gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
%%      sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
%%                  / "*" / "+" / "," / ";" / "="
%%
%%    http_uri has the following characters in its list of reserved
%%    chars:
%%
%%       reserved() ->
%%          sets:from_list([$;, $:, $@, $&, $=, $+, $,, $/, $?,
%%                          $#, $[, $], $<, $>, $\", ${, $}, $|, %"
%%                          $\\, $', $^, $%, $ ]).
%%
%%   Determining the difference between those two lists is left as
%%   an exercize to the reader.
%%
%%   Any bytes not in that list, are passed straight through:
%%
%%      16> http_uri:encode(binary_to_list(<<"!"/utf8>>)).
%%      "!"
%%
%%   including UTF-8:
%%
%%      14> binary_to_list(<<"张伟"/utf8>>).
%%      [229,188,160,228,188,159]
%%      15> http_uri:encode(binary_to_list(<<"张伟"/utf8>>)).
%%      [229,188,160,228,188,159]
%%
%%  - ibrowse_lib encodes anything that isn't: 0-9, a-z, A-Z, -, _, .
%%
%%    This includes unicode:
%%
%%    3> ibrowse_lib:url_encode(binary_to_list(<<"!"/utf8>>)).
%%    "%21"
%%    4> binary_to_list(<<"张伟"/utf8>>).
%%    [229,188,160,228,188,159]
%%    5> ibrowse_lib:url_encode(binary_to_list(<<"张伟"/utf8>>)).
%%    "%e5%bc%a0%e4%bc%9f"
%%
%%  - ibrowse_lib converts " " to + while http_uri uses %20
%%
%%    27> http_uri:encode(" ").
%%    "%20"
%%    28> ibrowse_lib:url_encode(" ").
%%    "+"
%%
%%  - ibrowse_lib uses lowercase letters in its percent encoding.
%%    http_uri uses uppercase letters.
%%
%% In Summary
%%
%% - encode_url_rfc1738 uses ibrowse_lib.  ibrowse_lib *is* pretty
%%   close to RFC1738 (and RFC3986) if only because it percent-encodes
%%   nearly everything.  Note, however, it is closer to the
%%   application/x-www-form-urlencoded media type becuase it converts
%%   spaces to + rather than %20.
%%
%% - Everything else uses http_uri which escapes a more conservative
%%   set of characters than ibrowse_lib, passing most characters
%%   straight through.
%%
%% * Seriously Mate?! I just want to know which one to use!
%%
%%   Hey, I'm just a comment! Don't get mad at me.  If you want
%%   advice, I would probably try encode_url_rfc1738 first, but
%%   ultimately you'll need to test against the consumer of the data.
%%
%% @doc Helper to decode binary strings
-spec decode_url(any()) -> any().
decode_url(undefined) ->
    undefined;
decode_url(Str) when erlang:is_list(Str) ->
    http_uri:decode(Str);
decode_url(Bin) when erlang:is_binary(Bin) ->
    list_to_binary(decode_url(erlang:binary_to_list(Bin))).

%% @doc Helper to encode binary strings
-spec encode_url(str_or_binary()) -> str_or_binary().
encode_url(Str) when erlang:is_list(Str) ->
    http_uri:encode(Str);
encode_url(Bin) when erlang:is_binary(Bin) ->
    list_to_binary(encode_url(erlang:binary_to_list(Bin))).

%% @doc Helper to encode binary strings using rfc 1738
-spec encode_url_rfc1738(str_or_binary()) -> str_or_binary().
encode_url_rfc1738(Str) when erlang:is_list(Str) ->
    ibrowse_lib:url_encode(Str);
encode_url_rfc1738(Bin) when erlang:is_binary(Bin) ->
    list_to_binary(encode_url_rfc1738(erlang:binary_to_list(Bin))).

str(Str) when erlang:is_list(Str) ->
    Str;
str(Str) when erlang:is_binary(Str) ->
    erlang:binary_to_list(Str).

%% @doc Meant to be used to build `content_type_(provided|accepted)'
%% cowboy callbacks
-spec content_type_json_map(atom()) -> list().
content_type_json_map(Callback) ->
    [{{<<"application">>, <<"json">>, '*'}, Callback}].

%% @doc Meant to be used to build `content_type_(provided|accepted)'
%% cowboy callbacks
-spec content_type_json_or_any_map(atom()) -> list().
content_type_json_or_any_map(Callback) ->
    [{{<<"application">>, <<"json">>, '*'}, Callback},
     {<<"*/*">>, Callback}].

%% @doc Meant to be used to build `content_type_(provided|accepted)'
%% cowboy callbacks
-spec content_type_plain_text(atom()) -> list().
content_type_plain_text(Callback) ->
    [{{<<"text">>, <<"plain; charset=utf-8">>, '*'}, Callback}].

%% @doc Create HAL EJSON links. `List' should be a list of tuples of
%% the form `{Name, Href}' or `{Name, {t, Href}}', where the latter
%% indicates a templated HREF. The `Name' values can be atoms or
%% binaries.
%%
%% Example:
%% ```
%% Resp0 = ej:set_p([<<"orgs">>], {[]}, OrgNames),
%% Resp = ej:set_p([<<"_links">>], Resp0,
%%                 make_hal([{'create-org', <<"/api/v0/e/ffdp/orgs/">>},
%%                           {'show-org', {t, <<"/api/v0/e/ffdp/orgs/{org_name}">>}}
%%                          ]))
%% Body = chef_json:encode(Resp)
%% '''
-spec make_hal([{Name, Href} | {Name, {t, Href}}]) -> json() when
      Name :: str_or_binary() | atom(),
      Href :: str_or_binary().
make_hal(List) ->
    lists:foldl(fun({Name, {t, Href}}, Acc) ->
                        BName = chef_utils:to_bin(Name),
                        Acc1 = ej:set_p([BName, <<"href">>], Acc, chef_utils:to_bin(Href)),
                        ej:set_p([BName, <<"templated">>], Acc1, true);
                   ({Name, Href}, Acc)  ->
                        ej:set_p([chef_utils:to_bin(Name), <<"href">>], Acc, chef_utils:to_bin(Href))
                end, {[]}, List).

-spec href(str_or_binary(), str_or_binary()) -> binary().
href(EntName, Rest) ->
    erlang:iolist_to_binary([api_prefix(), EntName, Rest]).

-spec make_web_url_base() -> string().
make_web_url_base() ->
    ?URI_PREFIX "/e/".

-spec make_web_url_for_change(binary(), binary(), binary(), binary()) ->
                                     binary().
make_web_url_for_change(EnterpriseName, OrganizationName,
                        ProjectName, ChangeId) ->
    erlang:iolist_to_binary([protocol(), "://", hostname(), make_web_url_base(),
                             EnterpriseName, "/#/organizations/",
                             OrganizationName, "/projects/", ProjectName,
                             "/changes/", ChangeId]).

-spec make_web_url_for_base(binary()) -> binary().
make_web_url_for_base(EnterpriseName) ->
    erlang:iolist_to_binary([protocol(), "://", hostname(), make_web_url_base(),
                             EnterpriseName, "/"]).

-spec make_web_url_for_login(binary()) -> binary().
make_web_url_for_login(EnterpriseName) ->
    erlang:iolist_to_binary([protocol(), "://", hostname(), make_web_url_base(),
                             EnterpriseName, "/#/login"]).

-spec make_web_url_for_dashboard(binary()) -> binary().
make_web_url_for_dashboard(EnterpriseName) ->
 erlang:iolist_to_binary(
   [protocol(), "://", hostname(), make_web_url_base(), EnterpriseName, "/#/dashboard"]).

-spec make_web_url_for_project(binary(), binary(), binary()) -> binary().
make_web_url_for_project(EnterpriseName, OrgName, ProjectName) ->
    erlang:iolist_to_binary(
      [protocol(), "://", hostname(), make_web_url_base(),
       EnterpriseName, "/#/organizations/",
       OrgName, "/projects/", ProjectName, "/changes"]).

-spec make_api_url_prefix() -> binary().
make_api_url_prefix() -> erlang:iolist_to_binary([protocol(), "://", hostname(),
                                                  api_prefix()]).

-spec make_api_url_prefix(binary()) -> binary().
make_api_url_prefix(EntName) -> erlang:iolist_to_binary([make_api_url_prefix(),
                                                         EntName, "/"]).

%%
%% In some cases (A2) can be a difference between the external URI we
%% wish to present, and the resource of the incoming request
%%
%% /api_prefix/api_version/e
-spec route_prefix() -> string().
route_prefix() ->
    %% Adjacent quoted strings are joined by the preprocessor
    ?API_PREFIX "/" ?DELIV_API_VERSION "/e/".

%% /uri_prefix/api_prefix/api_version/e
-spec api_prefix() -> string().
api_prefix() ->
    ?URI_PREFIX ++ route_prefix().

-spec hostname() -> string().
hostname() ->
    delivery_app:get_env(hostname).

-spec api_base() -> binary().
api_base() ->
    erlang:iolist_to_binary([deliv_web_utils:protocol(),
                             "://",
                             deliv_web_utils:hostname(),
                             ?URI_PREFIX ?API_PREFIX "/" ?DELIV_API_VERSION "/" ?DELIV_API_VERSION]).

-spec protocol() -> string().
protocol() ->
    delivery_app:get_env(api_proto).

%% @doc Generate an HREF to a particular resource. `Prefix' is any additional
%% address prefix. `Params' are specific parameters that are relevant to the
%% given resource `Type'.
-spec make_href_for(Type :: atom(), Prefix :: term(), Params :: [term()]) -> Url :: binary().
make_href_for(phase_run, Prefix, [EntName, OrgName, ProjName, PipeName, PhaseRunId]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/orgs/", OrgName,
                             "/projects/", ProjName,
                             "/pipelines/", PipeName,
                             "/phase_runs/", chef_utils:to_str(PhaseRunId)]);
make_href_for(phase_run_job, Prefix, [EntName, JobId]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/jobs/", JobId]);
make_href_for(trigger_stage, Prefix, [EntName, OrgName, ProjName, ChangeId, Stage]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/orgs/", OrgName,
                             "/projects/", ProjName,
                             "/changes/", ChangeId,
                             "/trigger/", Stage]);
make_href_for(delete_change, Prefix, [EntName, OrgName, ProjName, ChangeId]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/orgs/", OrgName,
                             "/projects/", ProjName,
                             "/changes/", ChangeId]);
make_href_for(merge_change, Prefix, [EntName, OrgName, ProjName, ChangeId]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/orgs/", OrgName,
                             "/projects/", ProjName,
                             "/changes/", ChangeId,
                             "/merge"]);
make_href_for(deliver_change, Prefix, [EntName, OrgName, ProjName, ChangeId]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/orgs/", OrgName,
                             "/projects/", ProjName,
                             "/changes/", ChangeId,
                             "/accept"]);
make_href_for(saml_metadata, Prefix, [EntName]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/saml/metadata"]);
make_href_for(saml_consume, Prefix, [EntName]) ->
    erlang:iolist_to_binary([Prefix, EntName,
                             "/saml/consume"]).

%% @doc Generate the relative HREF for a resource type.
-spec relative_href_for(Type :: atom(), Params :: [term()]) -> binary().
relative_href_for(Type, Params) ->
    make_href_for(Type, api_prefix(), Params).

%% @doc Generate the fully qualified API URL of a particular type.
-spec api_url_for(Type :: atom(), Params :: [term()]) -> binary().
api_url_for(Type, Params) ->
    make_href_for(Type, make_api_url_prefix(), Params).

%% @doc Convert an HTTP method atom to an uppercased binary string for
%% compatibility with Cowboy. Keeping this separate from `chef_utils:to_bin/1'
%% below since we also uppercase the method names.
-spec bin_method(http_method()) -> http_method_bin().
bin_method(get)     -> <<"GET">>;
bin_method(post)    -> <<"POST">>;
bin_method(delete)  -> <<"DELETE">>;
bin_method(put)     -> <<"PUT">>;
bin_method(head)    -> <<"HEAD">>;
bin_method(options) -> <<"OPTIONS">>.

%% @doc Useful to process query string parameters from a URL
%% Basically translates those into a prop-list of processed values
%% `ProcessingFun' should take two arguments
%% (the name of the key and its value), and return either the processed
%% value or `{error, _Why}'. If it returns an error, the processing stops
%% there, and that will be the value returned by `process_qs_vals'
-spec process_qs_vals(cowboy_req(), ProcessingFun, list(binary()) | list({binary(), any()}))
                     -> {ok, cowboy_req(), proplist(binary(), any())} | {error, _Why} when
      ProcessingFun :: fun((binary(), binary() | true | undefined) -> any() | {error, _Why}).
process_qs_vals(Req0, ProcessingFun, QsKeys) ->
    lists:foldl(
      fun(_QsKey, {error, _} = Error) ->
              Error;
         ({QsKey, Default}, {ok, Req1, ProcessedValues}) ->
              {Value, Req2} = cowboy_req:qs_val(QsKey, Req1, Default),
              exec_processing_fun(ProcessingFun, QsKey, Value, ProcessedValues, Req2)
      end,
      {ok, Req0, []},
      lists:map(fun({_,_} = Val) -> Val; (Val) -> {Val, undefined} end, QsKeys)
     ).

exec_processing_fun(ProcessingFun, QsKey, Value, ProcessedValues, Req2) ->
    case ProcessingFun(QsKey, Value) of
        {error, _Why} = Error ->
            Error;
        ProcessedValue ->
            {ok, Req2, [{QsKey, ProcessedValue} | ProcessedValues]}
    end.

%% @doc Same as `process_qs_vals/3', except the keys of the query string's
%% params simply are the ones which happen to be present in the URL, you don't
%% give them out explicitely
-spec process_qs_vals(cowboy_req(), ProcessingFun)
                     -> {ok, cowboy_req(), proplist(binary(), any())} | {error, _Why} when
      ProcessingFun :: fun((binary(), binary() | true | undefined) -> any() | {error, _Why}).
process_qs_vals(Req0, ProcessingFun) ->
    {QsVals, Req1} = cowboy_req:qs_vals(Req0),
    QsKeys = proplists:get_keys(QsVals),
    process_qs_vals(Req1, ProcessingFun, QsKeys).

%% @doc Given a req, extract ent_name, org_name, proj_name,
%% change_id, and return them along with modified req
%% TODO: This may be poorly named, as it assumes all these fields,
%% including change_id exists.
-spec extract_scoping_names(cowboy_req())
                           -> {cowboy_req(), binary(), binary(), binary(), binary()}.
extract_scoping_names(Req) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {OrgName, Req2} = cowboy_req:binding(org_name, Req1),
    {ProjName, Req3} = cowboy_req:binding(proj_name, Req2),
    {ChangeId, Req4} = cowboy_req:binding(change_id, Req3),
    {Req4, EntName, OrgName, ProjName, ChangeId}.

%% @doc Given a req, extract a #proj_coordinates{} record
%% and return it along with (hypothetically) modified req.
%% This should work for all cases including absent bindings
%% because cowboy_req:binding returns 'undefined' then,
%% which is also the default not-set value for a record field
-spec extract_proj_coordinates(cowboy_req())
                               -> {#proj_coordinates{}, cowboy_req()}.
extract_proj_coordinates(Req) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {OrgName, Req2} = cowboy_req:binding(org_name, Req1),
    {ProjName, Req3} = cowboy_req:binding(proj_name, Req2),
    {#proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}, Req3}.

%% @doc Given a list of binding names as atoms, return a list of
%% their values and the modified req.
-spec extract_bindings([atom()], cowboy_req()) -> {[binary()], cowboy_req()}.
extract_bindings(Bindings, Req) ->
    {Values, Req2} = lists:foldl(
        fun(Binding, {Values, ReqAcc}) ->
            {Value, ReqAcc1} = cowboy_req:binding(Binding, ReqAcc),
            {[Value | Values], ReqAcc1}
        end,
        {[], Req},
        Bindings
    ),
    %% Reverse so they are in the same order as the atom list that came in
    Values2 = lists:reverse(Values),
    {Values2, Req2}.

-spec reply_unauthorized(cowboy_req(), req_handler()) -> {halt, cowboy_req(), req_handler()}.
reply_unauthorized(Req, State) ->
    Body = chef_json:encode({[{<<"error">>, <<"unauthorized">>}]}),
    Req1 = cowboy_req:set_resp_body(Body, Req),
    Req2 = cowboy_req:set_resp_header(<<"www-authenticate">>, ?WWW_AUTHENTICATE, Req1),
    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
    {ok, Req4} = cowboy_req:reply(401, Req3),
    {halt, Req4, State}.

-spec(http_post_retry(Request :: term(), Retries :: integer(), Interval :: integer()) -> ok).
http_post_retry(_Request, 0, _) ->
    ok;
http_post_retry(Request, Retries, Interval) ->
    case httpc:request(post, Request, [], [{sync, true}]) of
        {error, Reason} ->
            chef_log:error("HTTP request failed: ~p", [Reason]),
            timer:sleep(Interval * 1000),
            http_post_retry(Request, Retries - 1, Interval);
        {ok, {{_, 204, Response}, _Headers, _Body}} ->
            chef_log:info("HTTP response: 204 ~p", [Response]),
            ok;
        {ok,{Response, _Headers, _Body}} ->
            chef_log:info("HTTP response:~p", [Response]),
            http_post_retry(Request, Retries -1, Interval)
    end.

%% @doc encode a URL with its arguments into one sring
-spec(encode_url_parameters([{string(), string()}]) -> string()).
encode_url_parameters(Attributes) ->
    encode_url_parameters(Attributes, "").

encode_url_parameters([], Acc) ->
    Acc;
encode_url_parameters([{Key, Value} | Rest], "") when is_list(Key),
                                           is_list(Value) ->
    encode_url_parameters(Rest, encode_url(Key) ++ "=" ++ encode_url(Value));
encode_url_parameters([{Key, Value} | Rest], Acc) when is_list(Key),
                                            is_list(Value) ->
    encode_url_parameters(Rest, Acc ++ "&" ++ encode_url(Key) ++ "=" ++ encode_url(Value)).

%% @doc translate a http status code into a readable atom
-spec status_to_atom(non_neg_integer()) -> atom().
status_to_atom(200) -> ok;
status_to_atom(201) -> created;
status_to_atom(202) -> accepted;
status_to_atom(204) -> no_content;
status_to_atom(301) -> moved_permanently;
status_to_atom(304) -> not_modified;
status_to_atom(400) -> bad_request;
status_to_atom(401) -> unauthorized;
status_to_atom(403) -> forbidden;
status_to_atom(404) -> not_found;
status_to_atom(409) -> conflict;
status_to_atom(500) -> internal_server_error;
status_to_atom(_Status) -> unhandled_status.

-spec validate_url_is_well_formed(binary()) -> atom().
validate_url_is_well_formed(Url) ->
    case http_uri:parse(chef_utils:to_str(Url)) of
        {ok, _} -> ok;
        {error, _} -> error
    end.
