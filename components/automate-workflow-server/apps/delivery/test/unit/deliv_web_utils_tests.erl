%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(deliv_web_utils_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile([export_all]).

parse_json_req_test_() ->
    {
      setup,
      fun() ->
              meck:new(cowboy_req),
              meck:expect(cowboy_req, body, fun(X) -> {ok, X, X} end)
      end,
      fun(_) ->
              ct_meck:unload(cowboy_req)
      end,
      fun() ->
              %% basic functionality
              {String, Json} = example_json_and_string(),
              ?assertEqual(
                 {Json, String},
                 deliv_web_utils:parse_json_req(String)
                ),
              %% with a matched spec
              MatchedSpec = {[
                              {<<"type">>, <<"object">>},
                              {<<"properties">>, {[
                                                   {<<"bin">>, {[
                                                                 {<<"type">>, <<"string">>}
                                                                ]}},
                                                   {<<"bool">>, {[
                                                                  {<<"type">>, <<"boolean">>}
                                                                 ]}},
                                                   {<<"list">>, {[
                                                                  {<<"type">>, <<"array">>}
                                                                 ]}}
                                                  ]} }
                             ]},
              ?assertEqual(
                 {Json, String},
                 deliv_web_utils:parse_json_req(String, MatchedSpec)
                ),
              %% with an unmatched spec
              UnmatchedSpec = ej:set([<<"properties">>, <<"bin">>, <<"type">>], MatchedSpec, <<"boolean">>),
              ?assertMatch(
                 {{error, {_Invalid, Json}}, String},
                 deliv_web_utils:parse_json_req(String, UnmatchedSpec)
                ),
              %% with an invalid Json
              ?assertEqual(
                 {{error, invalid_json}, <<"i'm not a json!">>},
                 deliv_web_utils:parse_json_req(<<"i'm not a json!">>, MatchedSpec)
                )
      end
    }.

read_body_handles_more_test() ->
    hoax:test(fun() ->
        Data1 = <<"{ \"first\": true,">>,
        Data2 = <<" \"second\": 24,">>,
        Data3 = <<" \"third\": \"bar\"}">>,
        Expected_String = <<"{ \"first\": true, \"second\": 24, \"third\": \"bar\"}">>,
        hoax:mock(cowboy_req, [
                  ?expect(body,
                          ?withArgs([mock_req1]),
                          ?andReturn({more, Data1, mock_req2})),
                  ?expect(body,
                          ?withArgs([mock_req2]),
                          ?andReturn({more, Data2, mock_req3})),
                  ?expect(body,
                          ?withArgs([mock_req3]),
                          ?andReturn({ok, Data3, mock_req4}))
                 ]),
        Actual = deliv_web_utils:read_body(mock_req1),
        ?assertEqual({Expected_String, mock_req4}, Actual)
    end).

read_body_returns_error_when_cowboy_returns_error_test() ->
    hoax:test(fun() ->
        hoax:mock(cowboy_req,
                  ?expect(body, ?withArgs([mock_req]), ?andReturn({error, reset}))),

        Actual = deliv_web_utils:read_body(mock_req),
        ?assertEqual({error, reset}, Actual)
    end).

url_decode_encode_test_() ->
    binary_or_list_test(
      fun(Cast) ->
              Str = Cast("wk%/\~coucou\\/url"),
              EncodedStr = deliv_web_utils:encode_url(Str),
              DecodedStr = deliv_web_utils:decode_url(EncodedStr),
              [
               ?_assertEqual(Str, DecodedStr),
               ?_assertEqual(Cast("wk%25%2F~coucou%5C%2Furl"), EncodedStr)
              ]
      end
     ).

url_decode_undefined_test() ->
    DecodedStr = deliv_web_utils:decode_url(undefined),
    ?assertEqual(undefined, DecodedStr).


encode_url_rfc1738_test_() ->
    binary_or_list_test(
      fun(Cast) ->
          Str = Cast(";Rt#O)+d$9yzwe3[yhQ*A"),
          EncodedStr = deliv_web_utils:encode_url_rfc1738(Str),
          ?_assertEqual(Cast("%3bRt%23O%29%2bd%249yzwe3%5byhQ%2aA"), EncodedStr)
      end
     ).

extract_header_test_() ->
    binary_or_list_test(
      fun(Cast) ->
              HeaderName = Cast("content-LengTh"),
              BaseHeaders = [{Cast("content-type"), Cast("application/json")}],
              [
               ?_assertEqual(ExpectedResult, deliv_web_utils:extract_header(HeaderName, Headers))
               || {Headers, ExpectedResult} <- [
                                                {[], not_found},
                                                {BaseHeaders, not_found},
                                                {BaseHeaders ++ [{Cast("Content-Length"), 12}], 12},
                                                {[{Cast("content-length"), 28}] ++ BaseHeaders, 28}
                                               ]
              ]
      end
     ).

binary_or_list_test(Test) ->
    [
     Test(fun(X) -> X end),
     Test(fun erlang:iolist_to_binary/1)
    ].

make_hal_test() ->
    Input = [{link1, <<"/abc">>},
             {<<"link2">>, <<"/xyz">>},
             {tlink1, {t, <<"/a/{b}">>}},
             {<<"tlink2">>, {t, <<"/x/{y}">>}}],
    HAL = deliv_web_utils:make_hal(Input),
    Expect = {[
               {<<"link1">>,
                {[{<<"href">>, <<"/abc">>}]}},

               {<<"link2">>,
                {[{<<"href">>, <<"/xyz">>}]}},

               {<<"tlink1">>,
                {[{<<"href">>, <<"/a/{b}">>},
                  {<<"templated">>, true}]}},

               {<<"tlink2">>,
                {[{<<"href">>, <<"/x/{y}">>},
                  {<<"templated">>, true}]}}
              ]},
    ?assertEqual(Expect, HAL).

href_test() ->
    Got = deliv_web_utils:href(<<"acme">>, [<<"/orgs/">>, <<"eng">>]),
    ?assertEqual(<<"/api/v0/e/acme/orgs/eng">>, Got).



translate_proplist_test() ->
    %% translating keys only
    PropList1 = [
                 {wk, <<"Chef">>},
                 {foo, bar},
                 {ignored, <<"bouh">>},
                 {blah, blih}
                ],
    TranslatingFun1 = fun(ignored) -> ignore;
                         (unexpected_field) -> {error, unexpected_field};
                         (Atom) -> {key, atom_to_binary(Atom, utf8)}
                      end,
    Expected1 = [
                 {<<"wk">>, <<"Chef">>},
                 {<<"foo">>, bar},
                 {<<"blah">>, blih}
                ],
    ?assertEqual({ok, Expected1}, deliv_web_utils:translate_proplist(PropList1, TranslatingFun1)),
    %% let's add an unexpected field
    PropList2 = [
                 {unexpected_field, 42}
                 | PropList1
                ],
    ?assertEqual(
       {error, unexpected_field},
       deliv_web_utils:translate_proplist(PropList2, TranslatingFun1)
      ),
    %% and let's check with a function of arity 2, that also translates some values
    TranslatingFun2 = fun(honey_badger, _Whatever) -> {error, <<"honeybadger don't care">>};
                         (foo, foo) -> {both, {bar, bar}};
                         (blah, _Value) -> {value, blouh};
                         (_Key, _Value) -> ignore
                      end,
    PropList3 = [
                 {wk, <<"Chef">>},
                 {foo, foo},
                 {blah, blah},
                 {po, 12}
                ],
    Expected2 = [{bar, bar}, {blah, blouh}],
    ?assertEqual({ok, Expected2}, deliv_web_utils:translate_proplist(PropList3, TranslatingFun2)),
    %% honeybadger don't care
    ?assertEqual(
       {error, <<"honeybadger don't care">>},
       deliv_web_utils:translate_proplist(PropList3 ++ [{honey_badger, cares}], TranslatingFun2)
      ).

make_api_url_prefix_test_() ->
    {
      foreach,
      fun() ->
              meck:new(delivery_app),
              meck:expect(delivery_app, get_env,
                          fun(hostname) ->
                                  "192.168.33.66";
                             (api_proto) ->
                                  "https"
                          end)
      end,
      fun(_) ->
              ct_meck:unload(delivery_app)
      end,
      [
       {"returns https api url when that's the proto",
        fun() ->
                ?assertEqual(<<"https://192.168.33.66/api/v0/e/">>,
                             deliv_web_utils:make_api_url_prefix())
        end}
      ]}.

make_api_url_prefix_arity_1_test_() ->
      {
        foreach,
        fun() ->
                meck:new(delivery_app),
                meck:expect(delivery_app, get_env,
                            fun(hostname) ->
                                    "192.168.33.66";
                               (api_proto) ->
                                    "https"
                            end)
        end,
        fun(_) ->
                ct_meck:unload(delivery_app)
        end,
        [
         {"returns https api url when that's the proto",
          fun() ->
                  ?assertEqual(<<"https://192.168.33.66/api/v0/e/my_ent/">>,
                               deliv_web_utils:make_api_url_prefix(<<"my_ent">>))
          end}
        ]}.

make_web_url_for_change_test_() ->
    {
      foreach,
      fun() ->
              meck:new(delivery_app),
              meck:expect(delivery_app, get_env,
                          fun(hostname) ->
                                  "192.168.33.66";
                             (api_proto) ->
                                  "https"
                          end)
      end,
      fun(_) ->
              ct_meck:unload(delivery_app)
      end,
      [
       {"returns https web url when that's the proto",
        fun() ->
                ?assertEqual(<<"https://192.168.33.66/e/NCC1701D/#/organizations/",
                               "bridge/projects/helm/changes/",
                               "450f1986-e2e3-45e8-9fb1-a029133999ff">>,
                             deliv_web_utils:make_web_url_for_change(
                               <<"NCC1701D">>, <<"bridge">>, <<"helm">>,
                               <<"450f1986-e2e3-45e8-9fb1-a029133999ff">>))
        end}
      ]}.

make_web_url_for_base_test_() ->
    {
      foreach,
      fun() ->
              meck:new(delivery_app),
              meck:expect(delivery_app, get_env,
                          fun(hostname) ->
                                  "192.168.33.66";
                             (api_proto) ->
                                  "https"
                          end)
      end,
      fun(_) ->
              ct_meck:unload(delivery_app)
      end,
      [
       {"returns https web url when that's the proto",
        fun() ->
                ?assertEqual(<<"https://192.168.33.66/e/NCC1701D/">>,
                             deliv_web_utils:make_web_url_for_base(
                               <<"NCC1701D">>))
        end}
      ]}.

make_web_url_for_login_test_() ->
    {
      foreach,
      fun() ->
              meck:new(delivery_app),
              meck:expect(delivery_app, get_env,
                          fun(hostname) ->
                                  "192.168.33.66";
                             (api_proto) ->
                                  "https"
                          end)
      end,
      fun(_) ->
              ct_meck:unload(delivery_app)
      end,
      [
       {"returns https web url when that's the proto",
        fun() ->
                ?assertEqual(<<"https://192.168.33.66/e/NCC1701D/#/login">>,
                             deliv_web_utils:make_web_url_for_login(
                               <<"NCC1701D">>))
        end}
      ]}.

make_web_url_for_dashboard_test_() ->
    {
      foreach,
      fun() ->
              meck:new(delivery_app),
              meck:expect(delivery_app, get_env,
                          fun(hostname) ->
                                  "192.168.33.66";
                             (api_proto) ->
                                  "https"
                          end)
      end,
      fun(_) ->
              ct_meck:unload(delivery_app)
      end,
      [
       {"returns https web url when that's the proto",
        fun() ->
                ?assertEqual(<<"https://192.168.33.66/e/NCC1701D/#/dashboard">>,
                             deliv_web_utils:make_web_url_for_dashboard(<<"NCC1701D">>))
        end}
      ]}.

api_url_for_test_() ->
    {setup,
     fun() ->
             meck:new(delivery_app),
             meck:expect(delivery_app, get_env,
                         fun(hostname) ->
                                 "127.0.0.1";
                            (api_proto) ->
                                 "http"
                         end)
     end,
     fun(_) ->
             ct_meck:unload(delivery_app)
     end,
     [
      ?_assertEqual(<<"http://127.0.0.1/api/v0/e"
                      "/Chef/orgs/Chef_Delivery"
                      "/projects/delivery"
                      "/changes"
                      "/8725f96f-838c-4add-897e-06a48894382"
                      "/trigger/verify">>,
                    deliv_web_utils:api_url_for(trigger_stage,
                                                [<<"Chef">>,
                                                 <<"Chef_Delivery">>,
                                                 <<"delivery">>,
                                                 <<"8725f96f-838c-4add-897e-06a48894382">>,
                                                 <<"verify">>])),
      ?_assertEqual(<<"http://127.0.0.1/api/v0/e"
                      "/Chef/orgs/Chef_Delivery"
                      "/projects/delivery"
                      "/pipelines/master"
                      "/phase_runs/4726">>,
                    deliv_web_utils:api_url_for(phase_run,
                                                ["Chef", <<"Chef_Delivery">>,
                                                 "delivery",
                                                 "master",
                                                 "4726"])),
      ?_assertEqual(<<"http://127.0.0.1/api/v0/e"
                      "/Chef/jobs/job-id">>,
                    deliv_web_utils:api_url_for(phase_run_job,
                                                ["Chef", <<"job-id">>])),
      ?_assertEqual(<<"http://127.0.0.1/api/v0/e"
                      "/Chef/saml/consume">>,
                    deliv_web_utils:api_url_for(saml_consume,
                                                ["Chef"])),
      ?_assertEqual(<<"http://127.0.0.1/api/v0/e"
                      "/Chef/saml/metadata">>,
                    deliv_web_utils:api_url_for(saml_metadata,
                                                ["Chef"]))
     ]}.

set_cookie_test() ->
    hoax:test(fun() ->
                 CookieKey = <<"MyKey">>,
                 CookieValue = <<"MyVal">>,
                 CookiePath = <<"/">>,
                 CookieSecure = false,
                 CookieOpts = [{path, CookiePath}],

                 hoax:mock(cowboy_req, [
                           ?expect(set_resp_cookie,
                                   ?withArgs([CookieKey, CookieValue, CookieOpts, req]),
                                   ?andReturn(req2))]),

                 ActualResult = deliv_web_utils:set_cookie(CookieKey, CookieValue, CookieSecure, req),
                 ?assertEqual({ok, req2}, ActualResult),
                 ?verifyAll
              end),
    hoax:test(fun() ->
                   CookieKey = <<"MyKey">>,
                   CookieValue = <<"MyVal">>,
                   CookiePath = <<"/">>,
                   CookieSecure = true,
                   CookieOpts = [{path, CookiePath}, {secure, CookieSecure}],

                   hoax:mock(cowboy_req, [
                             ?expect(set_resp_cookie,
                                     ?withArgs([CookieKey, CookieValue, CookieOpts, req]),
                                     ?andReturn(req2))]),

                   ActualResult = deliv_web_utils:set_cookie(CookieKey, CookieValue, CookieSecure, req),
                   ?assertEqual({ok, req2}, ActualResult),
                   ?verifyAll
               end).

extract_proj_coordinates_test() ->
    hoax:test(fun() ->
                hoax:mock(cowboy_req, [
                           ?expect(binding,
                                   ?withArgs([ent_name, in_req]),
                                   ?andReturn({url_ent_name, req2})),
                           ?expect(binding,
                                   ?withArgs([org_name, req2]),
                                   ?andReturn({url_org_name, req3})),
                           ?expect(binding,
                                   ?withArgs([proj_name, req3]),
                                   ?andReturn({url_proj_name, req4}))
                        ]),

                ActualResult = deliv_web_utils:extract_proj_coordinates(in_req),

                ?assertEqual({#proj_coordinates{ent_name = url_ent_name,
                                                org_name = url_org_name,
                                                proj_name = url_proj_name},
                              req4}, ActualResult),

                ?verifyAll
              end).

extract_bindings_test() ->
    hoax:test(fun() ->
                hoax:mock(cowboy_req, [
                           ?expect(binding,
                                   ?withArgs([ent_name, in_req]),
                                   ?andReturn({url_ent_name, req2})),
                           ?expect(binding,
                                   ?withArgs([org_name, req2]),
                                   ?andReturn({url_org_name, req3})),
                           ?expect(binding,
                                   ?withArgs([proj_name, req3]),
                                   ?andReturn({url_proj_name, req4}))
                        ]),

                ActualResult = deliv_web_utils:extract_bindings([ent_name, org_name, proj_name], in_req),

                ?assertEqual({[url_ent_name, url_org_name, url_proj_name], req4}, ActualResult),

                ?verifyAll
              end).

validate_url_is_well_formed_test_() ->
    hoax:fixture(?MODULE, "validate_url_is_well_formed_").

validate_url_is_well_formed_returns_ok_when_url_is_valid() ->
    ?assertEqual(ok, deliv_web_utils:validate_url_is_well_formed(<<"http://example.com">>)).

validate_url_is_well_formed_returns_error_when_url_is_invalid() ->
    ?assertEqual(error, deliv_web_utils:validate_url_is_well_formed(<<"nope">>)).

make_web_url_for_project_builds_url_test() ->
    hoax:test(fun() ->
        EntName = <<"HankCo">>,
        OrgName = <<"SuperWorld">>,
        ProjName = <<"HankBucks">>,

        hoax:mock(delivery_app, [
                   ?expect(get_env,
                           ?withArgs([api_proto]),
                           ?andReturn("https")),
                   ?expect(get_env,
                           ?withArgs([hostname]),
                           ?andReturn("127.0.0.1"))
                ]),

        Result = deliv_web_utils:make_web_url_for_project(EntName, OrgName, ProjName),

        ProjectPageUrl = <<"https://127.0.0.1/e/HankCo/#/organizations/SuperWorld/projects/HankBucks/changes">>,

        ?assertEqual(ProjectPageUrl, Result),
        ?verifyAll
      end).

example_json_and_string() ->
    {
        <<"{\"bin\":\"wk\",\"bool\":true,\"list\":[42,\"ok\"]}">>,
        {[{<<"bin">>, <<"wk">>}, {<<"bool">>, true}, {<<"list">>, [42, <<"ok">>]}]}
    }.
