-module(deliv_http_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("test_support/include/test_support.hrl").

-compile([export_all]).

req_test_() ->
    hoax:fixture(?MODULE, "req_").

req_accepts_an_iodata_as_input_for_the_http_url() ->
    UrlIOList = [<<"ht">>, ["tp", ["://", <<"example.com">>], "/route"]],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, non_ssl_expected_options()]),
                      ?andReturn(ibrowse_success_result()))),
    deliv_http:req(UrlIOList),
    ?verifyAll.

req_automatically_encodes_ejson_and_adds_the_content_type_header() ->
    ExpectedHeaders = [{"content-type", "application/json"}],
    EjsonBody = {[{<<"key">>, <<"value">>}]},
    BinBody = chef_json:encode(EjsonBody),
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), ExpectedHeaders, post, BinBody, non_ssl_expected_options()]),
                      ?andReturn(ibrowse_success_result()))),
    deliv_http:req(post, http_url(), EjsonBody),
    ?verifyAll.

req_accepts_binary_headers_and_turns_them_to_strings() ->
    Headers = [{"content-type", <<"application/json">>},
               {<<"host">>, "example.com"}],
    ExpectedHeaders = [{"content-type", "application/json"},
                       {"host", "example.com"}],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), ExpectedHeaders, post, <<>>, non_ssl_expected_options()]),
                      ?andReturn(ibrowse_success_result()))),
    deliv_http:req(post, http_url(), <<>>, Headers),
    ?verifyAll.

req_turns_the_status_code_into_an_integer_and_returns_a_binary_body() ->
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, non_ssl_expected_options()]),
                      ?andReturn(ibrowse_success_result()))),
    ?assertEqual({ok, 200, ["header-name","header-value"], <<"a body">>},
                 deliv_http:req(http_url())),
    ?verifyAll.

req_just_passes_along_any_ibrowse_error() ->
    Error = {error, something_went_bad},
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, non_ssl_expected_options()]),
                      ?andReturn(Error))),
    ?assertEqual(Error, deliv_http:req(http_url())),
    ?verifyAll.

req_automatically_turns_SSL_cert_validation_on_if_passed_an_https_address() ->
    CertFilePath = "CA_Certs/file/path",
    CertDepth = 5,
    VerifyFun = fun deliv_ssl_verify_functions:verify_selfsigned_cert/3,

    application:set_env(delivery, trusted_certificates_file, CertFilePath),
    application:set_env(delivery, ca_cert_chain_depth, CertDepth),

    ExpectedSslOptions = [{response_format, binary},
                          {is_ssl, true},
                          {ssl_options, [{verify_fun, {VerifyFun, CertFilePath}},
                                         {cacertfile, CertFilePath},
                                         {depth, CertDepth}]}],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([https_url(), [], get, <<>>, ExpectedSslOptions]),
                      ?andReturn(ibrowse_success_result()))),
    deliv_http:req(https_url()),

    ?assertFunctionExported(VerifyFun),
    ?verifyAll.

req_automatically_turns_SSL_cert_validation_on_if_passed_an_https_address_that_is_not_on_the_no_verify_list() ->
    CertFilePath = "CA_Certs/file/path",
    CertDepth = 5,
    VerifyFun = fun deliv_ssl_verify_functions:verify_selfsigned_cert/3,

    application:set_env(delivery, trusted_certificates_file, CertFilePath),
    application:set_env(delivery, ca_cert_chain_depth, CertDepth),
    application:set_env(delivery, no_ssl_verification, ["trusted.internal.host"]),

    ExpectedSslOptions = [{response_format, binary},
                          {is_ssl, true},
                          {ssl_options, [{verify_fun, {VerifyFun, CertFilePath}},
                                         {cacertfile, CertFilePath},
                                         {depth, CertDepth}]}],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([https_url(), [], get, <<>>, ExpectedSslOptions]),
                      ?andReturn(ibrowse_success_result()))),
    deliv_http:req(https_url()),

    ?assertFunctionExported(VerifyFun),
    ?verifyAll.

req_automatically_turns_tls_cert_validation_off_if_passed_an_https_address_whose_hostname_is_on_the_no_verification_list() ->
    CertFilePath = "CA_Certs/file/path",
    CertDepth = 5,
    Url = "https://trusted.internal.host/some/endpoint",

    application:set_env(delivery, trusted_certificates_file, CertFilePath),
    application:set_env(delivery, ca_cert_chain_depth, CertDepth),
    application:set_env(delivery, no_ssl_verification, ["trusted.internal.host"]),

    ExpectedSslOptions = [{response_format, binary},
                          {is_ssl, true},
                          {ssl_options, [{verify, verify_none}]}
                         ],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([Url, [], get, <<>>, ExpectedSslOptions]),
                      ?andReturn(ibrowse_success_result()))),
    deliv_http:req(Url),
    ?verifyAll.

req_pass_proxy_host_when_present() ->
    application:set_env(delivery, proxy_host, proxy_host()),
    application:set_env(delivery, proxy_port, proxy_port()),
    application:set_env(delivery, proxy_user, ""),
    application:set_env(delivery, proxy_password, ""),
    application:set_env(delivery, no_proxy, ""),

    ExpectedProxyOptions = [{response_format, binary},
                            {is_ssl, false},
                            {proxy_host, proxy_host()},
                            {proxy_port, proxy_port()}],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, ExpectedProxyOptions]),
                      ?andReturn(ibrowse_success_result()))),

    deliv_http:req(http_url()),
    ?verifyAll.

req_pass_proxy_host_and_no_proxy_when_present() ->
    application:set_env(delivery, proxy_host, proxy_host()),
    application:set_env(delivery, proxy_port, proxy_port()),
    application:set_env(delivery, proxy_user, ""),
    application:set_env(delivery, proxy_password, ""),
    application:set_env(delivery, no_proxy, no_proxy()),

    ExpectedProxyOptions = [{response_format, binary},
                            {is_ssl, false},
                            {proxy_host, proxy_host()},
                            {proxy_port, proxy_port()}],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, ExpectedProxyOptions]),
                      ?andReturn(ibrowse_success_result()))),

    deliv_http:req(http_url()),
    ?verifyAll.

req_pass_proxy_host_and_user_when_present() ->
    application:set_env(delivery, proxy_host, proxy_host()),
    application:set_env(delivery, proxy_port, proxy_port()),
    application:set_env(delivery, proxy_user, proxy_user()),
    application:set_env(delivery, proxy_password, proxy_password()),
    application:set_env(delivery, no_proxy, ""),

    ExpectedProxyOptions = [{response_format, binary},
                            {is_ssl, false},
                            {proxy_host, proxy_host()},
                            {proxy_port, proxy_port()},
                            {proxy_user, proxy_user()},
                            {proxy_password, proxy_password()}],
    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, ExpectedProxyOptions]),
                      ?andReturn(ibrowse_success_result()))),

    deliv_http:req(http_url()),
    ?verifyAll.

req_pass_ignore_proxy_if_host_not_present() ->
    application:set_env(delivery, proxy_host, ""),
    application:set_env(delivery, proxy_port, proxy_port()),
    application:set_env(delivery, proxy_user, proxy_user()),
    application:set_env(delivery, proxy_password, proxy_password()),
    application:set_env(delivery, no_proxy, no_proxy()),

    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, non_ssl_expected_options()]),
                      ?andReturn(ibrowse_success_result()))),

    deliv_http:req(http_url()),
    ?verifyAll.

reg_pass_ignore_proxy_if_req_matches_no_proxy() ->
    application:set_env(delivery, proxy_host, proxy_host()),
    application:set_env(delivery, proxy_port, proxy_port()),
    application:set_env(delivery, proxy_user, proxy_user()),
    application:set_env(delivery, proxy_password, proxy_password()),
    application:set_env(delivery, no_proxy, ["example.com"]),

    hoax:mock(ibrowse,
              ?expect(send_req,
                      ?withArgs([http_url(), [], get, <<>>, non_ssl_expected_options()]),
                      ?andReturn(ibrowse_success_result()))),

    deliv_http:req(http_url()),
    ?verifyAll.

%% Helpers
http_url() ->
    "http://example.com/route".

https_url() ->
    "https://example.com/route".

proxy_host() ->
    "myproxy.proxy.net".

proxy_port() ->
    1234.

proxy_user() ->
    "foo".

proxy_password() ->
    "1234abcd".

no_proxy() ->
    ["1.2.3.4", "abcde"].

ibrowse_success_result() ->
    {ok, "200", ["header-name", "header-value"], <<"a body">>}.

non_ssl_expected_options() ->
    [{response_format, binary}, {is_ssl, false}].
