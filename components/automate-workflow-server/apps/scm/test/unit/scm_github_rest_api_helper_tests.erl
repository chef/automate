-module(scm_github_rest_api_helper_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

root_api_url_converts_urls_when_needed_test() ->
    Conversions = [
                   [<<"http://github.com">>, <<"http://api.github.com:80/">>],
                   [<<"https://github.com/">>, <<"https://api.github.com:443/">>],
                   [<<"https://api.github.com/">>, <<"https://api.github.com:443/">>],
                   [<<"http://my-ent-github.com">>, <<"http://my-ent-github.com:80/api/v3/">>],
                   [<<"https://my-ent-github-install.com">>, <<"https://my-ent-github-install.com:443/api/v3/">>],
                   [<<"http://weird-port-github.co:1234/">>, <<"http://weird-port-github.co:1234/api/v3/">>]
                  ],
    [ ?assertEqual(RootApi, scm_github_rest_api_helper:root_api_url(Url)) || [Url, RootApi] <- Conversions ].
