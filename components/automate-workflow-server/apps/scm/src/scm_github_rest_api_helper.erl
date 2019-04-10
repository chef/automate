-module(scm_github_rest_api_helper).

-export([root_api_url/1]).

-spec root_api_url(binary()) -> binary().
root_api_url(Url) ->
    {ok, {Scheme, _, Host, Port, _Path, _}} = http_uri:parse(chef_utils:to_str(Url)),
    chef_utils:to_bin([chef_utils:to_bin(Scheme), "://",
                       maybe_convert_host(Host),
                       ":",
                       chef_utils:to_bin(Port),
                       maybe_append_v3(Host)]).

-spec maybe_convert_host(string()) -> string().
maybe_convert_host("github.com") -> "api.github.com";
maybe_convert_host(Host) -> Host.

-spec maybe_append_v3(string()) -> string().
maybe_append_v3("api.github.com") -> "/";
maybe_append_v3("github.com") -> "/";
maybe_append_v3(_Host) -> "/api/v3/".
