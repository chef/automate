-module(scm_github_rest_api).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include("scm_types.hrl").

-export([ensure_pull_request/4,
         check_reachability/2]).

-spec ensure_pull_request(d_patchset(), binary(), binary(), #proj_coordinates{}) -> {ok, term()} |
                                                                                    {exists, term()} |
                                                                                    {error, term()}.
ensure_pull_request(Patchset, SourceBranch, DestinationBranch, Coords = #proj_coordinates{ent_name = EntName}) ->
    {ok, BasicAuth} = scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>),
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),

    Url = repo_url(Coords),
    UserDescription = deliv_user:user_description_for_pr_comment(EntName, deliv_change:getval(submitted_by, Change)),
    RequestBody = pull_request_body(Change, SourceBranch, DestinationBranch, Coords, UserDescription),
    Token = deliv_basic_auth_application:getval(password, BasicAuth),
    Headers = request_headers(Token),

    case deliv_http:req(post, Url, RequestBody, Headers) of
        {ok, 201, _Head, ResponseBody} ->
            BodyEJson = chef_json:decode(ResponseBody),
            {ok, BodyEJson};
        {ok, 422, _Head, ResponseBody} ->
            BodyEJson = chef_json:decode(ResponseBody),
            Message = handle_response_message(ej:get({<<"errors">>, first, <<"message">>}, BodyEJson), ChangeId),
            {exists, Message};
        {ok, ResCode, _Head, _ResponseBody} ->
            BinCode = chef_utils:to_bin(ResCode),
            chef_log:info("Failed to open a Pull Request in GitHub. ResponseCode=~p Url=~p", [ResCode, Url]),
            {error, <<"Failed to open a Pull Request in GitHub with response code ", BinCode/binary, ".">>};
        {error, Why} ->
            {error, <<"Failed to open a Pull Request in GitHub with error: ", Why/binary>>}
    end.

-spec check_reachability(binary(), binary()) -> {ok, authenticated} | {error, unauthorized} | {error, term()}.
check_reachability(RootUrl, Token) ->
    GitHubUrl = scm_github_rest_api_helper:root_api_url(RootUrl),
    Headers = request_headers(Token),

    case deliv_http:req(get, GitHubUrl, [], Headers) of
        {ok, 200, _Head, _ResponseBody} ->
            {ok, authenticated};
        {ok, 401, _Head, _ResponseBody} ->
            {error, unauthorized};
        {ok, 400, _Head, _ResponseBody} ->
            {error, bad_request};
        {ok, 301, ResponseHeaders, _ResponseBody} ->
            Location = proplists:get_value("Location", ResponseHeaders),
            {error, {redirect, Location}};
        {ok, 404, _Head, _ResponseBody} ->
            {error, not_found};
        {error, Why} ->
            {error, Why}
    end.

-spec repo_url(#proj_coordinates{}) -> binary().
repo_url(Coords = #proj_coordinates{ent_name = EntName}) ->
    {ok, BasicAuth} = scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>),
    RootUrl = deliv_basic_auth_application:getval(root_api_url, BasicAuth),

    {ok, #metadata_by_scm{ repo_name = GitHubRepo, repo_group = GitHubOwner }} = scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords),

    iolist_to_binary([scm_github_rest_api_helper:root_api_url(RootUrl),
                      "repos/", GitHubOwner, "/",
                      GitHubRepo, "/pulls"]).

-spec pull_request_body(d_change(), binary(), binary(), #proj_coordinates{}, binary()) -> ej:json_object().
pull_request_body(Change, SourceBranch, DestinationBranch, #proj_coordinates{ent_name = EntName,
                                                                             org_name = OrgName,
                                                                             proj_name = ProjName}, UserDescription) ->
    ChangeDescription = deliv_change:getval(description, Change),
    ChangeId = deliv_change:getval(id, Change),
    Url = deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId),

    Description = <<UserDescription/binary, ChangeDescription/binary,
                    "\n\n----\nReady to merge? [View this change](", Url/binary,
                    ") in Chef Automate and click the Approve button.">>,

    {[{<<"title">>, deliv_change:getval(title, Change)},
      {<<"head">>, SourceBranch},
      {<<"base">>, DestinationBranch},
      {<<"body">>, Description}]}.

-spec request_headers(binary()) -> http_headers().
request_headers(Token) ->
    [{<<"Authorization">>, <<"Token ", Token/binary>>},
     {<<"User-Agent">>, <<"Chef-Automate-API">>}].

-spec handle_response_message(undefined | binary(), binary()) -> binary().
handle_response_message(undefined, ChangeId) ->
        case scm_change:fetch_by_change_id(ChangeId) of
            {ok, ScmMetadata} ->
                PRUrl = scm_change:getval(pr_url, ScmMetadata),
                <<"GitHub Pull Request: ", PRUrl/binary>>;
            {error, _Why} ->  <<"Failed to open a Pull Request in GitHub with response code 422.">>
        end;
handle_response_message(ErrorMessage, ChangeId) ->
    case scm_change:fetch_by_change_id(ChangeId) of
        {ok, ScmMetadata} ->
            PRUrl = scm_change:getval(pr_url, ScmMetadata),
            <<"GitHub Pull Request: ", PRUrl/binary>>;
        {error, _Why} ->  <<"GitHub Pull Request: ", ErrorMessage/binary>>
    end.
