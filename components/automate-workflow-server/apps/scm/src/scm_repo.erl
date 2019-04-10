%% doc This module is intended to be a wrapper around git operations
%% we want to perform on the bitbucket repo.
-module(scm_repo).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include("scm_types.hrl").

-export([url/1]).

-spec url(#proj_coordinates{}) -> {ok, binary()} | {error, term()}.
url(Coords = #proj_coordinates{ent_name = EntName}) ->
    case scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) of
        {ok, #metadata_by_scm{scm_type = ScmType, repo_group = RepoGroup, repo_name = RepoName}} ->
            case fetch_auth_creds(EntName, ScmType) of
                {ok, {UserName, Password, RootUrl}} ->
                    {ok, {Scheme, _, Host, Port, Path, _}} = http_uri:parse(chef_utils:to_str(RootUrl)),
                    Url = build_url(ScmType, Scheme, UserName, Password, Host, Port, Path, RepoGroup, RepoName),
                    {ok, Url};
                Error -> Error
            end;
        Error -> Error
    end.

%% Private functions
-spec fetch_auth_creds(binary(), binary()) -> {ok, {binary(), binary(), binary()}} | {error, term()}.
fetch_auth_creds(EntName, ScmType) ->
    case scm_basic_auth:load_basic_auth_credentials(EntName, ScmType) of
        {ok, BasicAuth} ->
            UserName = deliv_basic_auth_application:getval(user_id, BasicAuth),
            Password = deliv_basic_auth_application:getval(password, BasicAuth),
            RootUrl = deliv_basic_auth_application:getval(root_api_url, BasicAuth),
            {ok, {UserName, Password, RootUrl}};
        Error -> Error
    end.

-spec build_url(binary(), atom(), binary(), binary(), string(), pos_integer(), string(), binary(), binary()) -> binary().
build_url(<<"bitbucket">>, Scheme, UserName, Password, Host, Port, Path, Project, Repo) ->
    EncodedUsername = chef_utils:to_bin(deliv_web_utils:encode_url_rfc1738(UserName)),
    EncodedPW = chef_utils:to_bin(deliv_web_utils:encode_url_rfc1738(Password)),
    iolist_to_binary([atom_to_list(Scheme), "://",
                      EncodedUsername, ":", EncodedPW, "@", Host, ":",
                      integer_to_list(Port), normalize_path(Path), "/scm/", Project, "/",
                      Repo, ".git"]);
build_url(<<"github">>, Scheme, _UserName, Token, Host, Port, Path, Owner, Repo) ->
    iolist_to_binary([atom_to_list(Scheme), "://",
                      Token, ":x-oauth-basic" "@", Host, ":",
                      integer_to_list(Port), normalize_path(Path), "/", Owner, "/",
                      Repo, ".git"]).

-spec normalize_path(string()) -> string().
normalize_path("/") ->
    "";
normalize_path(Path) ->
    Path.
