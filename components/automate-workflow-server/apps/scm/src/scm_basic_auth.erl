-module(scm_basic_auth).

-include_lib("delivery/include/deliv_types.hrl").

-export([
         delete_basic_auth_credentials/3,
         load_basic_auth_credentials/2,
         load_basic_auth_credentials/3,
         save_basic_auth_credentials/5,
         to_ejson_with_self_hal/2,
         update_basic_auth_credentials/5
        ]).

%% @doc saves new credentials for bitbucket basic auth.
-spec save_basic_auth_credentials(binary(), binary(), binary(), db_id(), binary()) -> {ok, d_basic_auth_application()} | {error, atom()}.
save_basic_auth_credentials(Url, UserId, UserPass, EntId, ScmType) ->
    deliv_basic_auth_application:encrypt_and_insert(ScmType, Url,
                                                    UserId, UserPass, EntId).

%% @doc updates credentials for bitbucket basic auth.
-spec update_basic_auth_credentials(binary(), binary(), binary(), db_id(), binary()) -> {ok, d_basic_auth_application()} | {error, atom()}.
update_basic_auth_credentials(Url, UserId, UserPass, EntId, ScmType) ->
    deliv_basic_auth_application:encrypt_and_update(ScmType, Url,
                                                    UserId, UserPass, EntId).

%% Loads the basic auth credentials saved for bitbucket by EntName.
-spec load_basic_auth_credentials(binary(), binary()) -> {ok, d_basic_auth_application()} | {error, atom()}.
load_basic_auth_credentials(EntName, ScmType) ->
    deliv_basic_auth_application:fetch_by_enterprise_name_app_name_and_decrypt(EntName, ScmType).

%% Loads the basic auth credentials saved for bitbucket by EntName and Url.
-spec load_basic_auth_credentials(binary(), binary(), binary()) -> {ok, d_basic_auth_application()} | {error, atom()}.
load_basic_auth_credentials(EntName, ScmType, Url) ->
    deliv_basic_auth_application:fetch_by_enterprise_name_url_app_name_and_decrypt(EntName, ScmType, Url).

%% Delete the basic auth credentials saved for bitbucket by EntName and Url.
-spec delete_basic_auth_credentials(binary(), binary(), binary()) -> ok | {error, atom()}.
delete_basic_auth_credentials(EntName, Url, ScmType) ->
    Module = metadata_module(ScmType),
    case Module:fetch_all_for_ent_name_url(EntName, Url) of
        {ok, [_ | _]} ->
            {error, projects_exist};
        {error, not_found} ->
            deliv_basic_auth_application:delete_by_enterprise_name_url_app_name(EntName, ScmType, Url);
        Error ->
            Error
    end.

%% Turns a basic auth record into json with a self link.
-spec to_ejson_with_self_hal(binary(), d_basic_auth_application()) -> json().
to_ejson_with_self_hal(EntName, BasicAuth) ->
    UserId = deliv_basic_auth_application:getval(user_id, BasicAuth),
    Url = deliv_basic_auth_application:getval(root_api_url, BasicAuth),

    %% TODO: we currently only support one scm configuration per type, and that type
    %% is represented in the `name` field. We're piggybacking on that here to
    %% determing the ScmType based on the information that we have. When we support
    %% more than one scm configuration per type, we'll also need to store the type
    %% in the database and we can use that instead.
    ScmType = deliv_basic_auth_application:getval(name, BasicAuth),
    EncUrl = deliv_web_utils:encode_url(Url),
    SelfLink = erlang:iolist_to_binary([deliv_web_utils:make_api_url_prefix(EntName),
                                        "scm/", ScmType, "/servers/", EncUrl]),
    {[{<<"root_api_url">>, Url},
      {<<"user_id">>, UserId},
      {<<"_links">>, {[
          {<<"self">>, {[
              {<<"href">>, SelfLink}
          ]}}
      ]}}
     ]}.

% private
metadata_module(<<"bitbucket">>) -> scm_bitbucket_project_metadata;
metadata_module(<<"github">>) -> scm_github_project_metadata.
