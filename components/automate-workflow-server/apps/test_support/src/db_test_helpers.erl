%% Copyright 2014 Chef Software, Inc. All Rights Reserved.

%% @doc Provides a few helper functions to use in tests to create and/or
%% delete delivery objects in/from the DB.

-module(db_test_helpers).

-define(SSH_PUB_KEY, <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNiWOgICgu9VyRb4m"
                       "+nxbBvK9+k0IdoBAVUJsPddkQxp07QxzpxTJnXUsKg2ZYMBqLJZvx67"
                       "hIGHPRwHxfBPJBzs+pzdXKIeZnwEvK7KN9ouwP0UPbrlHLEQF7uzsLZ"
                       "osEPau/vuzGszvW1CqNAD05Mm7KT+dyZdIo8H4WDie+tdbyEIZ7hTzv"
                       "2DDjOkR9y5s4LSCLwy5wrB7+3EnK7os9PkhTUoLNgFm/CiuQtrSXhW3"
                       "8qjLnr3WTNjK5XFyFSy/20DoQMAlqyFfZ4GlnGJpnX2sTPfoRpV/GEw"
                       "4UQ2zhIdXmWksz/6ji4shI+pN1mwe2zrjCbpO7HtXcUMzQ7ssj">>).

-export([ensure_enterprise_exists/1,
         new_enterprise/1,
         new_intern_user_data/2,
         new_intern_user/2,
         new_intern_user/3,
         new_extern_user_data/1,
         new_extern_user/2,
         new_builder_user/1,
         new_organization/2,
         new_scoped_organization/1,
         new_project/3,
         new_scoped_project/1,
         new_pipeline/4,
         new_scoped_pipeline/1,
         new_patchset/1,
         new_patchset/6,
         prefixed_random_string/1,
         token_for_user/3]).

-include_lib("delivery/include/deliv_types.hrl").

%% @doc Makes sure an enterprise exists in the DB
-spec ensure_enterprise_exists(binary()) -> d_enterprise().
ensure_enterprise_exists(EntName) ->
    case deliv_enterprise:insert(EntName) of
        [Ent] ->
            Ent;
        {error, conflict} ->
            {ok, Ent} = deliv_enterprise:fetch(EntName),
            Ent;
        {error, _} = Error ->
            error(Error)
    end.

%% @doc Returns a new enterprise (i.e. one whose name wasn't in the DB before)
-spec new_enterprise(binary()) -> {binary(), d_enterprise()}.
new_enterprise(Prefix) when erlang:is_binary(Prefix) ->
    Name = prefixed_random_string(Prefix),
    [Enterprise] = deliv_enterprise:insert(Name),
    {Name, Enterprise}.

%% @doc Same as new_enterprise, for orgs
-spec new_organization(binary(), binary()) -> {binary(), d_organization()}.
new_organization(Prefix, EntName) ->
    Name = prefixed_random_string(Prefix),
    [Organization] = deliv_organization:insert(EntName, Name),
    {Name, Organization}.

%% @doc Creates both a new ent, and a new org attached to it
-spec new_scoped_organization(binary()) ->
    {{binary(), d_enterprise()}, {binary(), d_organization()}}.
new_scoped_organization(Prefix) ->
    {EntName, Ent} = new_enterprise(Prefix),
    {{EntName, Ent}, new_organization(Prefix, EntName)}.

%% @doc Returns the data needed to create a new intern user
-spec new_intern_user_data(binary(), binary()) -> proplist(atom(), any()).
new_intern_user_data(UserName, Password) ->
    [{name, UserName},
     {hashed_pass, user_password:hash(Password)},
     {hash_type, <<"bcrypt">>},
     {user_type, <<"internal">>}].

%% @doc Creates a new Builder user in the specified enterprise
-spec new_builder_user(binary()) -> d_intern_user().
new_builder_user(EntName) ->
    Data = [{name, <<"builder">>},
            {user_type, <<"internal">>},
            {ssh_pub_key, ?SSH_PUB_KEY}],
    [User] = deliv_intern_user:insert(EntName, Data),
    User.

%% @doc Proxy for `new_intern_user/3', handy if you don't care about the password
-spec new_intern_user(binary(), binary()) -> {binary(), d_intern_user()}.
new_intern_user(EntName, UserName) ->
    new_intern_user(EntName, UserName, <<"password">>).

%% @doc Creates a new intern user, and returns both a token and the user
-spec new_intern_user(binary(), binary(), binary()) -> {binary(), d_intern_user()}.
new_intern_user(EntName, UserName, Password) ->
    Data = new_intern_user_data(UserName, Password),
    [User] = deliv_intern_user:insert(EntName, Data),

    {ok, Token} =  deliv_token:assign_token(EntName, UserName),

    {Token, User}.

%% @doc Returns the data needed to create a new extern user
-spec new_extern_user_data(binary()) -> proplist(atom(), any()).
new_extern_user_data(UserName) ->
    [
        {name, UserName},
        {first_name, <<"Joe">>},
        {last_name, <<"User">>},
        {email, <<"joe@user.com">>},
        {user_type, <<"external">>}
    ].

%% @doc Creates a new extern user, bypassing the data-fetching
%% from the LDAP server (useful for test that need external users
%% but don't need to mock the whole LDAP setup)
-spec new_extern_user(binary(), binary()) -> d_user().
new_extern_user(EntName, UserName) ->
    Data = new_extern_user_data(UserName),
    [User] = deliv_user:insert(EntName, Data),
    User.

%% @doc Creates a new project, along with the corresponding repo on disk
-spec new_project(binary(), binary(), binary()) -> {binary(), d_project()}.
new_project(Prefix, EntName, OrgName) ->
    Name = prefixed_random_string(Prefix),
    {ok, Project} = deliv_project:new(EntName, OrgName, Name),
    {Name, Project}.

%% @doc Creates a new enterprise, a new organization attached to it, and a
%% project attached to that org
-spec new_scoped_project(binary()) ->
    {{binary(), d_enterprise()}, {binary(), d_organization()}, {binary(), d_project()}}.
new_scoped_project(Prefix) ->
    {{EntName, Ent}, {OrgName, Org}} = new_scoped_organization(Prefix),
    {{EntName, Ent}, {OrgName, Org}, new_project(Prefix, EntName, OrgName)}.

%% @doc Creates a new pipeline
-spec new_pipeline(binary(), binary(), binary(), binary()) -> {binary(), d_pipeline()}.
new_pipeline(Prefix, EntName, OrgName, ProjName) ->
    Name = prefixed_random_string(Prefix),
    [Pipeline] = deliv_pipeline:insert(EntName, OrgName, ProjName, Name),
    {Name, Pipeline}.

%% @doc Creates a new enterprise, a new organization attached to it, a
%% project attached to that org, and a pipeline attached to that project
-spec new_scoped_pipeline(binary()) ->
    {{binary(), d_enterprise()}, {binary(), d_organization()},
     {binary(), d_project()}, {binary(), d_pipeline()}}.
new_scoped_pipeline(Prefix) ->
    {{EntName, Ent}, {OrgName, Org}, {ProjName, Proj}} = new_scoped_project(Prefix),
    {{EntName, Ent}, {OrgName, Org}, {ProjName, Proj},
        new_pipeline(Prefix, EntName, OrgName, ProjName)}.

%% @doc Same as `new_patchset/6', but takes the params from the CT `Config', with the
%% same keys as for `ct_authz', plus `feat_branch_name'
-spec new_patchset(proplist(atom(), any()))
        -> {PatchsetId :: non_neg_integer(), d_patchset()}.
new_patchset(Config) ->
    EntUserOrgProjPipeNames = ct_authz:params_for_scope(pipeline, Config),
    FeatBranchName = ct_utils:get_config(feat_branch_name, Config),
    erlang:apply(fun new_patchset/6, EntUserOrgProjPipeNames ++ [FeatBranchName]).

%% @doc Creates a new patchset with a random sha in the DB
%% Note that this doesn't actually create a patchset in an actual repo
%% it just creates the record in the DB
-spec new_patchset(binary(), binary(), binary(), binary(), binary(), binary())
        -> {PatchsetId :: non_neg_integer(), d_patchset()}.
new_patchset(EntName, UserName, OrgName, ProjName, PipeName, FeatBranchName) ->
    %% results in a 40-char long SHA, same as regular git SHAs
    Sha = chef_utils:random_hex_string(20),
    [PatchSet] = deliv_patchset:new(EntName, UserName, OrgName, ProjName,
                                    PipeName, FeatBranchName, Sha),
    PatchSetId = deliv_patchset:getval(id, PatchSet),
    {PatchSetId, PatchSet}.

%% @doc Get a new token for the given user. The user must have been
%% already added to the system; your best bet is to use one present in
%% the `user-data.tsv` file.
-spec token_for_user(binary(), binary(), binary()) -> binary().
token_for_user(UserName, EntName, Password) ->
    Route = http_test_helpers:base_ent_route(EntName) ++ "/users/" ++ erlang:binary_to_list(UserName) ++ "/get-token",
    JSON = {[{<<"username">>, UserName}, {<<"password">>, Password}]},
    {200, _Headers, Body} = http_test_helpers:req(post, Route, JSON),
    ej:get([<<"token">>], chef_json:decode(Body)).

%% @private
-spec prefixed_random_string(binary()) -> binary().
prefixed_random_string(Prefix) ->
    RandomString = ct_utils:unique_string(),
    <<Prefix/binary, RandomString/binary>>.
