-module(github_ent_setup_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

-define(SSH_KEY_TITLE, <<"Delivery Builder (github_project_test_enterprise)">>).

%% Due to the unreliability of the Github Enterprise response time / availability,
%% these tests passed at the time of this writing but are disabled for the time being.
%%
%% To enable, change name to project_create_fixture_test_
%% To disable, change name to project_create_fixture_disable
ent_setup_fixture_disabled() ->
    hoax:parameterized_fixture(?MODULE, "ent_setup", setup, teardown).

setup() ->
    error_logger:tty(false),
    app_test_helpers:setup_app_env(),
    eu_database:setup(),
    eu_github:setup(),
    eu_github:with_enterprise(<<"github_project_test_enterprise">>,
        fun(Enterprise) ->
            EntName = deliv_enterprise:getval(name, Enterprise),
            {ok, KeyId} = github_repo:add_builder_pub_key(EntName),
            {Enterprise, KeyId}
        end).

teardown({Enterprise, KeyId}) ->
    eu_github:remove_key(Enterprise, KeyId),
    eu_github:teardown(),
    eu_database:teardown(),
    error_logger:tty(true).

ent_setup_builder_pub_key_added_as_github_user_key({Enterprise, KeyId}) ->
    Route = ["/keys/", chef_utils:to_str(KeyId)],
    EntName = deliv_enterprise:getval(name, Enterprise),
    {ok, BuilderUser} = deliv_intern_user:fetch(EntName, <<"builder">>),
    Key = deliv_intern_user:getval(ssh_pub_key, BuilderUser),
    case deliv_github_client:user_req(EntName, get, Route, <<>>) of
        {ok, Status, _, Response} ->
            Json = chef_json:decode(Response),
            ?assertEqual(200, Status),
            ?assertEqual(?SSH_KEY_TITLE, ej:get([<<"title">>], Json)),
            ?assertEqual(Key, ej:get([<<"key">>], Json));
        {error, Why} ->
            error(Why)
    end.
