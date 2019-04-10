-module(deliv_ldap_tests).

-include_lib("eunit/include/eunit.hrl").

lookup_test_() ->
    with_working_ldap_server(fun({Config, User, _Password}) ->
        UserName = deliv_user:getval(name, User),
        [[
         {"unknown user yields not_found",
          ?_assertEqual({error, not_found},
                        deliv_ldap:lookup(Config, Cast("no.such.user")))},

         {"known user is returned",
          ?_assertEqual({ok, ct_ldap:ldap_user(Config, User)},
                        deliv_ldap:lookup(Config, Cast(UserName)))}]
         %% we check for both binaries and lists
         || Cast <- [fun chef_utils:to_str/1, fun chef_utils:to_bin/1]]
    end).

anon_auth_lookup_test_() ->
    %% NOTE(ssd) 2017-09-27: The important bit of this test is actually the assertion in
    %% ct_ldap's server setup that asserts the proper anon_auth config is sent to eldap
    with_anon_config_ldap_server(fun({Config, User, _Password}) ->
        UserName = deliv_user:getval(name, User),
        [[
          {"known user is returned",
          ?_assertEqual({ok, ct_ldap:ldap_user(Config, User)},
                        deliv_ldap:lookup(Config, Cast(UserName)))}]
         %% we check for both binaries and lists
         || Cast <- [fun chef_utils:to_str/1, fun chef_utils:to_bin/1]]
    end).

verify_password_test_() ->
    with_working_ldap_server(fun({Config, User, Password}) ->
        UserName = deliv_user:getval(name, User),
        [[
         {"unknown user yields not_found",
          ?_assertEqual({error, not_found},
                        deliv_ldap:verify_password(Config, Cast("no.such.user"), "still, a password"))},

         {"wrong password gets denied",
          ?_assertEqual(denied,
                        deliv_ldap:verify_password(Config, Cast(UserName), "wrong password"))},

         {"known user with right password works",
          ?_assertEqual({verified, ct_ldap:ldap_user(Config, User)},
                        deliv_ldap:verify_password(Config, Cast(UserName), Password))}]
         %% we check for both binaries and lists
         || Cast <- [fun chef_utils:to_str/1, fun chef_utils:to_bin/1]]
    end).

map_attr_test_() ->
    with_working_ldap_server(fun({Config, User, _Password}) ->
        UserName = deliv_user:getval(name, User),
        Attrs = [{name, iolist_to_binary(UserName)},
                 {first_name, <<"Joe">>},
                 {last_name, <<"User">>},
                 {email, <<"joe@user.com">>},
                 {user_type, <<"external">>}],
        [[
         {"attrs return mapped correctly",
         ?_assertEqual(Attrs, deliv_ldap:mapped_attrs_for(Config, Cast(UserName)))}]
         || Cast <- [fun chef_utils:to_bin/1]]
    end).

%% @private
%% @doc Sets up mocks to emulate a functional LDAP server
with_working_ldap_server(TestFun) ->
    with_ldap_server_for_config(TestFun, ct_ldap:sample_config()).

with_anon_config_ldap_server(TestFun) ->
    with_ldap_server_for_config(TestFun, ct_ldap:sample_anon_config()).

with_ldap_server_for_config(TestFun, Config) ->
    {setup,
     fun() ->
             UserName = <<"user8675309">>,
             Password = "oh le joli password!",
             User = deliv_user:fromlist(db_test_helpers:new_extern_user_data(UserName)),
             ct_ldap:mock_working_ldap_server(
               Config,
               [{ct_ldap:ldap_user(Config, User), Password}]
              ),
             {Config, User, Password}
     end,
     fun(_) ->
             ok = ct_meck:unload(eldap)
     end,
     TestFun}.
