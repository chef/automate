-module(enterprise_ctl_tests).

-include_lib("hoax/include/hoax.hrl").

-define(SSH_PUB_KEY, <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBAODllGk/jNjA6xZ+IqgUC1wOm7pqqHoF3d06AmBXix0KtEn3SYacfJHsk+dQQP0OYtWinBQf5I+OUi7fPSrRhEHsSUjrJl7HRh4SkcQ2r8wIIHCyBk4Qb6DezOwuuq7TgP0Ac7HCpHBXFVlAWGhFbLu7ydYSpXSishgHRTLh4Q2NbxUXu4VORMIdFd3nGqai/bqAmV9fKCBbSz8ebfEw+yV8lju2s+wWgQyNDToIJtLdxSdXWxxjkPyBb9VpAJohXcvFHSe9fA7YZGvfHm2ixqT4sl6Whd7JAjCsHt5h4MI5e6KncSUjg0kEM3W1i+FYE9Ej7KOFGt+SCEYKOJG1 builder@delivery">>).
-define(TIMEOUT, infinity).

parse_options_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      {"create enterprise ffdp",
       ?_assertMatch({["create", "enterprise", "ffdp"], [{user_type, <<"internal">>}, {roles,"observer"}]},
                     enterprise_ctl:parse_options(["create", "enterprise", "ffdp"]))},
      {"create enterprise ffdp -p zoltan",
       ?_assertMatch({["create", "enterprise", "ffdp"],
                      [{password, "zoltan"}, {user_type, <<"internal">>}, {roles,"observer"}]},
                     enterprise_ctl:parse_options(["create", "enterprise", "ffdp", "-p",
                                                   "zoltan"]))},
      {"create enterprise ffdp -f /etc/somefile",
       ?_assertMatch({["create", "enterprise", "ffdp"],
                      [{password_file, "/etc/somefile"}, {user_type, <<"internal">>}, {roles,"observer"}]},
                     enterprise_ctl:parse_options(["create", "enterprise", "ffdp", "-f",
                                                   "/etc/somefile"]))},
      {"create enterprise ffdp -s /etc/somefile",
       ?_assertMatch({["create", "enterprise", "ffdp"],
                     [{ssh_pub_key_file, "/etc/somefile"}, {user_type, <<"internal">>}, {roles,"observer"}]},
                     enterprise_ctl:parse_options(["create", "enterprise", "ffdp", "-s",
                                                   "/etc/somefile"]))},
      {"create enterprise ffdp -p zoltan -s /etc/somefile",
       ?_assertMatch({["create", "enterprise", "ffdp"],
                      [{password, "zoltan"}, {ssh_pub_key_file, "/etc/somefile"},
                       {user_type, <<"internal">>}, {roles,"observer"}]},
                     enterprise_ctl:parse_options(["create", "enterprise", "ffdp", "-p",
                                                   "zoltan", "-s", "/etc/somefile"]))}
     ]}.

get_option_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      {"gets the value from a proplist and returns it",
       ?_assertMatch(<<"snarflebargle">>, enterprise_ctl:get_option(password, [{password,
                                                                                <<"snarflebargle">>}]))}
     ]}.

usage_error_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"returns {error, usage} when called", ?_assertMatch({error, usage}, enterprise_ctl:usage_error("Proper Usage"))}
     ]
    }.

parse_input2_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"returns {error, usage} when a command is not recognized",
       ?_assertMatch({error, invalid}, enterprise_ctl:parse_input2(["invalid", "command", "here"], []))},
      {"returns {error, usage} when create enterprise usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["create", "enterprise"], []))},
      {"returns {error, usage} when create enterprise has name with whitespace",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["create", "enterprise", "invalid name", "--ssh-pub-key-file=/etc/foo/bar.pem"], []))},
      {"returns {error, usage} when create enterprise usage is incorrect but missing required ssh key path",
       ?_assertMatch({error, missing_option}, enterprise_ctl:parse_input2(["create", "enterprise", "myent"], []))},
      {"returns {error, usage} when delete enterprise usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["delete", "enterprise"], []))},
      {"returns {error, usage} when list enterprises usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["list", "enterprises", "boo"], []))},
      {"returns {error, usage} when create user usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["create", "user"], []))},
      {"returns {error, usage} when create users usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["create", "users"], []))},
      {"returns {error, usage} when delete user usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["delete", "user"], []))},
      {"returns {error, usage} when list users usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["list", "users"], []))},
      {"returns {error, usage} when revoke-token usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["revoke-token"], []))},
      {"returns {error, usage} when update-project-hooks usage is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["update-project-hooks", ""], []))},
      {"returns {error, usage} when delete project is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["delete", "project"], []))},
      {"returns {error, usage} when migrate-change-description is incorrect ",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["migrate-change-description", "invalid"], []))},
      {"returns {error, usage} when migrate-change-description-dry-run is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["migrate-change-description-dry-run", "invalid"], []))},
      {"returns {error, usage} when migrate-patchset-diffs is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["migrate-patchset-diffs", "invalid"], []))},
      {"returns {error, usage} when migrate-patchset-diffs-dry-run is incorrect",
       ?_assertMatch({error, usage}, enterprise_ctl:parse_input2(["migrate-patchset-diffs-dry-run", "invalid"], []))}
     ]
    }.

set_option_values_test_() ->
    {setup,
     fun() ->
             meck:new(file, [unstick, passthrough]),
             meck:expect(file, read_file, [{["/etc/somefile"], {ok, <<"snarflebargle">>}},
                                           {["/etc/delivery/builder_key.pub"], {ok, ?SSH_PUB_KEY}}]),
             meck:new(rpc, [unstick, passthrough]),
             meck:expect(rpc, call, [{['_', '_', '_', '_', ?TIMEOUT], <<"123">>}])
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"set the password if its given by a file",
       ?_assertMatch({password, <<"snarflebargle">>}, lists:keyfind(password, 1,
                                                                    enterprise_ctl:set_option_values([
                                                                                                      {password_file, "/etc/somefile"}])))},
      {"set the password randomly if no password file or password given",
       fun() ->
               meck:expect(rpc, call, [{['_', user_password, random, [26], ?TIMEOUT],
                                        <<"0000000000000000000000000">>}]),
               ?assert(meck:validate(rpc)),
               ?assertMatch({password, <<"0000000000000000000000000">>}, lists:keyfind(password, 1, enterprise_ctl:set_option_values([])))
       end},
      {"use the given password in the face of other options",
       ?_assertMatch({password, "snarfle"}, lists:keyfind(password, 1,
                                                          enterprise_ctl:set_option_values([{password, "snarfle"},
                                                                                            {password_file,
                                                                                             "/etc/somefile"}])))},
      {"set the builder password if its given by a file",
       ?_assertMatch({builder_password, <<"snarflebargle">>}, lists:keyfind(builder_password, 1,
                                                                            enterprise_ctl:set_option_values([{builder_password_file,
                                                                                                               "/etc/somefile"}])))},
      {"set the password randomly if no password file or password given",
       fun() ->
               meck:expect(rpc, call, [{['_', user_password, random, [26], ?TIMEOUT],
                                        <<"0000000000000000000000000">>}]),
               ?assert(meck:validate(rpc)),
               ?assertMatch({builder_password, <<"0000000000000000000000000">>}, lists:keyfind(builder_password, 1, enterprise_ctl:set_option_values([])))
       end},
      {"use the given password in the face of other options",
       ?_assertMatch({builder_password, "snarfle"}, lists:keyfind(builder_password, 1,
                                                                  enterprise_ctl:set_option_values([{builder_password, "snarfle"},
                                                                                                    {builder_password_file,
                                                                                                     "/etc/somefile"}])))},
      {"set the ssh public key if a file is provided",
       ?_assertMatch({ssh_pub_key, ?SSH_PUB_KEY}, lists:keyfind(ssh_pub_key, 1,
                                                                enterprise_ctl:set_option_values([{ssh_pub_key_file,
                                                                                                   "/etc/delivery/builder_key.pub"}
                                                                                                 ])))}

     ]}.

delete_project_test_() ->
    {setup,
     fun() ->
             meck:new(rpc, [unstick, passthrough]),
             meck:new(io, [unstick, passthrough])
     end,
     fun(_) ->
             meck:unload(rpc),
             meck:unload(io)
     end,
     [
      {"delete_project shows a success message if the project exists",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', deliv_project, delete, [<<"chef">>, <<"myorg">>, <<"myproj">>, true], ?TIMEOUT], {ok, 1}}]),
               meck:expect(io,
                          format,
                          [{["Successfully deleted project: 'chef/myorg/myproj'"], ok}]),
               enterprise_ctl:delete_project(<<"chef">>, <<"myorg">>, <<"myproj">>),
               ?assert(meck:validate(rpc)),
               ?assert(meck:validate(io))
       end},
      {"delete_project shows a not found message if the project does not exist",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', deliv_project, delete, [<<"chef">>, <<"myorg">>, <<"myproj">>, true], ?TIMEOUT], {ok, 0}}]),
               meck:expect(io,
                          format,
                          [{["Project not found: 'chef/myorg/myproj'"], ok}]),
               enterprise_ctl:delete_project(<<"chef">>, <<"myorg">>, <<"myproj">>),
               ?assert(meck:validate(rpc)),
               ?assert(meck:validate(io))
       end}
      ]}.

create_user_test_() ->
    {setup,
     fun() ->
             meck:new(rpc, [unstick, passthrough]),
             meck:expect(rpc, call, [{['_', '_', '_', '_', ?TIMEOUT], <<"123">>}])
     end,
     fun(_) ->
             meck:unload(rpc)
     end,
     [
      {"create_user/2 internal user (default) with correct user data",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', user_password, hash, [<<"havoc">>], ?TIMEOUT], <<"123">>},
                            {['delivery@127.0.0.1', deliv_authz, assign_roles, [<<"AFI">>, <<"davey">>, [<<"observer">>]], ?TIMEOUT], ok},
                            {['delivery@127.0.0.1',
                              deliv_intern_user,
                              insert,
                              [<<"AFI">>, [{name, <<"davey">>},
                                           {hashed_pass, <<"123">>},
                                           {hash_type, <<"bcrypt">>},
                                           {user_type, <<"internal">>},
                                           {ssh_pub_key, <<"keysaredope">>}
                                          ]], ?TIMEOUT], ok}
                           ]),
               enterprise_ctl:create_user(<<"AFI">>, [{username, <<"davey">>},
                                                      {password, <<"havoc">>},
                                                      {ssh_pub_key, <<"keysaredope">>},
                                                      {roles, "observer"}]),
               ?assert(meck:validate(rpc))
       end},
      {"create_user/2 calls deliv_intern_user without a public key if one is not provided",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', user_password, hash, [<<"havoc">>], ?TIMEOUT], <<"123">>},
                            {['delivery@127.0.0.1', deliv_authz, assign_roles, [<<"AFI">>, <<"davey">>, [<<"observer">>]], ?TIMEOUT], ok},
                            {['delivery@127.0.0.1',
                              deliv_intern_user,
                              insert,
                              [<<"AFI">>, [{name, <<"davey">>},
                                           {hashed_pass, <<"123">>},
                                           {hash_type, <<"bcrypt">>},
                                           {user_type, <<"internal">>}
                                          ]], ?TIMEOUT], ok}
                           ]),
               enterprise_ctl:create_user(<<"AFI">>, [{username, <<"davey">>},
                                                      {password, <<"havoc">>},
                                                      {roles, "observer"}]),
               ?assert(meck:validate(rpc))
       end},

      {"create_user/2 with a public key calls deliv_intern_user with all required arguments",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', user_password, hash, [<<"havoc">>], ?TIMEOUT], <<"123">>},
                            {['delivery@127.0.0.1', deliv_authz, assign_roles, [<<"AFI">>, <<"davey">>, [<<"observer">>, <<"admin">>]], ?TIMEOUT], ok},
                            {['delivery@127.0.0.1',
                              deliv_user,
                              insert,
                              [<<"AFI">>, [{name, <<"davey">>},
                                           {user_type, <<"external">>},
                                           {ssh_pub_key, <<"keysaredope">>}
                                          ]], ?TIMEOUT], ok}
                           ]),
               enterprise_ctl:create_user(<<"AFI">>, [{username, <<"davey">>},
                                                      {password, <<"havoc">>},
                                                      {ssh_pub_key, <<"keysaredope">>},
                                                      {roles, "observer,admin"},
                                                      {user_type, <<"external">>}]),
              ?assert(meck:validate(rpc))
       end},
      {"create_user/2 fails without username",
       ?_assertError({badmatch, false}, enterprise_ctl:create_user(<<"AFI">>, [{password, <<"havoc">>}]))},
      {"create_user/2 fails without password",
       ?_assertError({badmatch, false}, enterprise_ctl:create_user(<<"AFI">>, [{username, <<"davey">>}]))},
      {"create_user/3 with all supported arguments calls deliv_intern_user properly",
        fun() ->
                meck:expect(rpc,
                            call,
                            [{['delivery@127.0.0.1', user_password, hash, ["havoc"], ?TIMEOUT], <<"123">>},
                             {['delivery@127.0.0.1', deliv_authz, assign_roles,
                               [<<"AFI">>, "davey", [<<"observer">>, <<"admin">>]], ?TIMEOUT], ok},
                             {['delivery@127.0.0.1',
                               deliv_intern_user,
                               insert,
                               [<<"AFI">>, [{name, "davey"},
                                            {hashed_pass, <<"123">>},
                                            {hash_type, <<"bcrypt">>},
                                            {user_type, <<"internal">>},
                                            {ssh_pub_key, <<"keysaredope">>}
                                           ]], ?TIMEOUT], ok}
                            ]),
                enterprise_ctl:create_user("AFI", "davey", [{password, "havoc"},
                                                            {ssh_pub_key, <<"keysaredope">>},
                                                            {roles, "observer,admin"}]),
                ?assert(meck:validate(rpc))
        end},
      {"create_user/3 with no roles or public key provides appropriate defaults",
       fun() ->
                meck:expect(rpc,
                            call,
                            [{['delivery@127.0.0.1', user_password, hash, ["havoc"], ?TIMEOUT], <<"123">>},
                             {['delivery@127.0.0.1', deliv_authz, assign_roles,
                               [<<"AFI">>, "davey", []], ?TIMEOUT], ok},
                             {['delivery@127.0.0.1',
                               deliv_intern_user,
                               insert,
                               [<<"AFI">>, [{name, "davey"},
                                            {hashed_pass, <<"123">>},
                                            {hash_type, <<"bcrypt">>},
                                            {user_type, <<"internal">>}
                                           ]], ?TIMEOUT], ok}
                            ]),
                enterprise_ctl:create_user("AFI", "davey", [{password, "havoc"} ])
       end}

     ]
    }.

create_enterprise_test_() ->
    {setup,
     fun() ->
             meck:new(rpc, [unstick, passthrough])
     end,
     fun(_) ->
             meck:unload(rpc)
     end,
     [
      {"create_enterprise/2 when there already is an enterprise does not create another one",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', deliv_enterprise, list_all, [], ?TIMEOUT],
                             [{deliv_enterprise, 1, <<"NCC-1701-C">>}, ?TIMEOUT]}
                           ]),
               enterprise_ctl:create_enterprise("NCC-1701-D", []),
               ?assert(meck:validate(rpc))
       end},
      {"create_enterprise/2 when there already is more than one enterprise does not create another one",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', deliv_enterprise, list_all, [], ?TIMEOUT],
                             [{deliv_enterprise, 1, <<"NCC-1701-B">>}, {deliv_enterprise, 2, <<"NCC-1701-C">>}, ?TIMEOUT]}
                           ]),
               enterprise_ctl:create_enterprise("NCC-1701-D", []),
               ?assert(meck:validate(rpc))
       end},
      {"create_enterprise/2 when there is an error listing enterprises does not create another one",
       fun() ->
               meck:expect(rpc,
                           call,
                           [{['delivery@127.0.0.1', deliv_enterprise, list_all, [], ?TIMEOUT],
                             {error, some_reason}}
                           ]),
               enterprise_ctl:create_enterprise("NCC-1701-D", []),
               ?assert(meck:validate(rpc))
       end},
      {"create_enterprise/2 when there is no enterprise creates one",
       fun() ->
               EntName = <<"NCC-1701-D">>,
               Opts = [{ssh_pub_key, <<"keysaredope">>},
                       {builder_password, <<"snarflebargle">>},
                       {password, <<"random">>},
                       {ssh_pub_key_file, "/dev/null"},
                       {user_type, <<"internal">>},
                       {roles, "observer"}],
               % so much to mock! just mock the things whose return arguments we care about
               Mocks = [{['delivery@127.0.0.1', deliv_enterprise, list_all, [], ?TIMEOUT],
                         []},
                        {['delivery@127.0.0.1', deliv_enterprise, insert, [EntName], ?TIMEOUT],
                         [{deliv_enterprise, 1, EntName}]}
                       ],
               meck:expect(rpc, call, fun(N, M, F, A) -> case proplists:get_value([N, M, F, A], Mocks) of
                                                             undefined -> ok;
                                                             Return -> Return
                                                         end
                                      end),
               enterprise_ctl:create_enterprise("NCC-1701-D", Opts),
               ?assert(meck:validate(rpc))
       end}
     ]
    }.

rename_enterprise_test_returns_ok_and_calls_io_format_if_rpc_call_errors_out_test() ->
    hoax:test(fun() ->
        Error = why,
        OldNameB = <<"cd">>,
        hoax:expect(receive
                        rpc:call('delivery@127.0.0.1', deliv_enterprise, rename, [OldNameB, <<"whatever">>], ?TIMEOUT) -> {error, Error};
                        io:format("Error renaming ~s: ~p~n", [OldNameB, Error]) -> ok
                    end),
        Result = enterprise_ctl:rename_enterprise("cd", "whatever"),
        ?assertEqual(ok, Result),
        ?verifyAll
    end).

rename_enterprise_test_returns_ok_and_calls_io_format_if_rpc_call_succeeds_test() ->
    hoax:test(fun() ->
        OldNameB = <<"cd">>,
        NewNameB = <<"whatever">>,
        hoax:expect(receive
                        rpc:call('delivery@127.0.0.1', deliv_enterprise, rename, [OldNameB, NewNameB], ?TIMEOUT) -> {ok, {deliv_enterprise, ignored, NewNameB}};
                        io:format("~s successfully renamed to ~s.~n", [OldNameB, NewNameB]) -> ok
                    end),
        Result = enterprise_ctl:rename_enterprise("cd", "whatever"),
        ?assertEqual(ok, Result),
        ?verifyAll
    end).
