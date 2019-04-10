-module(ct_ldap).

-include_lib("eldap/include/eldap.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("delivery/include/deliv_types.hrl").

-export([
         mock_working_ldap_server/2,
         set_env/1,
         unset_env/0,
         ldap_user/2,
         sample_config/0,
         sample_anon_config/0
        ]).

-define(STUB_SESSION, stub_session).

%% @doc Mocks all what's needed to simulate a working
%% LDAP server with the given `Config' (i.e. delivery
%% LDAP config), and that features the given LDAP users
%% and passwords
-spec mock_working_ldap_server([{atom(), _}],
                               [{#eldap_entry{}, Password :: str_or_binary()}]) -> ok.
mock_working_ldap_server(Config, UsersAndPasswords) ->
    ok = meck:new(eldap, [passthrough]),

    meck:expect(eldap, open,
        fun(HostsArg, ConfigArg) ->
            Hosts = get_value(hosts, Config),
            ct_utils:assert_same_elements(Hosts, HostsArg),
            BaseOpenConfig = [key_find(port, Config),
                              key_find(timeout, Config)],
            LDAPConfig = case {key_find(bind_dn, Config), key_find(bind_dn_password, Config)} of
                {{bind_dn, ""}, {bind_dn_password, ""}} -> [{anon_auth, true}];
                _ -> []
            end,
            SSLConfig = case key_find(encryption, Config) of
                {encryption, start_tls} -> [{ssl, true}];
                false -> []
            end,
            ExpectedConfig = LDAPConfig ++ SSLConfig ++ BaseOpenConfig,
            ct_utils:assert_same_elements(ExpectedConfig, ConfigArg),
            {ok, ?STUB_SESSION}
        end),

    meck:expect(eldap, start_tls,
        fun(_, [], _Timeout) -> ok end),

    meck:expect(eldap, simple_bind,
        fun(?STUB_SESSION, Dn, Pwd) ->
            {BindDn, BindDnPassword} = {get_value(bind_dn, Config),
                                        get_value(bind_dn_password, Config)},
            case {Dn, Pwd} =:= {BindDn, BindDnPassword} of
                true ->
                    ok;
                false ->
                    case lists:dropwhile(
                        fun({#eldap_entry{object_name = UserDn}, UserPwd}) ->
                            {Dn, Pwd} =/= {UserDn, chef_utils:to_str(UserPwd)}
                        end,
                        UsersAndPasswords
                    ) of
                        [_ | _] -> ok;
                        [] -> {error, invalidCredentials}
                    end
            end
        end),

    meck:expect(eldap, search,
        fun(?STUB_SESSION, [{base, Dn}, {filter, Filter}]) ->
            ?assertEqual(get_value(base_dn, Config), Dn),
            AttrLogin = get_value(attr_login, Config),
            Entries = case lists:dropwhile(
                fun({#eldap_entry{attributes = Attrs}, _Pwd}) ->
                    FilterForUser = eldap:equalityMatch(AttrLogin,
                                                        get_value(AttrLogin, Attrs)),
                    Filter =/= FilterForUser
                end,
                UsersAndPasswords
            ) of
                [{Entry, _Pwd} | _] -> [Entry];
                [] -> []
            end,
            {ok, #eldap_search_result{entries = Entries}}
        end),

    meck:expect(eldap, close,
                fun(_Session) -> ok end),

    ok.

%% @doc Creates an LDAP user from a deliv_user
-spec ldap_user([{atom(), _}], d_user()) -> #eldap_entry{}.
ldap_user(Config, User) ->
    UserAttr = fun(Key) ->
        case deliv_user:getval(Key, User) of
            undefined -> "UNDEF " ++ chef_utils:to_str(Key);
            Value -> chef_utils:to_str(Value)
        end
    end,
    Name = UserAttr(name),
    FirstName = UserAttr(first_name),
    LastName = UserAttr(last_name),
    Email = UserAttr(email),
    DisplayName = FirstName ++ " " ++ LastName,
    Dn = "CN=" ++ Name ++ ",OU=Employees,OU=Domain users,DC=examplecorp,DC=com",
    AttrLogin = get_value(attr_login, Config),
    Attrs = [{"objectClass",
              ["top","person","organizationalPerson","user"]},
             {"cn",[DisplayName]},
             {"sn",[LastName]},
             {"title",["Software Nightmare"]},
             {"givenName",[FirstName]},
             {"distinguishedName", [Dn]},
             {"instanceType",["4"]},
             {"whenCreated",["20140613234957.0Z"]},
             {"whenChanged",["20140713133101.0Z"]},
             {"displayName",[DisplayName]},
             {"mail", [Email]},
             {AttrLogin, Name}],
    #eldap_entry{
       object_name = Dn,
       attributes = Attrs}.

%% @doc A sample LDAP config for delivery
-spec sample_config() -> [{atom(), _}].
sample_config() ->
    [{hosts, ["win-ad1.example.co", "win-ad2.example.co"]},
     {port, 3269},
     {timeout, 2000},
     {bind_dn, "ldap.bind.user"},
     {bind_dn_password, "dollhouse"},
     {base_dn, "OU=Employees,OU=Domain users,DC=examplecorp,DC=com"},
     {attr_login, "sAMAccountName"},
     {attr_mail, "mail"},
     {attr_full_name, "fullName"},
     {encryption, start_tls}].

-spec sample_anon_config() -> [{atom(), _}].
sample_anon_config() ->
    [{hosts, ["win-ad1.example.co", "win-ad2.example.co"]},
     {port, 3269},
     {timeout, 2000},
     {bind_dn, ""},
     {bind_dn_password, ""},
     {base_dn, "OU=Employees,OU=Domain users,DC=examplecorp,DC=com"},
     {attr_login, "sAMAccountName"},
     {attr_mail, "mail"},
     {attr_full_name, "fullName"},
     {encryption, start_tls}].

%% @private
key_find(K, PL) ->
    lists:keyfind(K, 1, PL).

%% @private
get_value(K, PL) ->
    {K, V} = key_find(K, PL),
    V.

%% @doc Sets the right env vars
-spec set_env([{atom(), _}]) -> ok.
set_env(Config) ->
    ok = application:set_env(delivery, ldap, Config).

%% @doc Removes the env vars set previously by `set_env/1'
-spec unset_env() -> ok.
unset_env() ->
    ok = application:unset_env(delivery, ldap).
