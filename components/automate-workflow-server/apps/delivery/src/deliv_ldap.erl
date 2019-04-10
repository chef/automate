-module(deliv_ldap).

-include_lib("eldap/include/eldap.hrl").

-include("deliv_types.hrl").

-export([
         lookup/1,
         verify_password/2,
         mapped_attrs_for/1
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-type eldap_handle() :: pid().

%% @doc Looks up a user with the app-wide config
%% Each invocation establishes a new connection to the
%% directory service.
-spec lookup(binary())
        -> {ok, #eldap_entry{}} | {error, not_found | _OtherReason}.
lookup(UserName) ->
    lookup(app_wide_config(), UserName).

%% @private
-spec lookup([{atom(), _}], binary())
        -> {ok, #eldap_entry{}} | {error, not_found | _OtherReason}.
lookup(Config, UserName) ->
    case connect(Config) of
        {ok, Session} -> lookup(Config, Session, UserName);
        {error, _Why} = Error -> Error
    end.

%% @private
%% @doc Return the LDAP entry for `UserName'. Details describing
%% the LDAP service and how to search for users is passed in
%% `Config'.
-spec lookup([{atom(), _}], eldap_handle(), binary() | string())
        -> {ok, #eldap_entry{}} | {error, _}.
lookup(Config, Session, UserName) ->
    BaseDN = proplists:get_value(base_dn, Config),
    LoginAttr = proplists:get_value(attr_login, Config),
    Base = {base, BaseDN},
    Filter = {filter, eldap:equalityMatch(LoginAttr, chef_utils:to_str(UserName))},
    search_result(Session, [Base, Filter]).

%% @doc Does authn for the given user
%% Each invocation establishes a new connection to the
%% directory service.
%% If successful, returns a list of LDAP attributes.
%% TODO: maybe at some point if that proves to be a perf bottleneck
%% we'll want to keep the connection open, and make this a gen_server?
%% (doubt it will ever happen though)
-spec verify_password(binary(), str_or_binary())
        -> {verified, #eldap_entry{}} | denied | {error, _Why}.
verify_password(UserName, Password) ->
    verify_password(app_wide_config(), UserName, Password).

%% @private
%% @doc Same as `verify_password/2', except with a custom config.
-spec verify_password([{atom(), _}], binary(), str_or_binary())
        -> {verified, #eldap_entry{}} | denied | {error, _Why}.
verify_password(Config, UserName, Password) ->
    case connect(Config) of
        {error, _Why} = Error ->
            Error;
        {ok, Session} ->
            Result = case lookup(Config, Session, UserName) of
                {ok, Entry} -> verify_password_for_entry(Session, Entry, Password);
                {error, _Why} = Error -> Error
            end,
            %% no matter what the result is, we want to close the session
            eldap:close(Session),
            Result
    end.

%% @private
-spec verify_password_for_entry(eldap_handle(), #eldap_entry{}, str_or_binary())
        -> {verified, [{string(), term()}]} | denied | {error, _Why}.
verify_password_for_entry(Session, #eldap_entry{object_name = Dn} = Entry, Password) ->
    case eldap:simple_bind(Session, Dn, chef_utils:to_str(Password)) of
        ok -> {verified, Entry};
        {error, invalidCredentials} -> denied;
        {error, _Why} = Error -> Error
    end.

search_result(Session, SearchOpts) ->
    case eldap:search(Session, SearchOpts) of
        {ok, #eldap_search_result{entries = []}} ->
            {error, not_found};
        {ok, #eldap_search_result{
                entries = [#eldap_entry{} = Entry]}} ->
            {ok, Entry};
        {ok, #eldap_search_result{entries = Entries}} ->
            {error, {non_unique_match, [{search_opts, SearchOpts},
                                        {entries, Entries}]}};
        {error, _} = Error ->
            Error
    end.

-spec connect([{atom(), _}]) -> {ok, eldap_handle()} | {error, any()}.
connect(Config) ->
    Hosts       = proplists:get_value(hosts, Config),
    OpenOptions = open_options(Config),
    Connection  = eldap:open(Hosts, OpenOptions),
    BindDN      = proplists:get_value(bind_dn, Config),
    BindDNPass  = proplists:get_value(bind_dn_password, Config),
    bind(maybe_encrypt_session(OpenOptions, Connection),
         BindDN,
         BindDNPass).

-spec mapped_attrs_for(binary()) -> list().
mapped_attrs_for(UserName) ->
    mapped_attrs_for(app_wide_config(), UserName).

-spec mapped_attrs_for([{atom(), _}], binary()) -> list().
mapped_attrs_for(Config, UserName) ->
    user_data(Config, lookup(Config, UserName), UserName).

-spec user_data([{atom(), _}], term(), binary()) -> list().
user_data(_Config, {error, _Err}, UserName) ->
    [{name, UserName},
     {user_type, <<"external">>}];
user_data(Config, {ok, {eldap_entry, _, Attrs}}, UserName) ->
    FirstName = proplists:get_value(attr_given_name, Config, "givenName"),
    LastName = proplists:get_value(attr_sn, Config, "sn"),
    Email = proplists:get_value(attr_mail, Config, "mail"),
    [{name, UserName},
     {first_name, chef_utils:to_bin(proplists:get_value(FirstName, Attrs))},
     {last_name, chef_utils:to_bin(proplists:get_value(LastName, Attrs))},
     {email, chef_utils:to_bin(proplists:get_value(Email, Attrs))},
     {user_type, <<"external">>}].

bind({error, _} = E, _, _) ->
    E;
bind({ok, Session} = Result, BindDN, BindDNPass) ->
    case eldap:simple_bind(Session, BindDN, BindDNPass) of
        ok ->
            Result;
        {error, _} = E ->
            E
    end.

open_options(Config) ->
    Timeout     = proplists:get_value(timeout, Config),
    Port        = proplists:get_value(port, Config),
    Encryption  = proplists:get_value(encryption, Config),
    BindDN      = proplists:get_value(bind_dn, Config),
    BindDNPass  = proplists:get_value(bind_dn_password, Config),
    OpenOptions = [{port, Port}, {timeout, Timeout}],
    OpenOptions1 = maybe_ssl_options(Encryption, OpenOptions),
    maybe_anon_auth(BindDN, BindDNPass, OpenOptions1).

maybe_anon_auth("", "", Options) ->
    [{anon_auth, true} | Options];
maybe_anon_auth(_BindDN, _BindDNPass, Options) ->
    Options.

maybe_ssl_options(simple_tls, Options) ->
    [{ssl, true} | Options ];
maybe_ssl_options(start_tls, Options) ->
    [{ssl, true} | Options ];
maybe_ssl_options(_, Options) ->
    Options.

maybe_encrypt_session(_OpenOptions, Error = {error, _}) ->
    Error;
maybe_encrypt_session(OpenOptions, {ok, Session}) ->
    Timeout    = proplists:get_value(timeout, OpenOptions),
    Encryption = proplists:get_value(encryption, OpenOptions),
    maybe_encrypt_session(Encryption, {ok, Session}, Timeout).

maybe_encrypt_session(start_tls, {ok, Session}, Timeout) ->
    case eldap:start_tls(Session, [], Timeout) of
        ok ->
            {ok, Session};
        {error, tls_already_started} ->
            chef_log:info("~s", ["start_tls on ldap session ignored:"
                                  " tls already started"]),
            {ok, Session};
        {error, _} = Error ->
            Error
    end;
maybe_encrypt_session(_, {ok, Session}, _) ->
    {ok, Session}.

%% @private
app_wide_config() ->
    delivery_app:get_env(ldap).
