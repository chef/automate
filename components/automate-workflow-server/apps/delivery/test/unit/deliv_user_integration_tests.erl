-module(deliv_user_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

deliv_user_integration_test_() ->
    [eunit_sugar:parameterized_fixture(?MODULE, "fetch", setup, teardown),
     eunit_sugar:parameterized_fixture(?MODULE, "email_user", setup, teardown),
     eunit_sugar:fixture(?MODULE, "create_user", setup_basic, teardown),
     eunit_sugar:fixture(?MODULE, "update_", setup_basic, teardown)].

setup_basic() ->
    error_logger:tty(false),
    eu_database:setup().

setup() ->
    setup_basic(),
    OauthApp = eu_github:fetch_or_create_oauth_app(),
    OauthAppId = deliv_oauth_application:getval(id, OauthApp),
    eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        User = eu_data:fetch_or_create_user(Enterprise, <<"drventure">>),
        deliv_oauth_user_alias:insert(OauthAppId, deliv_user:getval(id, User), <<"alias">>),
        [Enterprise, User, OauthApp]
    end).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

fetch_by_ent_name_user_name_returns_expected_user([Ent, User, _OauthApp]) ->
    EntName = deliv_enterprise:getval(name, Ent),
    UserName = deliv_user:getval(name, User),

    {ok, FoundUser} = deliv_user:fetch(EntName, UserName),
    TelemetryEnabled = deliv_user:getval(telemetry_enabled, FoundUser),
    ?assertEqual(true, TelemetryEnabled).

fetch_by_alias_should_return_user_from_oauth_application_and_alias([_Ent, User, OauthApp]) ->
    ?assertEqual({ok, User}, deliv_user:fetch_by_alias(OauthApp, <<"alias">>)).

fetch_by_ent_name_user_name_returns_user_when_found([Ent, User, _OauthApp]) ->
    EntName = deliv_enterprise:getval(name, Ent),
    UserName = deliv_user:getval(name, User),

    FoundUser = deliv_user:fetch(EntName, UserName),

    ?assertEqual({ok, User}, FoundUser).

fetch_by_ent_name_user_name_returns_error_when_user_not_found([Ent, _User, _OauthApp]) ->
    EntName = deliv_enterprise:getval(name, Ent),
    UserName = <<"petewhite">>,

    FoundUser = deliv_user:fetch(EntName, UserName),

    ?assertMatch({error, not_found}, FoundUser).

create_user_of_type_internal_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>)
    end),

    ?assertMatch(<<"internal">>, deliv_user:getval(user_type, User)).

create_user_of_type_external_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"external">>)
    end),

    ?assertMatch(<<"external">>, deliv_user:getval(user_type, User)).

create_user_of_type_saml_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"saml">>)
    end),

    ?assertMatch(<<"saml">>, deliv_user:getval(user_type, User)).

email_user_returns_error_not_found_when_user_not_found([Ent, _User, _OauthApp]) ->
    EntName = deliv_enterprise:getval(name, Ent),
    UserName = <<"petewhite">>,
    SendEmailFun = fun(_UserEmail) -> ok end,

    Result = deliv_user:email_user(EntName, UserName, SendEmailFun),

    ?assertMatch({error, user_not_found}, Result).

email_user_returns_error_no_email_when_user_has_no_email([Ent, User, _OauthApp]) ->
    EntName = deliv_enterprise:getval(name, Ent),
    UserName = deliv_user:getval(name, User),
    SendEmailFun = fun(_UserEmail) -> ok end,

    %% Clear the email address for the User.
    User2 = deliv_user:setvals([{email, undefined}], User),
    deliv_user:update(User2),

    Result = deliv_user:email_user(EntName, UserName, SendEmailFun),

    ?assertEqual({error, no_user_email}, Result).

email_user_sends_email_when_user_has_an_email([Ent, User, _OauthApp]) ->
    EntName = deliv_enterprise:getval(name, Ent),
    UserName = deliv_user:getval(name, User),
    SendEmailFun = fun(_UserEmail) ->
                       message_sent
                   end,

    Result = deliv_user:email_user(EntName, UserName, SendEmailFun),

    ?assertEqual(message_sent, Result).

%% Test updates including the user_type column. Note that currently, there are
%% no database-level integrity checks in place, but the API forbids type
%% changes that are invalid (X -> internal). Thus we only test the success
%% cases here.

update_to_type_internal_when_type_is_internal_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"internal">>)
    end),

    {ok, Result} = deliv_user:update(User),
    ?assertEqual(<<"internal">>, deliv_user:getval(user_type, Result)).

update_to_type_external_when_type_is_external_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"external">>)
    end),

    {ok, Result} = deliv_user:update(User),
    ?assertEqual(<<"external">>, deliv_user:getval(user_type, Result)).

update_to_type_saml_when_type_is_saml_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"saml">>)
    end),

    User2 = deliv_user:setvals([{user_type, <<"saml">>}], User),
    {ok, Result} = deliv_user:update(User2),
    ?assertEqual(<<"saml">>, deliv_user:getval(user_type, Result)).

update_to_type_external_when_type_is_internal_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"internal">>)
    end),

    User2 = deliv_user:setvals([{user_type, <<"external">>}], User),
    {ok, Result} = deliv_user:update(User2),
    ?assertEqual(<<"external">>, deliv_user:getval(user_type, Result)).

update_to_type_saml_when_type_is_internal_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"internal">>)
    end),

    User2 = deliv_user:setvals([{user_type, <<"saml">>}], User),
    {ok, Result} = deliv_user:update(User2),
    ?assertEqual(<<"saml">>, deliv_user:getval(user_type, Result)).

update_to_type_saml_when_type_is_external_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"external">>)
    end),

    User2 = deliv_user:setvals([{user_type, <<"saml">>}], User),
    {ok, Result} = deliv_user:update(User2),
    ?assertEqual(<<"saml">>, deliv_user:getval(user_type, Result)).

update_to_type_external_when_type_is_saml_returns_valid_user() ->
    User = eu_data:with_enterprise(<<"VenTech">>, fun(Enterprise) ->
        eu_data:fetch_or_create_user(Enterprise, <<"drventure">>, <<"saml">>)
    end),

    User2 = deliv_user:setvals([{user_type, <<"external">>}], User),
    {ok, Result} = deliv_user:update(User2),
    ?assertEqual(<<"external">>, deliv_user:getval(user_type, Result)).
