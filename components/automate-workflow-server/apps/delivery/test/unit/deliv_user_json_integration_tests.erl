-module(deliv_user_json_integration_tests).

-compile(export_all).

-include_lib("hoax/include/hoax.hrl").

%% also includes tests for deliv_user_json:validate_update/2
validate_test_() ->
    hoax:fixture(?MODULE, "validate", setup, teardown).

setup() ->
    deliv_json:init_schemas().

teardown(_) ->
    ok.

validate_with_valid_saml_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"stephen">>},
                  {<<"user_type">>, <<"saml">>}]},

    ?assertEqual(ValidJson, deliv_user_json:validate(ValidJson)).

validate_without_user_type_fails_validation() ->
    InvalidJson = {[{<<"name">>, <<"stephen">>}]},

    ?assertMatch({error, [{data_invalid,
                            _Spec,
                            {missing_required_property, <<"user_type">>},
                            InvalidJson,
                            []}]},
                 deliv_user_json:validate(InvalidJson)).

validate_with_empty_json_fails_validation() ->
    InvalidJson = {[]},

    ?assertMatch({error, [{data_invalid,
                            _Spec,
                            {missing_required_property, <<"name">>},
                            InvalidJson,
                            []}]},
                 deliv_user_json:validate(InvalidJson)).

validate_without_name_for_internal_fails_validation() ->
    InvalidJson = {[{<<"user_type">>, <<"internal">>}]},

    ?assertMatch({error, [{data_invalid,
                            _Spec,
                            {missing_required_property, <<"name">>},
                            InvalidJson,
                            []}]},
                  deliv_user_json:validate(InvalidJson)).

validate_without_name_for_external_fails_validation() ->
    InvalidJson = {[{<<"user_type">>, <<"external">>}]},

    ?assertMatch({error, [{data_invalid,
                            _Spec,
                            {missing_required_property, <<"name">>},
                            InvalidJson,
                            []}]},
                  deliv_user_json:validate(InvalidJson)).

validate_without_name_for_saml_fails_validation() ->
    InvalidJson = {[{<<"user_type">>, <<"saml">>}]},

    ?assertMatch({error, [{data_invalid,
                            _Spec,
                            {missing_required_property, <<"name">>},
                            InvalidJson,
                            []}]},
                  deliv_user_json:validate(InvalidJson)).

validate_valid_external_user_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"stephen">>},
                  {<<"user_type">>, <<"external">>}]},

    ?assertEqual(ValidJson, deliv_user_json:validate(ValidJson)).

validate_with_ssh_pub_key_for_saml_user_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"stephen">>},
                  {<<"user_type">>,<<"saml">>},
                  {<<"ssh_pub_key">>,<<"pretendkey">>}]},
    ?assertEqual(ValidJson, deliv_user_json:validate(ValidJson)).

%% These are pulled directly from RAML and translated to EJSON (taken from
%% chef_json_tests)
validate_valid_internal_user_returns_valid_json() ->
    InternUserJson = {[{<<"name">>, <<"zbathory">>},
                       {<<"user_type">>, <<"internal">>},
                       {<<"first">>, <<"Zoltan">>},
                       {<<"last">>, <<"Bathory">>},
                       {<<"email">>, <<"z@ffdp.com">>},
                       {<<"ssh_pub_key">>, <<"ssh-rsa AAAAB3NzaC...">>},
                       {<<"telemetry_enabled">>, true}]},
    ?assertEqual(InternUserJson, deliv_user_json:validate(InternUserJson)).

validate_external_user_with_more_than_the_required_fields_returns_valid_json() ->
    ExternUserJson = {[{<<"name">>, <<"abingham">>},
                       {<<"user_type">>, <<"external">>},
                       {<<"first">>, <<"Andrew">>},
                       {<<"last">>, <<"Bingham">>},
                       {<<"email">>, <<"ab@ffdp.com">>},
                       {<<"ssh_pub_key">>, <<"ssh-rsa AAAAB3NzaC...">>},
                       {<<"telemetry_enabled">>, true}]},
    ?assertEqual(ExternUserJson, deliv_user_json:validate(ExternUserJson)).

validate_internal_user_with_a_wrong_type_for_first_returns_error() ->
    BadUserJson1 = {[{<<"name">>, <<"zbathory">>},
                     {<<"user_type">>, <<"internal">>},
                     {<<"first">>, 5},
                     {<<"last">>, <<"Bathory">>},
                     {<<"email">>, <<"z@ffdp.com">>},
                     {<<"ssh_pub_key">>, <<"ssh-rsa AAAAB3NzaC...">>},
                     {<<"telemetry_enabled">>, true}]},
    ?assertMatch({error, _}, deliv_user_json:validate(BadUserJson1)).

validate_internal_user_with_an_unknown_field_returns_error() ->
    BadUserJson2 = {[{<<"name">>, <<"zbathory">>},
                     {<<"user_type">>, <<"internal">>},
                     {<<"first">>, <<"Andrew">>},
                     {<<"middle">>, <<"Warmia">>},
                     {<<"email">>, <<"z@ffdp.com">>},
                     {<<"ssh_pub_key">>, <<"ssh-rsa AAAAB3NzaC...">>},
                     {<<"telemetry_enabled">>, true}]},
    ?assertMatch({error, _}, deliv_user_json:validate(BadUserJson2)).

validate_internal_user_with_a_wrong_type_for_name_returns_error() ->
    BadUserJson3 = {[{<<"name">>, true},
                     {<<"user_type">>, <<"internal">>},
                     {<<"first">>, <<"Andrew">>},
                     {<<"email">>, <<"z@ffdp.com">>},
                     {<<"ssh_pub_key">>, <<"ssh-rsa AAAAB3NzaC...">>},
                     {<<"telemetry_enabled">>, true}]},
    ?assertMatch({error, _}, deliv_user_json:validate(BadUserJson3)).

validate_internal_user_with_an_additional_unknown_field_returns_error() ->
    BadUserJson4 = {[{<<"name">>, <<"zbathory">>},
                     {<<"user_type">>, <<"internal">>},
                     {<<"first">>, <<"Andrew">>},
                     {<<"middle">>, <<"Warmia">>},
                     {<<"last">>, <<"Bathory">>},
                     {<<"email">>, <<"z@ffdp.com">>},
                     {<<"ssh_pub_key">>, <<"ssh-rsa AAAAB3NzaC...">>},
                     {<<"telemetry_enabled">>, true}]},
    ?assertMatch({error, _}, deliv_user_json:validate(BadUserJson4)).

validate_update_when_changing_name_of_valid_external_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"stephen">>},
                  {<<"user_type">>, <<"external">>}]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"external">>))),
    ?assertEqual(ValidJson, deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_when_changing_first_of_valid_external_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"stephen">>},
                  {<<"first">>, <<"stephen">>},
                  {<<"user_type">>, <<"external">>}]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"external">>))),

    ?assertEqual(ValidJson, deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_complete_valid_internal_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"first">>, <<"Andrew">>},
                  {<<"last">>, <<"Bathory">>},
                  {<<"email">>, <<"z@ffdp.com">>},
                  {<<"user_type">>, <<"internal">>},
                  {<<"telemetry_enabled">>, true}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"internal">>))),

    ?assertEqual(ValidJson, deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_complete_valid_external_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"first">>, <<"Andrew">>},
                  {<<"last">>, <<"Bathory">>},
                  {<<"email">>, <<"z@ffdp.com">>},
                  {<<"user_type">>, <<"external">>},
                  {<<"telemetry_enabled">>, true}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"external">>))),

    ?assertEqual(ValidJson, deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_complete_valid_saml_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"first">>, <<"Andrew">>},
                  {<<"last">>, <<"Bathory">>},
                  {<<"email">>, <<"z@ffdp.com">>},
                  {<<"user_type">>, <<"saml">>},
                  {<<"telemetry_enabled">>, true}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"saml">>))),

    ?assertMatch(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_ssh_pub_key_saml_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"stephen">>},
                  {<<"user_type">>, <<"saml">>},
                  {<<"ssh_pub_key">>,<<"pretendkey">>}]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"saml">>))),
    ?assertMatch(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_internal_for_valid_internal_user_json_returns_valid_json() ->
    %% Note that the intern_user schema has more required properties and
    %% deliv_user_json:validate_update/2 does not yet remove those requirements.
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"first">>, <<"Andrew">>},
                  {<<"last">>, <<"Bathory">>},
                  {<<"email">>, <<"z@ffdp.com">>},
                  {<<"user_type">>, <<"internal">>},
                  {<<"telemetry_enabled">>, true}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"internal">>))),

    ?assertEqual(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_external_for_valid_external_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"external">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"external">>))),

    ?assertEqual(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_saml_for_valid_saml_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"saml">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"saml">>))),

    ?assertEqual(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_external_for_valid_internal_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"external">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"internal">>))),

    ?assertEqual(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_saml_for_valid_internal_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"saml">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"internal">>))),

    ?assertEqual(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_external_for_valid_saml_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"external">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"saml">>))),

    ?assertEqual(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_saml_for_valid_external_user_json_returns_valid_json() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"saml">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"external">>))),

    ?assertEqual(ValidJson,
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_internal_for_valid_external_user_json_returns_error() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"internal">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"external">>))),

    ?assertEqual({error, invalid_type_change},
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.

validate_update_with_user_type_set_to_internal_for_valid_saml_user_json_returns_error() ->
    ValidJson = {[{<<"name">>, <<"ab">>},
                  {<<"user_type">>, <<"internal">>}
                 ]},
    hoax:mock(deliv_user,
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"saml">>))),

    ?assertEqual({error, invalid_type_change},
                 deliv_user_json:validate_update(user, ValidJson)),
    ?verifyAll.
