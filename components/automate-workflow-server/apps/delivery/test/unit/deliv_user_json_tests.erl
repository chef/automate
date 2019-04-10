-module(deliv_user_json_tests).

-compile(export_all).

-include_lib("hoax/include/hoax.hrl").

deliv_user_json_test_() ->
    [
     hoax:fixture(?MODULE, "validate"),
     hoax:fixture(?MODULE, "translate_input")
    ].

validate_when_user_json_is_valid_returns_json() ->
    ValidJson = {[{<<"name">>, <<"stephen">>},
                  {<<"user_type">>, <<"saml">>}]},
    hoax:mock(chef_json,
              ?expect(validate,
                      ?withArgs([extern_user, ValidJson]),
                      ?andReturn(ValidJson))),

    ?assertEqual(ValidJson, deliv_user_json:validate(ValidJson)),
    ?verifyAll.

validate_when_user_json_is_invalid_and_without_user_type_uses_intern_user_schema_and_returns_error() ->
    InvalidJson = {[{<<"foo">>, <<"bar">>}]},
    hoax:mock(chef_json,
              ?expect(validate,
                      ?withArgs([intern_user, InvalidJson]),
                      ?andReturn({error, error_spec}))),

    ?assertEqual({error, error_spec}, deliv_user_json:validate(InvalidJson)),
    ?verifyAll.

validate_when_user_json_is_invalid_and_with_user_type_returns_error() ->
    InvalidJson = {[{<<"foo">>, <<"bar">>},
                    {<<"user_type">>, <<"saml">>}]},
    hoax:mock(chef_json,
              ?expect(validate,
                      ?withArgs([extern_user, InvalidJson]),
                      ?andReturn({error, error_spec}))),

    ?assertEqual({error, error_spec}, deliv_user_json:validate(InvalidJson)),
    ?verifyAll.

validate_update_when_internal_user_type_is_changed_to_saml_returns_json() ->
    User = user,
    UpdateJson = {[{<<"user_type">>, <<"saml">>}]},
    Schema = {[{<<"properties">>, {[{<<"name">>, {[]}},
                                    {<<"user_type">>, {[]}}]}
               }]},
    hoax:mock(deliv_user, [
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"internal">>))]),
    hoax:mock(chef_json,
              ?expect(read,
                      ?withArgs([extern_user]),
                      ?andReturn(Schema))),
    hoax:mock(chef_json,
              ?expect(validate,
                      ?withArgs([Schema, UpdateJson]),
                      ?andReturn(UpdateJson))),

    ?assertEqual(UpdateJson, deliv_user_json:validate_update(User, UpdateJson)),
    ?verifyAll.

validate_update_when_external_user_type_is_changed_to_internal_returns_error() ->
    User = user,
    UpdateJson = {[{<<"user_type">>, <<"internal">>}]},
    hoax:mock(deliv_user, [
              ?expect(getval,
                      ?withArgs([user_type, user]),
                      ?andReturn(<<"external">>))]),

    ?assertEqual({error, invalid_type_change}, deliv_user_json:validate_update(User, UpdateJson)),
    ?verifyAll.

translate_input_when_user_type_is_internal_returns_translate_json_key() ->
    User = deliv_user:fromlist([{name, <<"kimmy">>},
                                {user_type, <<"internal">>},
                                {first_name, <<"kimberly">>},
                                {last_name, <<"schmidt">>},
                                {email, <<"kim@bunkernet.arpa">>},
                                {telemetry_allowed, true}]),
    ActualFun = deliv_user_json:translate_input(User),
    ?assertEqual(fun deliv_user_json:translate_json_key/1, ActualFun).

%% used for verifying the returned function
verify_fun(ActualFun) ->
    ?assertEqual({key, first_name}, ActualFun(<<"first">>, <<"kimberly">>)),
    ?assertEqual({error, <<"The field 'first' is an externally mapped field and cannot be modified">>}, ActualFun(<<"first">>, <<"KIMBERLY">>)),
    ?assertEqual({key, last_name}, ActualFun(<<"last">>, <<"schmidt">>)),
    ?assertEqual({error, <<"The field 'last' is an externally mapped field and cannot be modified">>}, ActualFun(<<"last">>, <<"SCHMIDT">>)),
    ?assertEqual({key, email}, ActualFun(<<"email">>, <<"kim@bunkernet.arpa">>)),
    ?assertEqual({error, <<"The field 'email' is an externally mapped field and cannot be modified">>}, ActualFun(<<"email">>, <<"kim@aol.com">>)).

translate_input_when_user_type_is_external_returns_function_checking_mapped_fields() ->
    User = deliv_user:fromlist([{name, <<"kimmy">>},
                                {user_type, <<"external">>},
                                {first_name, <<"kimberly">>},
                                {last_name, <<"schmidt">>},
                                {email, <<"kim@bunkernet.arpa">>},
                                {telemetry_allowed, true}]),
    ActualFun = deliv_user_json:translate_input(User),
    verify_fun(ActualFun).

translate_input_when_user_type_is_saml_returns_function_checking_mapped_fields() ->
    User = deliv_user:fromlist([{name, <<"kimmy">>},
                                {user_type, <<"saml">>},
                                {first_name, <<"kimberly">>},
                                {last_name, <<"schmidt">>},
                                {email, <<"kim@bunkernet.arpa">>},
                                {telemetry_allowed, true}]),
    ActualFun = deliv_user_json:translate_input(User),
    verify_fun(ActualFun).

translate_input_when_user_type_gets_updated_passes_translation_function() ->
    User = deliv_user:fromlist([{name, <<"kimmy">>},
                                {user_type, <<"external">>},
                                {first_name, <<"kimberly">>},
                                {last_name, <<"schmidt">>},
                                {email, <<"kim@bunkernet.arpa">>},
                                {telemetry_allowed, true}]),
    ActualFun = deliv_user_json:translate_input(User),
    ?assertEqual({key, user_type}, ActualFun(<<"user_type">>, <<"saml">>)),
    ?assertEqual({key, user_type}, ActualFun(<<"user_type">>, <<"external">>)).

translate_input_when_record_is_undefined_depends_on_new_value_being_null() ->
    User = deliv_user:fromlist([{name, <<"kimmy">>},
                                {user_type, <<"external">>},
                                {first_name, undefined},
                                {last_name, <<"schmidt">>},
                                {email, <<"kim@bunkernet.arpa">>},
                                {telemetry_allowed, undefined}]),
    ActualFun = deliv_user_json:translate_input(User),
    ?assertEqual({key, first_name}, ActualFun(<<"first">>, null)), % EJSON
    ?assertEqual({error, <<"The field 'first' is an externally mapped field and cannot be modified">>}, ActualFun(<<"first">>, <<"KIMBERLY">>)).
