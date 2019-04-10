-module(deliv_user_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

deliv_user_test_() ->
    [
     hoax:fixture(?MODULE, "fetch"),
     hoax:fixture(?MODULE, "email_user"),
     hoax:fixture(?MODULE, "verify_password"),
     hoax:fixture(?MODULE, "user_description_for_pr_comment"),
     hoax:fixture(?MODULE, "get_token")
    ].

fetch_by_ent_name_user_name_returns_error_reason_on_failure() ->
    EntName = <<"VenTech">>,
    UserName = <<"hankventure">>,

    hoax:mock(deliv_db,
              ?expect(fetch,
                      ?withArgs([deliv_user, [EntName], UserName]),
                      ?andReturn({error, oops}))),

    Result = deliv_user:fetch(EntName, UserName),

    ?assertEqual({error, oops}, Result),
    ?verifyAll.

email_user_returns_db_error_when_db_fetch_fails() ->
    EntName = <<"VenTech">>,
    UserName = <<"hankventure">>,

    hoax:mock(deliv_db,
              ?expect(fetch,
                      ?withArgs([deliv_user, [EntName], UserName]),
                      ?andReturn({error, unreachable}))),

    Result = deliv_user:email_user(EntName, UserName, send_email_fun),

    ?assertEqual({error, user_not_found, unreachable}, Result),
    ?verifyAll.

verify_password_when_given_internal_user_calls_deliv_intern_user_auth() ->
    EntName = <<"VenTech">>,
    UserName = <<"hankventure">>,
    Password = <<"letmein">>,

    hoax:mock(deliv_intern_user_authn,
              ?expect(verify_password,
                      ?withArgs([EntName, UserName, Password]),
                      ?andReturn(verified))),

    Actual = deliv_user:verify_password(<<"internal">>, UserName, EntName, Password),
    ?assertEqual(verified, Actual),
    ?verifyAll.

verify_password_when_given_external_user_calls_deliv_ldap() ->
    EntName = <<"VenTech">>,
    UserName = <<"hankventure">>,
    Password = <<"letmein">>,

    hoax:mock(deliv_ldap,
              ?expect(verify_password,
                      ?withArgs([UserName, Password]),
                      ?andReturn(verified))),

    Actual = deliv_user:verify_password(<<"external">>, UserName, EntName, Password),
    ?assertEqual(verified, Actual),
    ?verifyAll.

verify_password_when_given_saml_user_returns_error() ->
    EntName = <<"VenTech">>,
    UserName = <<"hankventure">>,
    Password = <<"letmein">>,

    Actual = deliv_user:verify_password(<<"saml">>, UserName, EntName, Password),
    ?assertEqual({error, saml_password_authn_not_supported}, Actual),
    ?verifyAll.

user_description_for_pr_comment_when_no_user_returns_username_with_description() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    Message =  <<"This Pull Request was opened by Chef Automate user ", UserName/binary, "\n\n">>,
    hoax:expect(receive
                        deliv_db:fetch(deliv_user, [EntName], UserName) -> {error, why}
                end),

    Result = deliv_user:user_description_for_pr_comment(EntName, UserName),
    ?assertEqual(Message, Result),
    ?verifyAll.

user_description_for_pr_comment_when_given_user_record_with_first_and_last_name_returns_them_in_description() ->
    EntName = <<"VenTech">>,
    UserName = <<"Ventech Industries">>,
    FirstName = <<"Hank">>,
    LastName = <<"Venture">>,
    UserRecord = deliv_user:fromlist([{first_name, FirstName}, {last_name, LastName}, {name, UserName}, {ent_name, EntName}]),
    Message = <<"This Pull Request was opened by Chef Automate user ", FirstName/binary, " ", LastName/binary,  "\n\n">>,

    hoax:expect(receive
                deliv_db:fetch(deliv_user, [EntName], UserName) -> {ok, UserRecord}
            end),

    Result = deliv_user:user_description_for_pr_comment(EntName, UserName),
    ?assertEqual(Message, Result),
    ?verifyAll.

user_description_for_pr_comment_when_given_user_record_without_first_or_last_name_returns_username_in_description() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    UserRecord = deliv_user:fromlist([{name, UserName}, {ent_name, EntName}]),
    Message =  <<"This Pull Request was opened by Chef Automate user ", UserName/binary, "\n\n">>,

    hoax:expect(receive
                deliv_db:fetch(deliv_user, [EntName], UserName) -> {ok, UserRecord}
            end),

    Result = deliv_user:user_description_for_pr_comment(EntName, UserName),
    ?assertEqual(Message, Result),
    ?verifyAll.

user_description_for_pr_comment_when_given_user_record_with_first_but_no_last_name_returns_first_name() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    LastName = <<"Venture">>,
    UserRecord = deliv_user:fromlist([{last_name, LastName}, {name, UserName}, {ent_name, EntName}]),
    Message = <<"This Pull Request was opened by Chef Automate user ", LastName/binary, "\n\n">>,

    hoax:expect(receive
                deliv_db:fetch(deliv_user, [EntName], UserName) -> {ok, UserRecord}
            end),

    Result = deliv_user:user_description_for_pr_comment(EntName, UserName),
    ?assertEqual(Message, Result),
    ?verifyAll.

user_description_for_pr_comments_when_given_user_record_with_no_first_but_last_name_returns_last_name() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    FirstName = <<"Hank">>,
    UserRecord = deliv_user:fromlist([{first_name, FirstName}, {name, UserName}, {ent_name, EntName}]),
    Message = <<"This Pull Request was opened by Chef Automate user ", FirstName/binary, "\n\n">>,

    hoax:expect(receive
                deliv_db:fetch(deliv_user, [EntName], UserName) -> {ok, UserRecord}
            end),

    Result = deliv_user:user_description_for_pr_comment(EntName, UserName),
    ?assertEqual(Message, Result),
    ?verifyAll.

get_token_returns_unauthorized_when_response_is_denied() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    Req = {},
    State = handler_state,
    ExpectedResponse = {halt, output_req, State},

    hoax:expect(receive
        deliv_web_utils:reply_unauthorized(Req, State) -> ExpectedResponse
    end),

    ActualResponse = deliv_user:get_token(denied, EntName, UserName, Req, State),

    ?assertEqual(ExpectedResponse, ActualResponse),
    ?verifyAll.

get_token_returns_bad_request_when_response_is_error_saml_password_authn_not_supported() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    Req = {},
    State = handler_state,
    ExpectedResponse = {halt, output_req, State},

    hoax:expect(receive
        deliv_web_utils:error_response(400, bad_request, <<"Password authentication for SAML user is not supported.">>, Req, State) -> ExpectedResponse
    end),

    ActualResponse = deliv_user:get_token({error, saml_password_authn_not_supported}, EntName, UserName, Req, State),

    ?assertEqual(ExpectedResponse, ActualResponse),
    ?verifyAll.

get_token_returns_internal_server_error_when_response_is_another_error() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    Req = {},
    State = handler_state,
    ExpectedResponse = {halt, output_req, State},

    hoax:expect(receive
        deliv_web_utils:error_response(500, internal_server_error, Req, State) -> ExpectedResponse
    end),

    ActualResponse = deliv_user:get_token({error, some_other_error}, EntName, UserName, Req, State),

    ?assertEqual(ExpectedResponse, ActualResponse),
    ?verifyAll.

get_token_returns_internal_server_error_when_assign_token_returns_an_error() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    Req = {},
    State = #handler{},
    ExpectedResponse = {halt, output_req, State},

    hoax:expect(receive
        deliv_web_utils:error_response(500, internal_server_error, Req, State) -> ExpectedResponse;
        deliv_token:assign_token(EntName, UserName) -> {error, some_reason}
    end),

    ActualResponse = deliv_user:get_token(verified, EntName, UserName, Req, State),

    ?assertEqual(ExpectedResponse, ActualResponse),
    ?verifyAll.

get_token_returns_json_containing_user_token_and_token_ttl_when_assign_token_returns_ok_token() ->
    EntName = <<"VenTech">>,
    UserName = <<"Hank Venture">>,
    Req = {},
    TTL = 500,
    State = #handler{read_ttl = TTL},
    Token = <<"token">>,
    ExpectedEJson = {[{<<"token">>, Token}, {<<"ttl">>, TTL}]},
    ExpectedResponse = {true, some_json, State},

    hoax:expect(receive
        deliv_token:assign_token(EntName, UserName) -> {ok, Token};
        deliv_web_utils:set_json_body(ExpectedEJson, Req) -> some_json
    end),

    ActualResponse = deliv_user:get_token(verified, EntName, UserName, Req, State),

    ?assertEqual(ExpectedResponse, ActualResponse),
    ?verifyAll.
