-module(chef_log_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

-record(test_record, {foo :: binary()}).

-define(TEST_MSG, "Hi there! I'm a test message").
-define(TEST_MSG_WITH_FORMAT, "~g is the answer").
-define(FORMAT_ARGS, [42]).
-define(TEST_FOO_VALUE, <<"bar">>).
-define(TEST_RECORD, #test_record{foo = ?TEST_FOO_VALUE}).
-define(TEST_RECORD_FORMAT_ARGS, [test_record, ?TEST_FOO_VALUE]).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE, log).

log_basic() ->
    hoax:mock(lager,
              ?expect(log,
                      ?withArgs([info, self(), ?TEST_MSG, []]))),

    chef_log:log(info, ?TEST_MSG).

log_with_msg_format() ->
    hoax:mock(lager,
              ?expect(log,
                      ?withArgs([debug, self(), ?TEST_MSG_WITH_FORMAT, ?FORMAT_ARGS]))),

    chef_log:debug(?TEST_MSG_WITH_FORMAT, ?FORMAT_ARGS).

log_item() ->
    hoax:mock(lager, [
              ?expect(log,
                      ?withArgs([error, self(), "[~s ~p]: " ++ ?TEST_MSG, ?TEST_RECORD_FORMAT_ARGS])),
              ?expect(log,
                      ?withArgs([warning, self(), "[~s ~p]: " ++ ?TEST_MSG, ?TEST_RECORD_FORMAT_ARGS]))
    ]),
    chef_log:log_item(error, ?TEST_RECORD, ?TEST_MSG),
    chef_log:log_item(warning, [?TEST_RECORD], ?TEST_MSG).

log_item_with_msg_formats() ->
    hoax:mock(lager,
              ?expect(log,
                      ?withArgs([blah, self(), "[~s ~p]: " ++ ?TEST_MSG_WITH_FORMAT, ?TEST_RECORD_FORMAT_ARGS ++ ?FORMAT_ARGS]))),
    chef_log:log_item(blah, ?TEST_RECORD, ?TEST_MSG_WITH_FORMAT, ?FORMAT_ARGS).

log_failed_call() ->
    hoax:mock(lager,
              ?expect(log,
                      ?withArgs([error,
                                 self(),
                                 "failed_call={~p:~p, ~p}; reason=~p",
                                 [module, fun_name, [1, "", <<"">>], not_found]
                                ]))),
    chef_log:failed_call(module, fun_name, [1, "", <<"">>], not_found).
