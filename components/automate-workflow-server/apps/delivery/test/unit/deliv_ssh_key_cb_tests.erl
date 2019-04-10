-module(deliv_ssh_key_cb_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

extract_user_ent_should_pull_user_ent_when_no_email_test() ->
    ?assertEqual({<<"oliver">>, <<"Chef">>}, deliv_ssh_key_cb:extract_user_ent("oliver@Chef")).

extract_user_ent_should_pull_user_ent_when_email_test() ->
    ?assertEqual({<<"oliver@chef.io">>, <<"Chef">>}, deliv_ssh_key_cb:extract_user_ent("oliver@chef.io@Chef")).

extract_user_ent_should_pull_user_ent_when_email_malformed_test() ->
    ?assertEqual({<<"oliver@chef.io@chef.com">>, <<"Chef">>}, deliv_ssh_key_cb:extract_user_ent("oliver@chef.io@chef.com@Chef")).
