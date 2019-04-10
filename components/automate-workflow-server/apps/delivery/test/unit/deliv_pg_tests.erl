-module(deliv_pg_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

translate_error_test_() ->
    Tests = [
             %% {Input, Expected}
             {<<"CD004">>, {not_found, user}},
             {{<<"CD004">>, <<"blah">>}, {not_found, user}},
             {<<"blah">>, {system_error, <<"blah">>}},
             {{eek, ohno}, {system_error, eek}}
            ],
    [ ?_assertEqual(Expect, deliv_pg:translate_error(In))
      || {In, Expect} <- Tests ].
