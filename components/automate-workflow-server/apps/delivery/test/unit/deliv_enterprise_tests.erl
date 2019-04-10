-module(deliv_enterprise_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

deliv_enterprise_test_() ->
    [
     hoax:fixture(?MODULE, "get_canonical_enterprise_")
    ].

get_canonical_enterprise_with_one_enteprise_returns_enterprise_name() ->
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_enterprise, get_canonical_enterprise, []) -> [<<"acme">>]
                end),

    Result = deliv_enterprise:get_canonical_enterprise(),

    ?assertEqual({ok, <<"acme">>}, Result),
    ?verifyAll.

get_canonical_enterprise_with_no_enteprise_returns_no_canonical_enterprise() ->
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_enterprise, get_canonical_enterprise, []) -> []
                end),

    Result = deliv_enterprise:get_canonical_enterprise(),

    ?assertEqual({error, no_canonical_enterprise}, Result),
    ?verifyAll.

get_canonical_enterprise_with_multiple_enteprises_returns_error() ->
    hoax:expect(receive
                    sqerl_rec:scalar_fetch(deliv_enterprise, get_canonical_enterprise, []) -> [<<"acme">>, <<"acme2">>, <<"acme3">>]
                end),

    Result = deliv_enterprise:get_canonical_enterprise(),

    ?assertEqual({error, no_canonical_enterprise}, Result),
    ?verifyAll.
