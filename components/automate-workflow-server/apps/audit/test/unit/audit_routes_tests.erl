-module(audit_routes_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

routes_should_return_audits_route_list_test() ->
    ?assertEqual([{deliv_web_utils:route_prefix() ++
                       ":ent_name/audit", audit_hand_audit, []}], audit_routes:routes()).
