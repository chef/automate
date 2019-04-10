-module(audit_routes).

-export([
         routes/0
        ]).


routes() ->
    [
     {deliv_web_utils:route_prefix() ++ ":ent_name/audit",
      audit_hand_audit, []}
    ].
