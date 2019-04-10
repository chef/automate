-module(deliv_github_routes).

-export([
         routes/0
        ]).

-include_lib("delivery/include/deliv_types.hrl").

routes() ->
    State = deliv_routes:set_ttls(#handler{}),
    [
     {"/api/" ++ ?DELIV_API_VERSION ++ "/github_auth_callback",
      deliv_hand_github_auth_callback, State},

     {deliv_web_utils:route_prefix()
      ++ "[:ent_name]/orgs/[:org_name]/projects/[:proj_name]/github-webhook",
      deliv_hand_github_webhook, State}
    ].
