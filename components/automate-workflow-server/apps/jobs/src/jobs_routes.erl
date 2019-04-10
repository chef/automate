-module(jobs_routes).

-include("jobs_types.hrl").

-export([
         routes/0
        ]).

-define(ALL_BUT_OBSERVER, lists:delete(<<"observer">>, ?ALL_ROLES)).

routes() ->
    State = deliv_routes:set_ttls(#handler{}),
    [
     {
      route(":ent_name/runners"),
      jobs_hand_runners,
      new_handler_state(State, [{to_bin(get), ?ALL_ROLES},
                                {to_bin(post), [<<"admin">>]}])
     },
     {
      route(":ent_name/runners/streaming"),
      jobs_hand_runners_sse,
      new_handler_state(State, [{to_bin(get), ?ALL_ROLES}])
     },
     {
      route(":ent_name/runners/:hostname"),
      jobs_hand_runners_named,
      new_handler_state(State, [{to_bin(get), ?ALL_ROLES},
                                {to_bin(delete), [<<"admin">>]}])
     },
     {
      route(":ent_name/runners/:hostname/health"),
      jobs_hand_runners_named_health,
      new_handler_state(State, [{to_bin(post), ?ALL_ROLES}])
     },
     {
      route(":ent_name/jobs"),
      jobs_handler,
      new_handler_state(State, [{to_bin(get), ?ALL_ROLES}])
     },
     {
      route(":ent_name/jobs/streaming"),
      jobs_handler_sse,
      new_handler_state(State, [{to_bin(get), ?ALL_ROLES}])
     },
     {
      route(":ent_name/jobs/:job_id"),
      jobs_handler_named,
      new_handler_state(State, [{to_bin(get), ?ALL_ROLES},
                                {to_bin(delete), ?ALL_BUT_OBSERVER}])
     }
    ].

new_handler_state(State, Rules) ->
    State#handler{authz = {enterprise, Rules}}.

route(Path) ->
    deliv_web_utils:route_prefix() ++ Path.

to_bin(HttpMethod) ->
    deliv_web_utils:bin_method(HttpMethod).
