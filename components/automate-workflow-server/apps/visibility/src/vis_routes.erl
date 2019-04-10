-module(vis_routes).

-include_lib("delivery/include/deliv_types.hrl").

-export([
         routes/0
        ]).

routes() ->
    State = deliv_routes:set_ttls(#handler{}),
    [
     {
      route("count/nodes"),
      vis_hand_count_nodes,
      State
     },
     {
      route("count/cookbooks"),
      vis_hand_count_cookbooks,
      State
     },
     {
      route("count/changes"),
      vis_hand_count_changes,
      State
     },
     {
      route("timeseries/converges"),
      vis_hand_timeseries_converges,
      State
     }
    ].

route(Path) ->
    "/api/v0/visibility/" ++ Path.
