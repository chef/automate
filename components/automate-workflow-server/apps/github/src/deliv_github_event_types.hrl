-include_lib("delivery/include/deliv_coordinates.hrl").
-record(github_pr, {
          action :: opened | synchronized | closed,
          proj_coordinates :: #proj_coordinates{},
          payload :: json()
         }).

-record(github_comment, {
          action :: created,
		  proj_coordinates :: #proj_coordinates{},
          payload :: json()
         }).

-type github_input_event() :: #github_pr{} | #github_comment{} | {error, undefined_event}.
