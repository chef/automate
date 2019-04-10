-module(deliv_stage_data).

-export([stages/0]).

-include("deliv_stage.hrl").

stages() ->
    ?STAGES_DATA.
