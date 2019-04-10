-module(jobs_hand_runners_sse).
-behaviour(deliv_web_sse).

-include_lib("mixer/include/mixer.hrl").

-export([init/3,
         events/2,
         format_event/4,
         forbidden_callback/0
        ]).

-mixin([{deliv_web_sse, [handle/2, info/3, terminate/3]}]).

init(_Transport, Req, State) ->
    deliv_web_sse:init(?MODULE, Req, State).

events(Req, State) ->
    {Req, State, [runners_state_updated]}.

format_event(runners_state_updated, Runners, Req, State) ->
    RandomId = chef_utils:random_hex_string(16),
    EJson = jobs_runner_json:to_json(Runners),
    IoData = deliv_web_sse:format_event(RandomId, <<"runners_state_updated">>, {ejson, EJson}),
    {keep_open, Req, State, IoData}.

forbidden_callback() ->
    forbidden_none.
