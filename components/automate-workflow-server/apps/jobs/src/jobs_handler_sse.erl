-module(jobs_handler_sse).
-behaviour(deliv_web_sse).

-include_lib("mixer/include/mixer.hrl").

-export([init/3,
         events/2,
         format_event/4,
         forbidden_callback/0
         ]).

-mixin([{deliv_web_sse, [handle/2, info/3, terminate/3]}]).

forbidden_callback() -> forbidden_none.

init(_Transport, Req, State) ->
    deliv_web_sse:init(?MODULE, Req, State).

events(Req, State) ->
    {Req, State, [update_job_queue]}.

format_event(update_job_queue, Jobs, Req, State) ->
    Id = chef_utils:random_hex_string(16),
    IOData = deliv_web_sse:format_event(Id, <<"update_job_queue">>, {ejson, jobs_job_json:to_json(Jobs)}),
    {keep_open, Req, State, IOData}.
