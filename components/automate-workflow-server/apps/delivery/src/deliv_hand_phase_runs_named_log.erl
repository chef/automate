-module(deliv_hand_phase_runs_named_log).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/3,
         rest_init/2,
         handle/2
        ]).

-include("deliv_types.hrl").

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_plain_text(handle), Req, State}.

handle(Req, State) ->
    {Coords, Req1} = deliv_web_utils:extract_proj_coordinates(Req),
    {Pipe, Req2} = cowboy_req:binding(pipe_name, Req1),
    {PhaseRunId, Req3} = cowboy_req:binding(run_id, Req2),

    {ok, PhaseRun} = deliv_phase_run:fetch(chef_utils:to_int(PhaseRunId)),
    RunLog = deliv_phase_run:getval(run_log, PhaseRun),

    {Colorize, Req4} = cowboy_req:qs_val(<<"colorize">>, Req3, <<"false">>),

    Filename = deliv_phase_run:build_filename(Coords, Pipe, PhaseRun),
    Disposition = <<"attachment; filename=\"", Filename/binary, "\"">>,
    Req5 = cowboy_req:set_resp_header(<<"content-disposition">>, Disposition, Req4),

    do_handle(Colorize, RunLog, Req5, State).

do_handle(<<"true">>, RunLog, Req, State) ->
    {RunLog, Req, State};
do_handle(_, RunLog, Req, State) ->
    %% Regex used for matching ANSI color codes
    %% Adapted from https://github.com/chalk/ansi-regex/blob/master/index.js
    AnsiRegex = "[\x1b][[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]|[\x0f]",
    PlainRunLog = re:replace(RunLog, AnsiRegex, "", [global]),
    {PlainRunLog, Req, State}.
