-module(jobs_rest).

%% This module exports default methods that are mixed in this app's handlers to
%% keep them DRY.

-include("jobs_types.hrl").

-export([
         init/3,
         rest_init/2,
         resource_exists/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

%% Note that from here on, we replace the state variable used with the mixins
%% with the one specific to this handler: for passing the runner record.
resource_exists(Req, #handler{ent_name = EntName}) ->
    case cowboy_req:binding(hostname, Req) of
        {undefined, Req1} -> {false, Req1, #runner{}};
        {RunnerHostname, Req1} ->
            case jobs_runners_state:fetch(EntName, RunnerHostname) of
                [Runner] -> {true, Req1, Runner};
                [] -> {false, Req1, #runner{}}
            end
    end.
