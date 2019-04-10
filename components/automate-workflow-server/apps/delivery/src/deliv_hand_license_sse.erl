-module(deliv_hand_license_sse).
-behaviour(deliv_web_sse).

-include("deliv_types.hrl").

-include_lib("mixer/include/mixer.hrl").

-export([init/3]).

%% Required by deliv_web_sse
-export([events/2,
         format_event/4]).

-mixin([{deliv_web_sse, [handle/2, info/3, terminate/3]}]).

init(_Transport, Req, State) ->
    deliv_web_sse:init(?MODULE, Req, State).

events(Req, State) ->
    Events = [license],
    {Req, State, Events}.

-spec format_event(Event, Data, Req, State) -> {keep_open, Req, State, IOData} when
      Event :: license,
      Data :: {binary(), json()},
      Req :: cowboy_req(),
      State :: #handler{},
      IOData :: iodata().
format_event(license, {Status, Data}, Req, State) ->
    Ejson = {[{<<"status">>, Status},
              {<<"license_data">>, Data}]},
    Id = chef_utils:random_hex_string(16),
    IOData = deliv_web_sse:format_event(Id, <<"license">>, {ejson, Ejson}),
    {keep_open, Req, State, IOData}.
