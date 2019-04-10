%% This code is currently a prototype for sending events to hipchat.
%%
%% If there is no configuration file in /etc/delivery/deivery-hichat.json,
%% nothing should happen. If there is content similar to:
%% {
%%   "delivery-hipchat-config" :
%%     [
%%       {
%%           "event_topic" : "{stage_run, failures}",
%%           "hipchat_url" : "https://api.hipchat.com/v2/room",
%%           "hipchat_token" : "YOUR HIPCHAT TOKEN HERE",
%%           "room_id" : "YOUR ROOM NAME HERE",
%%           "sender" : "CHEF Delivery",
%%           "color" : "red",
%%           "mentions" : "",
%%           "notify" : "true",
%%           "retry_interval" : 1,
%%           "retry_times" : 1
%%        },
%%        { ...another config here ...}
%%    ]
%% }
%%
%% gen_event handlers for hipchat will be created, one per coniguration group.
%%
%% event_topic should match with the deliv_event key that is being published to;
%% for the moment, {stage_run, failures} only.
%%
%% The list of event handlers can be longer than one, so matching up event keys
%% to different rooms, or colored messages, is possible.
-module(deliv_hipchat_sup).
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).
-define(CONFIG_FILE, "/etc/delivery/delivery-hipchat.json").

start_link() ->
    {ok, Pid} = supervisor:start_link(deliv_hipchat_sup, []),
    start_handlers(),
    {ok, Pid}.

init([]) ->
    Children = [
                {deliv_hipchat_event, {gen_event, start_link, [{local, deliv_hipchat_event}]},
                 permanent, 5000, worker, [dynamic]}
               ],
    {ok, { {one_for_one, 10, 60}, Children } }.

start_handlers() ->
    case chef_utils:read_config_file(?CONFIG_FILE) of
        {ok, Bin} ->
            Config = chef_json:decode(Bin),
            chef_log:debug("Decoded config:~p", [Config]),
            start_handlers(Config);
        {error, Reason} ->
            chef_log:info("Could not open hipchat config file ~p: ~p", [?CONFIG_FILE, Reason]),
            {error, no_config_file}
    end.

start_handlers({[{<<"delivery-hipchat-config">>, Config}]}) ->
    chef_log:info("Starting hipchat handlers."),
    [ start_hipchat_handler(C) || {C} <- Config ];
start_handlers(BadConfig) ->
    chef_log:info("Configuration doesn't look right:~p", [BadConfig]).

start_hipchat_handler(Config) ->
    chef_log:debug("start_hipchat_handler got Config:~p", [Config]),
    Topic = chef_utils:string_to_term(chef_utils:to_str(proplists:get_value(<<"event_topic">>, Config))),
    Url = chef_utils:to_str(proplists:get_value(<<"hipchat_url">>, Config)),
    ApiToken = chef_utils:to_str(proplists:get_value(<<"hipchat_token">>, Config)),
    RoomId = chef_utils:to_str(proplists:get_value(<<"room_id">>, Config)),
    Sender  = chef_utils:to_str(proplists:get_value(<<"sender">>, Config)),
    Color = chef_utils:to_str(proplists:get_value(<<"color">>, Config)),
    Mentions = chef_utils:to_str(proplists:get_value(<<"mentions">>, Config)),
    Notify = chef_utils:to_str(proplists:get_value(<<"notify">>, Config)),
    RetryInterval = proplists:get_value(<<"retry_interval">>, Config),
    RetryTimes = proplists:get_value(<<"retry_times">>, Config),
    Args = [Topic, Url, ApiToken, RoomId, Sender, Color, Mentions, Notify, RetryTimes, RetryInterval],
    chef_log:debug("init args ~p:~p:~p:~p:~p:~p:~p:~p:~p:~p", [Args]),

    gen_event:add_handler(deliv_hipchat_event, deliv_hipchat_notify, Args).
