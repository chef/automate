-module(deliv_hipchat_notify).

-behaviour(gen_event).

-export([
         init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([init_event_stream/1, send_notification/1]).

-record(state, {
                    event_topic    :: term(),
                    notify_url     :: string(),
                    room_topic_url :: string(),
                    auth_token     :: string(),
                    room_id        :: string(),
                    sender         :: string(),
                    color          :: string(),
                    mentions       :: [string() | atom()],
                    notify         :: integer(),
                    retry_interval :: integer(),
                    retry_times    :: integer()
               }).

-define (API_REQUEST (Url, Data), {Url, [], "application/x-www-form-urlencoded", Data}).

% @doc subscribe to the configured event key. This will cause calls to deliv_event:publish(Topic)
% to send a message to this notifier, via handle_info, which in turn fires a notification.
init_event_stream(Topic) ->
    chef_log:info("Subscribing to ~p", [Topic]),
    deliv_event:subscribe(Topic).

init([EventTopic, HipchatUri, AuthToken, RoomId, Sender, Color, Mentions, Notify, RetryTimes, RetryInterval]) ->
    State = #state{
                   event_topic    = EventTopic,
                   notify_url     = HipchatUri ++ "/" ++ RoomId ++ "/notification",
                   room_topic_url = HipchatUri ++ "/" ++ RoomId ++ "/topic",
                   auth_token     = AuthToken,
                   room_id        = RoomId,
                   sender         = Sender,
                   color          = Color,
                   mentions       = Mentions,
                   notify         = Notify,
                   retry_interval = RetryInterval,
                   retry_times    = RetryTimes
                  },
    init_event_stream(EventTopic),
    {ok, State}.

%
% @doc notification API
send_notification(Msg) ->
    gen_event:notify(deliv_hipchat_event, {post_message, Msg}).

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({post_message, Message}, #state{ notify_url = NotifyUrl, auth_token = AuthToken,
                                              color = Color, mentions = Mentions,
                                              room_id = RoomId, sender = Sender,
                                              notify = Notify, retry_times = RetryTimes,
                                              retry_interval = RetryInterval, event_topic = _EventTopic} = State) ->

    Data = [{"format", "json"}, {"auth_token", AuthToken},
           {"room_id", RoomId}, {"color", Color},
           {"message_format", "text"}, {"message", string:concat(Mentions, Message)},
           {"notify", Notify}, {"from", Sender}],
    deliv_web_utils:http_post_retry(?API_REQUEST(NotifyUrl, deliv_web_utils:encode_url_parameters(Data)),
                                    RetryTimes, RetryInterval),
    {ok, State};
handle_event({set_room_topic, Message}, #state{ room_topic_url = RoomTopicUrl, auth_token = AuthToken,
                                                room_id = RoomId, retry_times = RetryTimes,
                                                retry_interval = RetryInterval, event_topic = _EventTopic} = State) ->

    Data = [{"format", "json"},
            {"auth_token", AuthToken},
            {"room_id", RoomId},
            {"topic", Message}],
    deliv_web_utils:http_post_retry(?API_REQUEST(RoomTopicUrl, deliv_web_utils:encode_url_parameters(Data)),
                                    RetryTimes, RetryInterval),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%
% @doc called when deliv_event:publish(Topic, ...) is called.
%
% gen_event:notify should be removed and another proc introduced to handle events.
handle_info({Pid, Topic, Msg}, #state{ event_topic = Topic } = State) ->
    chef_log:debug("Received event on topic ~p: ~p -> ~p: ~p\n", [Topic, Pid, self(), Msg]),
    gen_event:notify(deliv_hipchat_event, {post_message, Msg}),
    {ok, State};
handle_info({gen_event_EXIT, HandlerModule, Reason}, HandlerModule) ->
    chef_log:info("~w: detected handler ~p shutdown:~p",
              [?MODULE, HandlerModule, Reason]),
    {stop, {handler_died, HandlerModule, Reason},HandlerModule};
handle_info(Info, State) ->
    chef_log:info("unhandled EVENT ~p", [Info]),
    {ok, State}.
