-module(deliv_notify).

-export([format/2,
         msg/1]).

-include("deliv_types.hrl").

%% @doc Send a notification using format syntax.
%%
%% The recipient of the notification is determined by configuration settings.
%% Typically, this will be a team chat channel
%%
%% *Note: We currently only support messaging a Hipchat channel
-spec format(string(), list()) -> ok | {error, any()}.
format(Msg, Arguments) ->
    MsgBin = erlang:list_to_binary(io_lib:format(Msg, Arguments)),
    chef_log:info("deliv_notify going to message: ~s", [MsgBin]),
    msg(MsgBin).

%% @doc Send a notification using default configuration.
%%
%% TODO: Actually check for errors and handle them nicely after
%% the lists:foreach/2 call
-spec msg(string () | binary()) -> ok | {error, any()}.
msg(Msg) ->
    MsgBin = chef_utils:to_bin(Msg),
    Res = lists:map(fun({Proto, Params}) ->
                            msg(Proto, Params, MsgBin)
                    end,
                    get_config()),
    case lists:filter(fun(X) -> X =/= ok end, Res) of
        [] ->
            ok;
        Errors ->
            {error, Errors}
    end.

-spec msg(hipchat | other, term(), binary()) -> ok | {error, binary()}.
msg(hipchat, {Room, Key}, Msg) ->
    Method = post,
    URL = <<"https://api.hipchat.com/v2/room/", Room/binary,
            "/notification?auth_token=", Key/binary >>,
    Payload = {[{<<"message">>, Msg}]},
    Resp = deliv_http:req(Method, URL, Payload),
    chef_log:debug("deliv_notify response Rsp: ~p", [Resp]),
    handle_rsp(Resp);
msg(Other, _Params, _Msg) ->
    {error, io_lib:format("~p protocol is not yet implemented.", [Other])}.


%%%%%%%%%%
%% private
%%%%%%%%%%

handle_rsp({ok, 204, _RespHeaders, _Body}) ->
    ok;
handle_rsp({ok, StatusCode, _RespHeaders, _Body}) ->
    {error, io_lib:format("Bad StatusCode: ~c", [StatusCode])};
handle_rsp({error, Reason}) ->
    {error, Reason}.

%% @doc Load configuration settings for notifications
%% Format is a list of tuples in form {protocol, Params},
%% where for example dropbox would be {dropbox, {"room", "key"}}
-spec get_config() -> list().
get_config() ->
    delivery_app:get_env(deliv_notify_config, []).
