-module(deliv_hand_verify_token).
-behaviour(cowboy_http_handler).

%% This uses plain-cowboy, because we don't want to trick the cowboy_rest FSM
%% into doing what we want it to do if the behaviour we want is as simple as
%% this. For the same reason we ignore what deliv_token:is_authorized/2 does
%% to Req and State.

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

init(_Transport, Req, State) -> {ok, Req, State}.

handle(Req, State) ->
    handle_get_and_head(cowboy_req:method(Req), State).

handle_get_and_head({<<"GET">>, Req}, State) ->
    handle_request(Req, State);
handle_get_and_head({<<"HEAD">>, Req}, State) ->
    handle_request(Req, State);
handle_get_and_head({_, Req}, State) ->
    {ok, Req1} = cowboy_req:reply(405, Req),
    {ok, Req1, State}.

handle_request(Req, State) ->
    case deliv_token:is_authorized(Req, State) of
        {true, _Req, _State} ->
            {ok, Req1} = cowboy_req:reply(200, Req),
            {ok, Req1, State};
        {_, _Req, _State} ->
            {ok, Req1} = cowboy_req:reply(401, Req),
            {ok, Req1, State}
    end.

terminate(_Reason, _Req, _State) -> ok.
