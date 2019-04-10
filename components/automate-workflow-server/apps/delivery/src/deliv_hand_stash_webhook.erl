-module(deliv_hand_stash_webhook).
-behaviour(deliv_rest).

%% Cowboy API
-export([
         allowed_methods/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         handle/2
        ]).

%% @see cowboy_http_handler:init/3
init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

%% @see cowboy_http_handler:resit_init/2
rest_init(Req, State) ->
    {ok, Req, State}.

%% @see cowboy_http_handler:allowed_methods/2
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% @see cowboy_http_handler:content_types_accepted/2
content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

handle(Req, State) ->
    chef_log:info("Got request ~p with state ~p", [Req, State]),
    {true, Req, State}.
