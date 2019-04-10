%% @doc Stub handler module for serving static JSON.
%%
%% This is a vanilla cowboy handler module that does not use
%% `cowboy_rest'. For our purposes of blindly returning some JSON from
%% a file, this is an easier approach.
%%
%% We search for files in `priv/stubs/' matching the pattern
%% `$METHOD.$NAME' where `$NAME' comes from the `stub' field of the
%% handler state (set in dispatch config) and `$METHOD' is taken from
%% the request.
-module(deliv_hand_stub).
-behaviour(deliv_rest).

-include("deliv_types.hrl").

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

init(_Transport, Req, State) ->
    {ok, Req, State}.

handle(Req, #handler{stub = StubFile}) ->
    {Method, _} = cowboy_req:method(Req),
    Priv = chef_utils:priv_dir_for_mod(?MODULE),
    FullPath = erlang:iolist_to_binary([Priv, "/stubs/", Method, ".", StubFile]),
    case file:read_file(erlang:binary_to_list(FullPath)) of
        {ok, Json} ->
            cowboy_req:reply(200,
                            [{<<"content-type">>, <<"application/json">>}],
                            Json, Req);
        {error, enoent} ->
            chef_log:error("couldn't find stub file '~s'", [FullPath]),
            cowboy_req:reply(404, [], <<"missing stub\n">>, Req);
        {error, Why} ->
            chef_log:error("error reading stub file '~s': ~p", [FullPath, Why]),
            cowboy_req:reply(500, [], <<"bad_stub\n">>, Req)
    end.

terminate(_, _, _) ->
    ok.
