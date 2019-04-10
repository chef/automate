%% ===================================================================
%% Bitbucket worker - Runs git commands
%% ===================================================================
-module(scm_git_worker).

-behavior(gen_server).

-include_lib("delivery/include/deliv_coordinates.hrl").

%% ===================================================================
%% Our API
%% ===================================================================
-export([start_link/0,
         force_push/3,
         delete_branch/2,
         request_finished/0]).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================
-export([init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).

%% ===================================================================
%% Our API
%% ===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec force_push(binary(), binary(), #proj_coordinates{}) -> ok.
force_push(SourceBranch, DestBranch, Coords) ->
    gen_server:cast(?MODULE, {force_push, [SourceBranch, DestBranch, Coords]}).

-spec delete_branch(binary(), #proj_coordinates{}) -> ok.
delete_branch(DestBranch, Coords) ->
    gen_server:cast(?MODULE, {delete_branch, [DestBranch, Coords]}).

-spec request_finished() -> ok.
 request_finished() ->
    gen_server:cast(?MODULE, request_finished).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================
%% State might look like {running | not_running, [NextRequest | _]}
-spec init(term()) -> {ok, term()}.
init([]) ->
    {ok, {not_running, []}}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_call(term(), term(), term()) -> {noreply, term()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), term()) -> {noreply, term()}.
handle_cast(request_finished, {running, []}) ->
    {noreply, {not_running, []}};
handle_cast(request_finished, {running, [Request | Rest]}) ->
    run_command(Request),
    {noreply, {running,Rest}};
handle_cast(Request, {running, Queue} ) ->
    {noreply, {running, Queue ++ [Request]}};
handle_cast({_, _} = Request,  _State) ->
    run_command(Request),
    {noreply, {running, []}};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term() | {down, term()}, term(), term()) -> {ok, term()}.
code_change(_Oldvsn, State, _Extra) ->
    {ok, State}.

run_command({Function, Args}) ->
    erlang:apply(scm_async_git_worker_sup, Function, Args).
