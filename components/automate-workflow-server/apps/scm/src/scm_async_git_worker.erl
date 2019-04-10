%% ===================================================================
%% Bitbucket async worker - Runs git commands asynchronously
%% ===================================================================
-module(scm_async_git_worker).

-behavior(gen_server).

-include_lib("delivery/include/deliv_coordinates.hrl").

%% ===================================================================
%% Our API
%% ===================================================================
-export([start_link/2]).

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
-spec start_link(force_push | delete_branch, [term()]) -> {ok, pid()} |
                                                          ignore |
                                                          {error, term()}.
start_link(Action, Args) ->
    gen_server:start_link(?MODULE, {Action, Args}, []).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================
-spec init(term()) -> {ok, term(), 0}.
init(State) ->
    {ok, State, 0}.

-spec handle_info(timeout | term(),
                  {force_push | delete_branch, [term()]} | term()) -> {stop, normal, term()} | {noreply, term()}.
handle_info(timeout, {force_push, [SourceBranch, DestBranch, #proj_coordinates{ent_name = EntName,
                                                                               org_name = OrgName,
                                                                               proj_name = ProjName} = Coords]} = State) ->
    case scm_git_client:force_push(SourceBranch, DestBranch, Coords) of
        {ok, _} ->
            chef_log:info("Successfully pushed branch ~s of project ~s/~s/~s to Bitbucket branch ~s.",
                           [SourceBranch, EntName, OrgName, ProjName, DestBranch]);
        {error, _} ->
            chef_log:error("Failed to push branch ~s of project ~s/~s/~s to Bitbucket branch ~s.",
                            [SourceBranch, EntName, OrgName, ProjName, DestBranch])
    end,
    {stop, normal, State};
handle_info(timeout, {delete_branch, [DestBranch, #proj_coordinates{ent_name = EntName,
                                                                               org_name = OrgName,
                                                                               proj_name = ProjName} = Coords]} = State) ->
    case scm_git_client:delete_branch(DestBranch, Coords) of
        {ok, _} ->
            chef_log:info("Successfully deleted branch ~s of project ~s/~s/~s.",
                           [DestBranch, EntName, OrgName, ProjName]);
        {error, _} ->
            chef_log:error("Failed to delete branch ~s of project ~s/~s/~s.",
                            [DestBranch, EntName, OrgName, ProjName])
    end,
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_call(term(), term(), term()) -> {noreply, term()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), term()) -> {noreply, term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), term()) -> ok.
terminate(_Reason, _State) ->
    scm_git_worker:request_finished(),
    ok.

-spec code_change(term() | {down, term()}, term(), term()) -> {ok, term()}.
code_change(_Oldvsn, State, _Extra) ->
    {ok, State}.
