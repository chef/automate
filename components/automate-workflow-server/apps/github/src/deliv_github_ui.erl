%% @doc Module for human consumable communication between delivery and github
%%      It includes setting status as well as communication through comments
-module(deliv_github_ui).

-behaviour(gen_server).

-include_lib("delivery/include/deliv_types.hrl").

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Events = [build_event_for_change,
              github_pull_request_opened,
              github_pull_request_synchronize,
              github_comment_created],

    chef_log:debug("~s Subcribing to: ~p~n",
                    [chef_utils:capitalize_str(?MODULE), Events]),
    true = deliv_event:subscribe(Events),
    {ok, undefined}.

%% Handle the build_event_for_change which means creating or updating the
%% comment on the pr in github.
handle_info({_Pid, build_event_for_change, Change}, State) ->
    ChangeId = deliv_change:getval(id, Change),
    Scope = deliv_scopes:from_change_id(ChangeId),

    case deliv_scopes:'#get'(scm_module, Scope) of
        deliv_scm_github ->
            %% As we move more event driven these could really be there own
            %% gen_servers listening for the events they care about independently.
            %% TODO: Just pass scope it has the changeid.
            deliv_github_status:sync_status(Scope, ChangeId),
            deliv_github_status_comment:sync_comment(Scope, ChangeId),
            deliv_github_chatops:handle_build_event_for_change(Scope);
        _ ->
            chef_log:debug("Ignoring event for change ~p. Not a github project.")
    end,

    {noreply, State};
%% Handle the github_pull_request_opened which means adding a label to the pr
%% and setting the current commit to pending.
handle_info({_Pid, github_pull_request_opened, {ChangeId, Payload}}, State) ->
    Scope = deliv_scopes:from_change_id(ChangeId),
    PRNumber = deliv_github_pull_request:pull_request_number(Payload),

    add_label(Scope, PRNumber, <<"opened">>),
    deliv_github_status:sync_status(Scope, ChangeId),

    {noreply, State};
%% Handle the github_pull_request_synchronize which means setting the current
%% commit to pending.
handle_info({_Pid, github_pull_request_synchronize, {ChangeId, _Payload}}, State) ->
    Scope = deliv_scopes:from_change_id(ChangeId),

    deliv_github_status:sync_status(Scope, ChangeId),

    {noreply, State};
handle_info({_Pid, github_comment_created, Msg}, State) ->
    deliv_github_chatops:handle_comment_created(Msg),
    {noreply, State};
handle_info(Event, State) ->
    chef_log:debug("Unexpected message in ~p~n", [Event]),
    {noreply, State}.


%% We don't really expect this to be be called since all interaction
%% with this module should come through gproc events via handle_info.
handle_call(Request, _From, State) ->
    chef_log:info("Call: ~p", [Request]),
    {reply, ignored, State}.

%% We don't really expect this to be be called since all interaction
%% with this module should come through gproc events via handle_info.
handle_cast(Msg, State) ->
    chef_log:info("Cast: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    deliv_event:unsubscribe_all(),
    init([]).

add_label(Scope, PRNumber, <<"opened">>) ->
    Label = <<"Delivery Change Created">>,
    ChangeId = deliv_scopes:'#get'(change_id, Scope),

    case deliv_github_api:add_label_to_issue(Scope, PRNumber, Label) of
        {error, Why} = Err ->
            chef_log:info("add_label on github failed for change_id ~p with error ~p",
                           [ChangeId, Why]),
            Err;
        _ -> ok
    end.
