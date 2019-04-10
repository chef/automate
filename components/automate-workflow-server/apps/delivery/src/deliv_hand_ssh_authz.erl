%% @doc This module is called internally by our git hooks
%% It can only be called from the local box (enforced by nginx conf)
-module(deliv_hand_ssh_authz).
-behaviour(deliv_rest).

-export([
         init/3,
         rest_init/2,
         handle/2,
         allowed_methods/2,
         content_types_provided/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_plain_text(handle), Req, State}.

handle(Req, State) ->
    {UserName, Req1} = cowboy_req:qs_val(<<"user">>, Req),
    {EntName, Req2} = cowboy_req:qs_val(<<"ent">>, Req1),
    {OrgName, Req3} = cowboy_req:qs_val(<<"org">>, Req2),
    {ProjName, Req4} = cowboy_req:qs_val(<<"proj">>, Req3),
    {BinAction, Req5} = cowboy_req:qs_val(<<"action">>, Req4),
    Action = erlang:binary_to_existing_atom(BinAction, utf8),
    Result = deliv_git:authorized_git_action(EntName, OrgName, ProjName,
                                             UserName, Action),
    chef_log:info("ssh_authz for ~p => ~p",
                   [{EntName, OrgName, ProjName, UserName, Action}, Result]),
    {chef_utils:to_bin(Result), Req5, State}.
