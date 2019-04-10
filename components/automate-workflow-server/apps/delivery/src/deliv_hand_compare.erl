-module(deliv_hand_compare).
-behaviour(deliv_rest).

%% FIXME: when our current endpoint replies with an empty content for a file,
%% there's no way of knowing if the file was not found for that revision, or
%% if it's there, but empty... that's sad!
%% We don't see permissions either... we should.

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
    %% let's first check that the QS values are correct
    QsKeys = [<<"start">>, <<"end">>, <<"file">>, <<"context">>],
    case deliv_web_utils:process_qs_vals(Req, fun qs_val/2, QsKeys) of
        {error, Why} ->
            Msg = erlang:iolist_to_binary(Why),
            deliv_web_utils:error_response(400, Msg, Req, State);
        {ok, Req2, ProcessedQsValues} ->
            case do_to_json(Req2, ProcessedQsValues) of
                {ok, Output} ->
                    {Output, Req2, State};
                {error, {ErrCode, ErrIoList}} ->
                    ErrMsg = erlang:iolist_to_binary(ErrIoList),
                    deliv_web_utils:error_response(ErrCode, ErrMsg, Req, State)
            end
    end.

qs_val(<<"start">>, undefined) -> <<"base">>;
qs_val(<<"start">>, <<"base">>) -> <<"base">>;
qs_val(<<"start">>, StartPatch) -> patchset_num_from_param(StartPatch);
qs_val(<<"context">>, undefined) -> context_unspecified;
qs_val(<<"context">>, Context) -> Context;
qs_val(QsKey, undefined) -> {error, [QsKey, " is a mandatory query parameter"]};
qs_val(<<"end">>, EndPatch) -> patchset_num_from_param(EndPatch);
qs_val(<<"file">>, File) -> File.

patchset_num_from_param(<<"p", SeqNumberBin/binary>>) ->
    case chef_utils:to_int(SeqNumberBin) of
        {error, not_an_int} -> patchset_num_from_qs_error_msg(SeqNumberBin);
        SeqNumber -> SeqNumber
    end;
patchset_num_from_param(Other) ->
    patchset_num_from_qs_error_msg(Other).

patchset_num_from_qs_error_msg(Input) ->
    {error, [Input, " is not a valid input; must be of the form 'pN' "
                    "where N is a valid patchset number (can also be 'base' "
                    "for the 'start' parameter)"]}.

do_to_json(Req, ProcessedQsValues) ->
    %% let's check that this change exists with
    %% the given scoping params
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {OrgName, Req2} = cowboy_req:binding(org_name, Req1),
    {ProjName, Req3} = cowboy_req:binding(proj_name, Req2),
    {ChangeId, _Req4} = cowboy_req:binding(change_id, Req3),

    case deliv_change:scoping_names(ChangeId) of
        [EntName, OrgName, ProjName, PipeName] ->
            %% the 2 following DB calls are 'guaranteed' to succeed since we've just
            %% returned from `deliv_change:scoping_names' (modulo a possible race
            %% condition with another request, but then IMHO it's fine to crash with
            %% a 500)
            {ok, RepoBin} = deliv_project:repo_path(EntName, OrgName, ProjName),
            Repo = chef_utils:to_str(RepoBin),
            handle_extract_shas(extract_shas(PipeName, ChangeId, Repo, ProcessedQsValues),
                                Repo, ProcessedQsValues);
        Other ->
            chef_log:debug("Unexpected change's scoping names ~p : ~p VS expected ~p",
                            [ChangeId, Other, {EntName, OrgName, ProjName}]),
            {error, 404, "change not found"}
    end.

handle_extract_shas({error, _} = Error, _Repo, _ProcessedQsValues) ->
    Error;
handle_extract_shas({ok, StartSha, EndSha}, Repo, ProcessedQsValues) ->
    %% if there's an error when fetching the files from git, we're gonna
    %% crash with a 500, but there's little else we can do anyway, so no
    %% need to guard against that here
    %% we've already checked that the file param is set!
    File = proplists:get_value(<<"file">>, ProcessedQsValues),
    Context = proplists:get_value(<<"context">>, ProcessedQsValues, context_unspecified),
    {ok, Diff} = deliv_git:file_diff(Repo, StartSha, EndSha, File, Context),
    {ok, make_json(StartSha, EndSha, Diff)}.

extract_shas(PipeName, ChangeId, Repo, ProcessedQsValues) ->
    StartPatch = proplists:get_value(<<"start">>, ProcessedQsValues),
    EndPatch = proplists:get_value(<<"end">>, ProcessedQsValues),
    EndShaResult = patchset_to_sha(EndPatch, PipeName, ChangeId, Repo, null),
    extract_start_sha(EndShaResult, StartPatch, PipeName, ChangeId, Repo).

extract_start_sha({error, _} = Error, _StartPatch, _PipeName, _ChangeId, _Repo) ->
    Error;
extract_start_sha({ok, EndSha}, StartPatch, PipeName, ChangeId, Repo) ->
    case patchset_to_sha(StartPatch, PipeName, ChangeId, Repo, EndSha) of
        {ok, StartSha} ->
            {ok, StartSha, EndSha};
        {error, _} = Error ->
            Error
    end.

patchset_to_sha(<<"base">>, PipeName, _ChangeId, Repo, EndSha) when EndSha =/= null ->
    case deliv_git:merge_base(Repo, PipeName, EndSha) of
        {error, Why} ->
            chef_log:error("Failed to get merge base between ~p and ~p : ~p",
                            [PipeName, EndSha, Why]),
            {error, {500, "could not resolve base commit"}};
        Sha ->
            {ok, Sha}
    end;
patchset_to_sha(SeqNumber, _PipeName, ChangeId, _Repo, _EndSha) ->
    case deliv_patchset:fetch(ChangeId, SeqNumber) of
        [Patchset] -> {ok, deliv_patchset:getval(sha, Patchset)};
        _ -> {error, {404, "patchset not found"}}
    end.

make_json(StartSha, EndSha, Diff) ->
    chef_json:encode(
      {[
        {<<"diff">>,
         {[
           {<<"start_sha">>, StartSha},
           {<<"end_sha">>, EndSha},
           {<<"content">>, Diff}
          ]}}]}).
