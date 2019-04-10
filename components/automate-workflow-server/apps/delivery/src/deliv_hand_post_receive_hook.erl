%% @doc This module is called internally by our git hooks
%% It can only be called from the local box (enforced by nginx conf)
%% Meant to be called by the post-receive hook
%% Expects a flat JSON bdy comprising the following fields:
%% enterprise, user, organization, project, reference_name, to_commit
%% Returns a plain text that should be returned to the git client if not empty
%% TODO: there's room for a race condition here, if 2 users push to the
%% same feature branch at the same time; fine for now, but we should give
%% some thought about how to solve that issue; a very simple solution would
%% be to push to `_for/<pipe>/<sha>/<feat_branch_name>` instead of just
%% `_for/<pipe>/<feat_branch_name>`; the SHA information would just be
%% ignored, but would ensure that 2 people puhsing 2 different patchsets
%% on the same branch targeting the same pipeline at the same time wouldn't
%% interfere with each other.
-module(deliv_hand_post_receive_hook).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include("deliv_coordinates.hrl").

-export([
         init/3,
         rest_init/2,
         handle/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_plain_text(handle), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(handle), Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

handle(Req, State) ->
    handle_parse(deliv_web_utils:parse_json_req(Req, jesse_spec()), State).

handle_parse({{error, _Why}, Req}, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_parse({Json, Req}, State) ->
    RefName = ej:get([<<"reference_name">>], Json),
    case extract_pipe_and_feat_branch(RefName) of
        not_found ->
            %% no need to go any further
            {true, Req, State};
        {PipeName, FeatBranchName} ->
            Coords = #proj_coordinates{
                        ent_name = EntName = decode_from_json(<<"enterprise">>, Json),
                        org_name = OrgName = decode_from_json(<<"organization">>, Json),
                        proj_name = ProjName = decode_from_json(<<"project">>, Json)},
            UserName = decode_from_json(<<"user">>, Json),
            ToCommit = ej:get([<<"to_commit">>], Json),
            "refs/heads/" ++ RawBranchName = chef_utils:to_str(RefName),
            process_with_pipeline(deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName), Coords, PipeName, FeatBranchName, UserName, ToCommit, RawBranchName, Req, State)
    end.

process_with_pipeline({ok, _},
                      #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName} = Coords,
                      PipeName, FeatBranchName, UserName, ToCommit, RawBranchName, Req, State) ->
    {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
    ProjDir = chef_utils:to_str(deliv_project:repo_path(Project)),

    %% Check to see if the submitted SHA == SHA for TargetBranch
    case {ToCommit, deliv_git:rev_parse(PipeName, ProjDir)} of
        {<<"0000000000000000000000000000000000000000">>, _} ->
            %% Git uses all-zeros as the "null SHA1" meaning "nothing yet".
            %% This is the kind of SHA1 you get in Git receive/update hooks
            %% and new branches, representing an "old-ref" for a non-existent object.
            %% Since it is a non-existent object we should not save it as a patchset because
            %% it could cause problems when we go to look up this patchset later based
            %% on the SHA.
            Msg = <<"Could not create new patchset - invalid commit sha">>,
            deliv_web_utils:error_response(403, invalid_sha, Msg, Req, State);
        {C, C} ->
            %% The two SHAs match!
            %% Return HTTP 403/Forbidden. We don't need to worry
            %% about returning a CLI friendly response because
            %% the CLI handles this check locally.
            Msg = <<"Could not create new patchset - empty change">>,
            deliv_web_utils:error_response(403, empty_change, Msg, Req, State);
        {_C, _} ->
            case validate_proj_config(deliv_git:file_at_sha(ProjDir, ToCommit, ?PROJECT_CONFIG_FILE)) of
                {error, _Reason} ->
                    Msg = <<"Could not create new patchset - invalid project config">>,
                    remove_target_branch(process_with_pipeline, ProjDir, RawBranchName),
                    deliv_web_utils:error_response(400, invalid_config, Msg, Req, State);
                {ok, _Config} ->
                    handle_new_patchset(deliv_patchset:new(EntName,
                                                           UserName, OrgName, ProjName, PipeName,
                                                           FeatBranchName, ToCommit), ProjDir, PipeName,
                                        FeatBranchName, RawBranchName, Coords, Req, State)
            end
    end;
process_with_pipeline({error, not_found}, _, PipeName, _, _, _, _, Req, State) ->
    %% Return plaintext to be displayed in-line in the CLI
    Msg = <<"Could not create new patchset - pipeline \'",
            PipeName/binary, "\' does not exist">>,
    response(Msg, Req, State).

%% @private
-spec validate_proj_config({ok, binary()} | {error, term()}) -> {error, term()} | ok.
validate_proj_config({ok, Config}) -> deliv_proj_config:validate(Config);
validate_proj_config({error, _} = Error) -> Error.

%% @doc Our client-side CLI pushes to _for/<pipe_name>/<feature_branch_name>
%% This function extracts the pipeline and feature branch names
-spec extract_pipe_and_feat_branch(binary()) ->
        {PipeName :: binary(), FeatBranchName :: binary()}
        | not_found.
%% This makes sure we only go further if the refence being pushed is a
%% branch (as opposed to a tag), and with the right prefix "_for"
extract_pipe_and_feat_branch(<<"refs/heads/_for/",
                               RawBranchName/binary >>) ->
    case binary:split(RawBranchName, <<"/">>) of
        [PipeName, FeatBranchName] -> {PipeName, FeatBranchName};
        _ -> not_found
    end;
extract_pipe_and_feat_branch(_) ->
    not_found.

handle_new_patchset([Patchset], ProjDir, PipeName, FeatBranchName, RawBranchName,
                    #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName} = Coords,
                    Req, State) ->
    %% we move the target branch to the review one
    {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
    ScmModule = chef_utils:to_atom(deliv_project:getval(scm_module, Project)),
    {ok, Msg} = ScmModule:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),
    deliv_event:publish(patchset_created, Patchset),

    response(Msg, Req, State);
handle_new_patchset({error, Why}, ProjDir, PipeName, FeatBranchName, RawBranchName,
                    #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
                    Req, State) ->
    case deliv_pg:translate_error(Why) of
        new_patchset_identical_to_old_one ->
            %% the thing being pushed already is the latest patchset
            %% we just remove the target branch, and inform the user
            {ok, Patchset} = deliv_patchset:fetch(EntName, OrgName, ProjName, PipeName,
                                                  FeatBranchName, 1),
            ChangeID = deliv_patchset:getval(change_id, Patchset),
            remove_target_branch(handle_new_patchset, ProjDir, RawBranchName),
            URL = deliv_web_utils:make_web_url_for_change(EntName, OrgName,
                                                          ProjName, ChangeID),
            Msg = <<"Patchset already up to date, nothing to do\n",
                    URL/binary>>,
            response(Msg, Req, State);
        {not_found, WhatAtom} ->
            WhatBin = chef_utils:to_bin(WhatAtom),
            deliv_web_utils:error_response(404, <<WhatBin/binary, " not found">>, Req, State);
        Other ->
            chef_log:error("Unexpected result from deliv_patchset:new : ~p", [Other]),
            deliv_web_utils:error_response(500, internal_error, Req, State)
    end.

remove_target_branch(Caller, ProjDir, RawBranchName) ->
    Cmd = ["branch", "-D", RawBranchName],
    {ok, _Output} = deliv_git:run_git(Caller, ProjDir, Cmd).

jesse_spec() ->
    chef_json:simple_string_dict_spec([<<"enterprise">>,
                                       <<"user">>,
                                       <<"organization">>,
                                       <<"project">>,
                                       <<"reference_name">>,
                                       <<"to_commit">>]).

%% @private
%% @doc The ent/org/proj/user names are encoded in the git repo, we need to
%% decode them first
-spec decode_from_json(binary(), json()) -> binary().
decode_from_json(Field, Json) ->
    deliv_encode:decode(ej:get([Field], Json)).

%% @private
%% @doc Returns the response with `Msg' as body
response(Msg, Req, State) ->
    NewReq = cowboy_req:set_resp_body(Msg, Req),
    {true, NewReq, State}.
