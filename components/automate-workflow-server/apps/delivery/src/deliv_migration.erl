-module(deliv_migration).

-include("deliv_types.hrl").

-export([add_change_description/0,
         add_change_description_dry_run/0,
         update_patchset_diffs/0,
         update_patchset_diffs_dry_run/0
        ]).
%% This migration iterates through all non-withrdawn and changes and applies
%% the title and description generation logic to generate the title and
%% description for the deliv_change object when those values do not yet
%% exist.
%%
%% We intentionally do no error checking, since this should work, and if not,
%% we prefer it blows up to find out why.
%%
%% We intentionally duplicate already existing code instead of refactoring, as
%% it is likely this code will be short-lived, and is not core to Delivery.
%% (See TODO below). As such, the code is not as beautiful as it could be.
%%
%% WARNING: This is a ONE WAY operation. There is no way to undo this operation
%% other than recovering from a backup, so make a backup. You have been warned.
%% This iteration has gone under minimal testing and as of this writing will
%% only be used to perform two specific migrations. If you find the need for
%% this functionality, or similar, in the future, you should carefully review
%% the code to make sure assumptions made are still valid.
%%
%% TODO: After the above mentioned migrations are done, it is very likely this
%% code should be removed, as its purpose will have been served and it will only
%% likely become stale. Make sure to remove all relevant parts in ctl.
add_change_description() ->
    EntNames = add_change_description_common(),
    lists:map(fun(X) -> all_changes_for_ent(X, live) end, EntNames).

%% This performs the same checks and operations as above, but *DOES NOT* perform
%% any database write operations. This should be a low risk function to run
%% before the real migration as a way to verify it will go smoothly. Keep in
%% mind reading from the database *CAN* have side effects, due to cacheing and
%% other assumptions we may be missing. You *SHOULD* always run a backup, even
%% before doing a dry run.
add_change_description_dry_run() ->
    EntNames = add_change_description_common(),
    lists:map(fun(X) -> all_changes_for_ent(X, dry) end, EntNames).

%% Common init code for above live and dry runs
add_change_description_common() ->
    Ents = deliv_enterprise:list_all(),
    lists:map(fun(X) -> deliv_enterprise:getval(name, X) end, Ents).

%%
%%  add_description/0 helpers
%%
all_changes_for_ent(EntName, Mode) ->
    Orgs = sqerl_rec:qfetch(deliv_organization, fetch_for_ent, [EntName]),
    OrgNames = lists:map(fun(X) ->
                                 deliv_organization:getval(name, X) end, Orgs),
    lists:map(fun(X) -> all_changes_for_ent_org(EntName, X, Mode) end, OrgNames).

all_changes_for_ent_org(EntName, OrgName, Mode) ->
    ProjNames = deliv_project:fetch_names(EntName, OrgName),
    lists:map(fun(X) ->
                      all_changes_for_ent_org_proj(EntName, OrgName, X, Mode) end,
              ProjNames).

all_changes_for_ent_org_proj(EntName, OrgName, ProjName, Mode) ->
    %% The below call sets a limit of 2147483647, since there is no way to pass
    %% a value to say unlimited, since the get_changes stored procedure will
    %% default to only a limit of 10 if nothing is specified. The limit is
    %% an integer, so we request the maximum integer value allowed in postgres.
    %% This is better than worrying about rewriting a stored procedure or
    %% another likely as ugly hack to get all the changes.
    {ok, Changes} = deliv_project:changes(EntName, OrgName, ProjName,
                                          <<"this_is_always_ignore_and_should_go_away">>,
                                          [{state, all}, {limit, 2147483647}]),
    %% filter out withdrawn commits
    Changes2 = lists:filter(fun(X) ->
                                    <<"withdrawn">> =/= proplists:get_value(<<"status">>, X)
                            end, Changes),
    lists:foreach(fun(X) -> update_change(EntName, OrgName, ProjName, X, Mode) end, Changes2).

%% This is the main function for updating the "Change". It should be noted, that
%% above call to deliv_project:changes/5 doesn't actually return a proper list
%% of changes, instead it returns proplist objects in the following form:
%%
%% [[{<<"id">>,
%%    <<"cfdfba49-d520-4e1c-8fca-f075d151d24d">>},
%%   {<<"feature_branch">>,<<"feature1">>},
%%   {<<"pipeline">>,<<"master">>},
%%   {<<"status">>,<<"open">>},
%%   {<<"submitted_at">>,{{2015,1,14},{21,14,1.89239}}},
%%   {<<"submitted_by">>,<<"admin">>},
%%   {<<"merge_sha">>,null}],
%%  ...]
%%
%% Given this, we cannot use the regular deliv_change:getval/2 function for
%% accessing data, instead, we use proplists:get_value/2.
update_change(EntName, OrgName, ProjName, Change, Mode) ->
    %% Get the git commit message, if the change has been merged,
    %% we will use the merge_sha, otherwise, we will get the
    %% commit information from the active patchset.

    ChangeId = proplists:get_value(<<"id">>, Change),

    Commit = case proplists:get_value(<<"merge_sha">>, Change) of
                 null ->
                     %% get commits from patchset
                     PipeName = proplists:get_value(<<"pipeline">>, Change),
                     FeatureBranch = proplists:get_value(feature_branch,
                                                         Change),
                     {ok, Patchset} =
                         deliv_patchset:latest_patchset_for_change(ChangeId),

                     %% Left for future debugging, if needed
                     %% io:format("Patchset: ~p~n", [Patchset]),

                     case deliv_patchset:getval(verified_against_sha, Patchset) of
                         %% this fixes another empty commit issue
                         undefined ->
                             [];
                         Patchset ->
                             case deliv_git:patchset_commit_log(EntName, OrgName,
                                                                ProjName, ChangeId,
                                                                PipeName, FeatureBranch,
                                                                Patchset) of
                                 {ok, []} ->
                                     [];
                                 {ok, Commits} ->
                                     %% use first commit's subject and body
                                     erlang:hd(Commits)
                             end
                     end;
                 MergeSha ->
                     {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
                     RepoPath = deliv_project:repo_path(Project),
                     %% get commit using MergeSha
                     case deliv_git:log(RepoPath, <<MergeSha/binary, "~1">>, MergeSha) of
                         {ok, [PatchsetCommit]} ->
                             PatchsetCommit;
                         {ok, PatchsetCommits} ->
                             hd(lists:reverse(PatchsetCommits))
                     end
             end,

    %% Left for future debugging, if needed
    %% io:format("Commit: ~p~n", [Commit]),

    case Commit of
        %% for weird changes that have no commits
        [] ->
            ok;
        Commit ->
            Title = deliv_patchset_commit:getval(subject, Commit),
            Description = deliv_patchset_commit:getval(body, Commit),

            %% We need to get the actual full change object to update it
            {ok, FullChange} = deliv_change:fetch_by_id(ChangeId),

            io:format("**********************\n"
                      "* Original FullChange: ~n~p~n"
                      "**********************\n", [FullChange]),

            NewFullChange = set_if_new(title, Title, FullChange),
            NewFullChange2 = set_if_new(description, Description, NewFullChange),

            io:format("#################\n"
                      "# New FullChange: ~n~p~n"
                      "#################\n", [NewFullChange2]),

            case Mode of
                live ->
                    {ok, NewFullChange2} = deliv_change:update(NewFullChange2),
                    io:format("LIVE RUN: Change update succesful!~n");
                dry ->
                    io:format("DRY RUN: Not committing update to database!~n")
            end,

            ok
    end.

%% @private
%% @doc Sets the given field in change if it has not yet been set (undefined).
-spec set_if_new(atom(), any(), d_change()) -> d_change().
set_if_new(Field, Value, Change) ->
    case deliv_change:getval(Field, Change) of
        undefined ->
            io:format("Will set field ~p to: ~p\n", [Field, Value]),
            deliv_change:setvals([{Field, Value}], Change);
        ExistingValue->
            io:format("Will NOT update field ~p, it is already defined as: ~p\n",
                      [Field, ExistingValue]),
            Change
    end.

update_patchset_diffs() ->
  Patchsets = deliv_patchset:list_all(),
  walk_patchsets(Patchsets, live, 0,0,0,0,0).
update_patchset_diffs_dry_run() ->
  Patchsets = deliv_patchset:list_all(),
  walk_patchsets(Patchsets, dry_run, 0,0,0,0,0).


walk_patchsets([], _, Errors, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed) ->
  io:format("
            Empty Patchsets:~p
            Error Patchset:~p
            Diffs Found:~p
            Diffs Updated:~p
            Diff Updates Failed:~p
            ", [Empties, Errors, DiffsFound, DiffsUpdated, DiffUpdatesFailed]),
  done;
walk_patchsets({error, _}, Mode, Errors, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed) ->
  walk_patchsets([], Mode, Errors, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed);
walk_patchsets([Patchset | Rest], Mode, Errors, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed) ->
  ChangeId = deliv_patchset:getval(change_id, Patchset),
  case length(Rest) rem 100 of
    0 ->
      io:format("Patchsets Remaining:~p~n", [length(Rest)]);
    _ ->
      continue
  end,
  {ok, Change} = deliv_change:fetch_by_id(ChangeId),
  FeatureBranch = deliv_change:getval(feature_branch, Change),
  [EntName, OrgName, ProjName, PipeName] = deliv_change:scoping_names(ChangeId),
  {UpdatedErrors,
   UpdatedEmpties,
   UpdatedDiffsFound,
   UpdatedDiffsUpdated,
   UpdatedDiffUpdatesFailed} = update_patchset(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset, Errors, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed, Mode),
  walk_patchsets(Rest, Mode, UpdatedErrors, UpdatedEmpties, UpdatedDiffsFound, UpdatedDiffsUpdated, UpdatedDiffUpdatesFailed).



update_patchset(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset, Errors, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed, live) ->
  try deliv_git:patchset_changed_files(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) of
    {ok, []} ->
      {Errors, Empties + 1, DiffsFound, DiffsUpdated, DiffUpdatesFailed};
    {ok, Diffs} ->
      {Us, Fs} = lists:foldl(fun(Diff, {Updates, Fails}) ->
                      case deliv_patchset_changed_file:update_by_patchset_id_file(Diff) of
                        {ok, _} ->
                          {Updates + 1, Fails};
                        {error, _} ->
                          {Updates, Fails +1}
                      end
                  end,
                  {0,0},
                  Diffs),
      {Errors, Empties, DiffsFound + length(Diffs), DiffsUpdated + Us, DiffUpdatesFailed + Fs}
  catch
    _Error:_Why ->
      {Errors + 1, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed}
  end;
update_patchset(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset, Errors, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed, dry_run) ->
  try deliv_git:patchset_changed_files(EntName, OrgName, ProjName, ChangeId, PipeName, FeatureBranch, Patchset) of
    {ok, []} ->
      {Errors, Empties + 1, DiffsFound, DiffsUpdated, DiffUpdatesFailed};
    {ok, Diffs} ->
      {Errors, Empties, DiffsFound + length(Diffs), DiffsUpdated, DiffUpdatesFailed}
  catch
    _Error:_Why ->
      {Errors + 1, Empties, DiffsFound, DiffsUpdated, DiffUpdatesFailed}
  end.
