-module(notification_web_utils).

-include("notification_types.hrl").

-export([
         scoped_web_url_for_change/5,
         to_json/1
        ]).

%% GitHub integrated projects don't use the Automate Workflow UI. The notification should
%% link to the pull request if the Change corresponds to a GitHub integrated
%% project.
-spec scoped_web_url_for_change(binary(), binary(), binary(), binary(), url_scope()) -> binary().
scoped_web_url_for_change(EntName, OrgName, ProjName, ChangeId, URLScope) ->
    {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
    scoped_web_url_for_change(deliv_project:scm_type(Project), EntName, OrgName,
                              ProjName, ChangeId, URLScope).

-spec to_json(#notification_config{}) -> json().
to_json(#notification_config{notification_type = slack_webhook,
                             name = Name,
                             enabled = Enabled,
                             settings = Settings}) ->
    {[
        {<<"webhook">>,
            {[
                {<<"url">>, ej:get([<<"url">>], Settings)},
                {<<"name">>, Name},
                {<<"enabled">>, Enabled}
            ]}
        }
    ]};
to_json(#notification_config{notification_type = smtp,
                             settings = Settings}) ->
    ej:delete([<<"password">>], Settings).

%% PRIVATE

scoped_web_url_for_change(<<"github">>, _EntName, _OrgName, _ProjName, ChangeId, URLScope) ->
    {ok, Patchset} = deliv_patchset:latest_patchset_for_change(ChangeId),
    PullRequestURL = deliv_github_patchset:pr_url(Patchset),
    case URLScope of
        review ->
            chef_utils:to_bin("~s/~p", [PullRequestURL, files]);
        _ ->
            PullRequestURL
    end;
scoped_web_url_for_change(_SCMType, EntName, OrgName, ProjName, ChangeId, URLScope) ->
    BaseURL = deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId),
    chef_utils:to_bin("~s/~p", [BaseURL, URLScope]).
