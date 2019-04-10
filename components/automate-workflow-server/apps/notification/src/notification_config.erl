-module(notification_config).

-include("notification_types.hrl").

-export([save/3,
         gen_smtp_options/1,
         with_merged_password/2
        ]).

-type password() :: binary().

%% Create a new notification config and save it.
%% Params:
%%   NotificationType: slack_webhook | smtp
%%   ScopingNames:
%%     for enterprise-level configs, provide [EntName],
%%     for organization-level configs, provide [EntName, OrgName],
%%     for project-level configs, provide [EntName, OrgName, ProjName]
%%   Settings: provide the appropriate JSON configuring a notification.
%%     This is the JSON that comes back from the UI. See the notification
%%     handlers for examples. It is expected that you provide JSON corresponding
%%     to the specified NotificationType.
-spec save(NotificationType :: notification_type(),
           ScopingNames     :: [binary()],
           Settings         :: json()) ->
    #notification_config{} | {error, ent_not_found  | org_not_found |
                                     proj_not_found | no_password   | term()}.
save(NotificationType, [EntName], Settings) ->
    save(
        config_for_ent(NotificationType, deliv_enterprise:fetch(EntName), Settings)
    );
save(NotificationType, [EntName, OrgName], Settings) ->
    save(
        config_for_org(NotificationType, deliv_organization:fetch(EntName, OrgName),
                       Settings)
    );
save(NotificationType, [EntName, OrgName, ProjName], Settings) ->
    save(
        config_for_proj(NotificationType,
                        deliv_project:fetch(EntName, OrgName, ProjName), Settings)
    ).

%% PRIVATE

save({error, _Why} = Error) ->
    Error;
save(Config) ->
    notification_config_db:upsert(Config).

config_for_ent(_NotificationType, {error, not_found}, _Settings) ->
    {error, ent_not_found};
config_for_ent(smtp, {ok, Ent}, IncomingSettings) ->
    EntName = deliv_enterprise:getval(name, Ent),
    EntId = deliv_enterprise:getval(id, Ent),

    case with_merged_password(EntName, IncomingSettings) of
        {ok, NewSettings} ->
            #notification_config{
                notification_type = smtp,
                settings = NewSettings,
                enabled = true,
                enterprise_id = EntId
            };
        {error, _Why} = Error ->
            Error
    end.

-spec get_password({error, term()} | [#notification_config{}]) ->
    {error, term()} | {ok, password()}.
get_password({error, _Why} = Error) ->
    Error;
get_password([]) ->
    {error, no_password};
get_password([#notification_config{settings = Settings}]) ->
    case ej:get([<<"password">>], Settings) of
        undefined -> {error, no_password};
        Password -> {ok, Password}
    end.

config_for_org(_NotificationType, {error, not_found}, _Settings) ->
    {error, org_not_found};
config_for_org(slack_webhook, {ok, Org}, Settings) ->
    Config = slack_webhook_config(Settings),

    OrgId = deliv_organization:getval(id, Org),
    Config#notification_config{organization_id = OrgId}.

config_for_proj(_NotificationType, {error, not_found}, _Settings) ->
    {error, proj_not_found};
config_for_proj(slack_webhook, {ok, Proj}, Settings) ->
    Config = slack_webhook_config(Settings),

    ProjId = deliv_project:getval(id, Proj),
    Config#notification_config{project_id = ProjId}.

slack_webhook_config(Settings) ->
    Name = ej:get([<<"name">>], Settings),
    URL = ej:get([<<"url">>], Settings),
    Enabled = ej:get([<<"enabled">>], Settings),

    #notification_config{
        notification_type = slack_webhook,
        name = Name,
        settings = {[ {<<"url">>, URL} ]},
        enabled = Enabled
    }.

-spec gen_smtp_options(json()) -> proplists:proplist().
gen_smtp_options(Settings) ->
    Host = ej:get([<<"host">>], Settings),
    Port = ej:get([<<"port">>], Settings),
    Username = ej:get([<<"smtp_login">>], Settings),
    Password = ej:get([<<"password">>], Settings),
    Opts = [{relay, Host},
            {port, Port}],
    case {Username, Password} of
        %% gen_smtp requires BOTH a username and password, so if either of both
        %% is not there, don't do authentication
        {undefined, _} -> Opts;
        {_, undefined} -> Opts;
        {Login, EncodedPassword} ->
            [{username, Login}, {password, base64:decode(EncodedPassword)} | Opts]
    end.

-spec with_merged_password(EntName :: binary(),
                         RequestBody :: json()) ->
    {ok, MergedSettings :: json()} | {error, term()}.
with_merged_password(EntName, IncomingSettings) ->
    merge_password(EntName, IncomingSettings, ej:get([<<"password">>], IncomingSettings)).

merge_password(EntName, NewSettings, undefined) ->
    case get_password(notification_config_db:fetch(EntName)) of
        {error, no_password} ->
            {ok, NewSettings};
        {error, _} = Error ->
            Error;
        {ok, OldPassword} -> %% comes from database, thus is encoded
            {ok, ej:set([<<"password">>], NewSettings, OldPassword)}
    end;
merge_password(_EntName, NewSettings, NewPassword) ->
    {ok, ej:set([<<"password">>], NewSettings, base64:encode(NewPassword))}.
