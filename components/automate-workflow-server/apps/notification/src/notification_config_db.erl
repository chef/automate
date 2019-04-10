-module(notification_config_db).

-include("notification_types.hrl").

-compile({parse_transform, sqerl_gobot}).

-export([delete/1,
         delete/2,
         delete/3,
         fetch/1,
         fetch/2,
         fetch/3,
         fetch_enabled_config_by_type/2,
         upsert/1]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

%% Internal record only used for inserting notification configurations into the
%% database. JSON fields (e.g., settings) must be encoded as binary.
-record(notification_config_db, {id :: db_id(),
                                 notification_type :: binary(),
                                 name :: binary(),
                                 settings :: binary(),
                                 enabled :: boolean(),
                                 organization_id :: db_id() | undefined,
                                 enterprise_id :: db_id() | undefined,
                                 project_id :: db_id() | undefined}).

'#insert_fields'() -> [notification_type, name, settings,
                       enabled, organization_id, enterprise_id].
'#update_fields'() -> [name, settings, enabled].

'#table_name'() -> "notification_config".

'#statements'() ->
    [default,
     {fetch_by_enterprise,
      <<"SELECT * FROM notification_config n
                  JOIN enterprises e
                    ON n.enterprise_id = e.id
                 WHERE e.name = $1">>},
     {fetch_by_organization,
      <<"SELECT * FROM notification_config n
                  JOIN organizations o
                    ON n.organization_id = o.id
                  JOIN enterprises e
                    ON o.enterprise_id = e.id
                 WHERE e.name = $1
                   AND o.name = $2">>},
     {fetch_by_project,
      <<"SELECT * FROM notification_config n
                  JOIN projects p
                    ON n.project_id = p.id
                  JOIN organizations o
                    ON p.organization_id = o.id
                  JOIN enterprises e
                    ON o.enterprise_id = e.id
                 WHERE e.name = $1
                   AND o.name = $2
                   AND p.name = $3">>},
    {fetch_enabled_by_type,
     <<"SELECT n.*
          FROM notification_config n
          JOIN ( SELECT p.id as proj_id, o.id as org_id, e.id as ent_id
                   FROM projects p
                   JOIN organizations o
                     ON p.organization_id = o.id
                   JOIN enterprises e
                     ON o.enterprise_id = e.id
                  WHERE e.name = $2
                    AND o.name = $3
                    AND p.name = $4
               ) scope
            ON CASE
                    WHEN n.project_id IS NOT NULL
                         THEN scope.proj_id = n.project_id
                    WHEN n.organization_id IS NOT NULL
                         THEN scope.org_id = n.organization_id
                    WHEN n.enterprise_id IS NOT NULL
                         THEN scope.ent_id = n.enterprise_id
                END
         WHERE n.notification_type = $1
           AND n.enabled = true">>},
    {upsert,
    <<"SELECT *
         FROM upsert_notification_config($1, $2, $3, $4, $5, $6, $7)">>},
     {delete_by_org_id,
         <<"DELETE FROM notification_config
            WHERE organization_id = $1">>},
     {delete_by_ent_id,
          <<"DELETE FROM notification_config
             WHERE enterprise_id = $1">>},
     {delete_by_proj,
          <<"DELETE FROM notification_config
             WHERE project_id in (
                SELECT n.project_id
                  FROM notification_config n
                  JOIN projects p
                    ON n.project_id = p.id
                  JOIN organizations o
                    ON p.organization_id = o.id
                  JOIN enterprises e
                    ON o.enterprise_id = e.id
                 WHERE e.name = $1
                   AND o.name = $2
                   AND p.name = $3)">>}
    ].

-spec upsert(#notification_config{}) -> db_op_result(any()).
upsert(#notification_config{notification_type = NotificationType,
                          name = Name,
                          settings = Settings,
                          enabled = Enabled,
                          organization_id = OrgId,
                          enterprise_id = EntId,
                          project_id = ProjId}) ->
    deliv_db:qfetch(?MODULE,
                    upsert,
                    [NotificationType, Name, chef_json:encode(Settings),
                     Enabled, OrgId, EntId, ProjId]).

%% @doc Fetches by enterprise Name
% For looking up by enterprise id, which is for email at this point.
-spec fetch(binary()) -> [#notification_config{}] | {error, term()}.
fetch(EntName) ->
    deserialize(deliv_db:qfetch(?MODULE, fetch_by_enterprise, [EntName])).

%% @doc Fetches by enterprise name & name
% For looking up by org id, which is webhooks at this point
-spec fetch(binary(), binary()) -> [#notification_config{}] | {error, term()}.
fetch(EntName, OrgName) ->
    deserialize(deliv_db:qfetch(?MODULE, fetch_by_organization, [EntName, OrgName])).

fetch(EntName, OrgName, ProjName) ->
    deserialize(deliv_db:qfetch(?MODULE, fetch_by_project, [EntName, OrgName, ProjName])).

%% @doc Fetches all enabled configs matching any of the scoping parameters
%% by notification type.
-spec fetch_enabled_config_by_type(atom(), d_change()) -> [#notification_config{}] | {error, term()}.
fetch_enabled_config_by_type(Type, Change) ->
    ChangeId = deliv_change:getval(id, Change),
    [EntName, OrgName, ProjName | _] = deliv_change:scoping_names(ChangeId),
    deserialize(
        deliv_db:qfetch(?MODULE,
                        fetch_enabled_by_type,
                        [Type, EntName, OrgName, ProjName])
    ).

%% @doc Deletes configuration by enterprise id
%% used for smtp configurations
-spec delete(binary()) -> ok | {error, any()}.
delete(EntName) ->
    case deliv_enterprise:fetch(EntName) of
        {error, not_found} ->
            {error, ent_not_found};
        {ok, Ent} ->
            EntId = deliv_enterprise:getval(id, Ent),
            handle_delete(sqerl_rec:cquery(?MODULE, delete_by_ent_id, [EntId]), EntName, enterprise)
    end.

%% @doc Deletes configuration by organization id
%% used for slack webhook configurations
-spec delete(binary(), binary()) -> ok | {error, any()}.
delete(EntName, OrgName) ->
    case deliv_organization:fetch(EntName, OrgName) of
        {error, not_found} ->
            {error, org_not_found};
        {ok, Org} ->
            OrgId = deliv_organization:getval(id, Org),
            handle_delete(sqerl_rec:cquery(?MODULE, delete_by_org_id, [OrgId]), OrgName, organization)
    end.

delete(EntName, OrgName, ProjName) ->
    handle_delete(
        sqerl_rec:cquery(?MODULE, delete_by_proj, [EntName, OrgName, ProjName]),
        ProjName,
        project
    ).

% private
log_delete({ok, 0}, ScopedName, NotificationScope) ->
    chef_log:debug("Could not delete notification configuration for ~p ~p: not found", [NotificationScope, ScopedName]);
log_delete({ok, 1}, ScopedName, NotificationScope) ->
    chef_log:info("Deleted notification configuration for ~p ~p", [NotificationScope, ScopedName]);
log_delete({error, Why}, ScopedName, _NotificationScope) ->
    chef_log:failed_call(?MODULE, delete, [ScopedName], Why).

% private
handle_delete({ok, _Rows} = Deleted, ScopedName, NotificationScope) ->
    log_delete(Deleted, ScopedName, NotificationScope),
    ok;
handle_delete({error, _Why} = Error, ScopedName, NotificationScope) ->
    log_delete(Error, ScopedName, NotificationScope),
    Error.

%% private
deserialize({error, _Why} = Error) ->
    Error;
deserialize(Configs) ->
    lists:map(
        fun(Config) ->
            deserialize_config(Config)
        end,
        Configs
    ).

deserialize_config(#notification_config_db{notification_type = NotificationType,
                                           name = Name,
                                           settings =  Settings,
                                           enabled = Enabled,
                                           organization_id = OrgId,
                                           enterprise_id = EntId,
                                           project_id = ProjId}) ->

    %% TODO Consider validation of the NotificationType we get from the db:
    %% ensure it matches one of the atoms in notification_type().
    #notification_config{notification_type = chef_utils:to_atom(NotificationType),
                         name = Name,
                         settings = chef_json:decode(Settings),
                         enabled = Enabled,
                         organization_id = OrgId,
                         enterprise_id = EntId,
                         project_id = ProjId}.
