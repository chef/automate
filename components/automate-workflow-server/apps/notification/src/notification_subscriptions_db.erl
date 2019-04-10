-module(notification_subscriptions_db).

-include_lib("delivery/include/deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

-export([
         upsert/5,
         fetch/4,
         fetch_emails_by_category/4
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-record(notification_subscriptions_db, {id :: db_id(),
                                        project_id :: db_id(),
                                        user_id :: db_id(),
                                        categories :: [binary()]}).

'#insert_fields'() -> [project_id, user_id, categories].

'#update_fields'() -> [].

'#table_name'() -> "notification_subscriptions".

'#statements'() ->
    [default,
     {upsert, "SELECT *
                 FROM upsert_notification_subscription($1, $2, $3, $4, $5)"},

     {fetch, "SELECT ns.project_id, ns.user_id, ns.categories
                FROM enterprises e
                JOIN organizations o
                  ON e.id = o.enterprise_id
                JOIN projects p
                  ON o.id = p.organization_id
                JOIN notification_subscriptions ns
                  ON p.id = ns.project_id
                JOIN users u
                  ON u.id = ns.user_id
               WHERE e.name = $1
                 AND o.name = $2
                 AND p.name = $3
                 AND u.name = $4"},

     {fetch_emails_by_category,
                            <<"SELECT u.email
                                 FROM enterprises e
                                 JOIN organizations o
                                   ON e.id = o.enterprise_id
                                 JOIN projects p
                                   ON o.id = p.organization_id
                                 JOIN notification_subscriptions ns
                                   ON p.id = ns.project_id
                                 JOIN users u
                                   ON u.id = ns.user_id
                                WHERE e.name = $1
                                  AND o.name = $2
                                  AND p.name = $3
                                  AND $4 = ANY(ns.categories)
                                  AND u.email IS NOT NULL
                                  AND u.email <> ''">>}].

-spec upsert(binary(), binary(), binary(), binary(), [binary()]) -> db_op_result() | {error, any()}.
upsert(EntName, OrgName, ProjName, UserName, Subscription) ->
    deliv_db:qfetch(?MODULE, upsert, [EntName, OrgName, ProjName, UserName, Subscription]).

-spec fetch(binary(), binary(), binary(), binary()) -> {ok, [Row :: list() | tuple()]} | {error, term()}.
fetch(EntName, OrgName, ProjName, UserName) ->
    deliv_db:select(?MODULE, fetch, [EntName, OrgName, ProjName, UserName]).

-spec fetch_emails_by_category(binary(), binary(), binary(), binary()) -> [binary()] | {error, any()}.
fetch_emails_by_category(EntName, OrgName, ProjName, Category) ->
     case sqerl_rec:scalar_fetch(?MODULE, fetch_emails_by_category, [EntName, OrgName, ProjName, Category]) of
            {error, Why} = Error ->
                 chef_log:failed_call(?MODULE, fetch_emails_by_category, [EntName, OrgName, ProjName, Category], Why),
                 Error;
            EmailList ->
                EmailList
    end.
