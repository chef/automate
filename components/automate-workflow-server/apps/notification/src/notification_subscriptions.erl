-module(notification_subscriptions).

-include("notification_types.hrl").

-export([
    subscribe/5,
    subscriber_emails_by_event/4,
    subscription/4
]).

%% Subscribes a user to notifications from a project.
-spec subscribe(binary(), binary(), binary(), binary(), [binary()]) -> ok | {error, term()}.
subscribe(EntName, OrgName, ProjName, UserName, Subscription) ->
    handle_subscribe(
        notification_subscriptions_db:upsert(EntName, OrgName, ProjName, UserName, Subscription)
    ).

%% Returns true if the user is subscribed to notifications for a project.
%% Returns false otherwise.
-spec subscription(binary(), binary(), binary(), binary()) -> [binary()] | {error, term()}.
subscription(EntName, OrgName, ProjName, UserName) ->
    handle_subscription(
        notification_subscriptions_db:fetch(EntName, OrgName, ProjName, UserName)
    ).

%% Returns a list of email addresses of users subscribed to the event's category
%% for the given project.
-spec subscriber_emails_by_event(binary(), binary(), binary(), atom()) -> [binary()] | {error, term()}.
subscriber_emails_by_event(EntName, OrgName, ProjName, Event) ->
    case event_category(Event) of
        undefined ->
            chef_log:error("Failed to retrieve subscribers for unhandled event ~s in ent ~s, org ~s, project ~s",
                             [Event, EntName, OrgName, ProjName]),
            {error, unhandled_event};
        Category ->
            handle_subscriber_emails(
                notification_subscriptions_db:fetch_emails_by_category(EntName, OrgName, ProjName, Category)
            )
    end.

%% PRIVATE

event_category(Event) ->
    Categories = [
                {verify_passed, <<"review">>},
                {change_approved, <<"review">>},
                {comment_created, <<"review">>},
                {union_failed, <<"observe">>},
                {rehearsal_failed, <<"observe">>},
                {delivered_failed, <<"observe">>},
                {delivered_passed, <<"observe">>},
                {acceptance_failed, <<"observe">>},
                {acceptance_passed, <<"deliver">>},
                {change_delivered, <<"deliver">>},
                {build_failed, <<"observe">>}
             ],
    proplists:get_value(Event, Categories).

handle_subscribe({error,{<<"23502">>, _}}) ->
    %% This is an error returned by pgsql when there is a not null violation
    %% For example if a null project_id or user_id was inserted
    {error, cannot_subscribe};
handle_subscribe({error, _} = Error) ->
    Error;
handle_subscribe(_Subscription) ->
    ok.

handle_subscription({ok, []}) ->
    [];
handle_subscription({ok, [Subscription]}) ->
    proplists:get_value(<<"categories">>, Subscription);
handle_subscription({error, _Why} = Error) ->
    Error.

handle_subscriber_emails({error, _Why} = Error) ->
    Error;
handle_subscriber_emails(Result) ->
    Result.
