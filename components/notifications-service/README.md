# Notifications

Provides alerting for inbound notifications according to customer-defined rules.

## Terminology

* *Notification* - a thing emitted by the A2 system and received by the notifications
                 service that indicates something has happened.
* *Rule* - defined by the customer, rules indicate what types of notifications we care about,
           what actions to take in response to their arrival, and include the necessary
           information to perform those actions (such as endpoint of a URL)
* *Target* - part of a rule. The target is the location where an alert is sent.
* *Action* - a capability offered by the Notifications Service that can be attached to a rule.
             Currently "Action" is not implemented in code - there is not yet a need to,
             given that there is only one support action of *alert*.
* *Alert* - a type of action that indicates sending an outbound payload somewhere (currently
            slack and/or custom webhook). Requires a URL to post to; and posting respects
            the usual PROXY_* env vars.

## Development

The development is done in hab studio. Look in the .studio/notifications-service 
file for studio commands for development. Below are the steps to start development.

* From the A2 project root run `hab studio enter`
* `build components/notifications-service`
* `start_all_services`
* From here you can use the notifications UI page and send failed chef runs with `send_chef_run_failure_example`
* After to run modifications to the notifications-service component files run `notifications_update_component`
* To rebuild changes to the protobuf files run `notifications_compile_protobuf` then `notifications_update_component` to add the changes to thr running service


### Helper modules

This project includes an IEX customization file, .iex.exs It will load automatically, and
when you start `iex -S mix` it will show you the capabilities it added to your IEX session


### Unit tests

To run the unit test, from inside of hab studio, run `notifications_unit_test`

### Adding new notification types
1. After updating priv/notifications.proto, regenerate the
   grpc interface by running `notifications_compile_protobuf`.
2. Add the new type to apps/common/lib/notifications.ex 'notification'
   spec. This ensures that dialyzer will correctly identify the new type as
   a notification.
3. Add formatting handling for the new type to Notifications.Formatters.Slack,
   Notifications.Formatters.Webhook, and Notifications.Formatters.ServiceNow.
4. Create a sql migration in data/migrations.ex to recreate the rule_event enum.
   DO NOT modify the original migration that created the enum type - it will
   not get executed an systems that have already run that migration.  Instead, use
  `ALTER TYPE` as follows:
      ```
      ALTER TYPE rule_event ADD 'NewInboundNotificationTypeName'
      ```

### Enabling excluded types
The module Prefilter has a list of excluded notification types. When
support is enabled for those types, they must be removed from

### Adding database schema changes and migrations
In `lib/data/migrations.ex#all/0` add a new entry to the end of the list in the
following form:

```
[
...
%{description: "A brief description of the migration or change",
  queries: [ ... a list of SQL queries (DDL, data conversion, etc) ..
 }
...


```

Migrations are run automatically on notifications service startup, before it begins listening
for GRPC requests.  A list of migrations that have been run and when can be found in
the table migrations in db notifications_service.

Migrations are run within transactions, and are all-or-nothing. If any step of a given migration
fails, the changes to that point are not committed, and notifications
service will fail to start.

