-include_lib("delivery/include/deliv_types.hrl").

-type notification_type() :: slack_webhook | smtp.

-type url_scope() :: review | status | summary.

-type notification_event() :: acceptance_failed | acceptance_passed | build_failed | change_approved |
                              change_delivered | comment_created | delivered_failed |
                              delivered_passed | rehearsal_failed | union_failed |
                              verify_passed.

-record(notification_config, {notification_type :: notification_type(),
                              name :: binary(),
                              settings :: json(),
                              enabled :: boolean(),
                              organization_id :: db_id(),
                              enterprise_id :: db_id(),
                              project_id :: db_id()}).
