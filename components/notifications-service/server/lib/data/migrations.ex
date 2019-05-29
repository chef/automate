defmodule Notifications.Data.Migrator.Migrations do
  @moduledoc "Contains migration records to apply"

  @doc """
  returns the complete list of migrations to be applied.
  Data.Migrator manages tracking which ones to apply/have already
  been applied
  """

  def all() do
    [%{
       description: "Migration tracking setup",
       queries: [
         "CREATE TABLE migrations (num INTEGER NOT NULL, descr TEXT, at TIMESTAMPTZ NOT NULL);"
       ]},
     %{description: "Create notifications data types and relations",
       queries: [
         """
         CREATE TYPE rule_event AS ENUM ('CCRSuccess',
                                         'CCRFailure',
                                         'ComplianceSuccess',
                                         'ComplianceFailure');
         """,
         # This matches the possible oneof field names and data types of the Notifications.Rule.action field
         # which simplifies setting the correct value when loading from db
         "CREATE TYPE rule_action AS ENUM ('SlackAlert', 'WebhookAlert');",
         """
           CREATE TABLE rules (
                id uuid PRIMARY KEY,
              name TEXT UNIQUE NOT NULL,
             event rule_event NOT NULL,
            action rule_action NOT NULL,
               url TEXT NOT NULL );
         """
       ]},
     %{description: "Enable deduplication of received events",
       queries: [processed_event_table_sql(), log_and_clean_event_sql()]
     },
     %{description: "Add ServiceNowAlert as a rule_action",
       queries: ["ALTER TYPE rule_action ADD VALUE 'ServiceNowAlert' AFTER 'WebhookAlert';"]
     },
     %{description: "Add secret ID to rules table",
       queries: [
        "ALTER TABLE rules ADD COLUMN secret_id VARCHAR;"]
     },
     #
     # Sample Migration to add a new supported rule_event type:
     #
     #     %{description: "Add support for new FooHappened event",
     #       queries: ["ALTER TYPE rule_event ADD VALUE 'FooHappened'"]}
     #
     %{description: "Add support for Assets event",
       queries: ["ALTER TYPE rule_event ADD VALUE 'Assets'"]
     },
     %{description: "Add critical_controls_only to rules table",
       queries: [
        "ALTER TABLE rules ADD COLUMN critical_controls_only BOOLEAN;",
        "ALTER TABLE rules ALTER COLUMN critical_controls_only SET DEFAULT FALSE;"]
     },
     %{description: "UPDATE rules SET critical_controls_only=FALSE",
       queries: [
        "UPDATE rules SET critical_controls_only=FALSE;"]
     },
    ]
  end

  defp processed_event_table_sql() do
    # NOTE: unlogged tables bypass the WAL; this gives better performance but
    # no resiliency; and the table is truncated with a database restart.
    # This is acceptable for our intended usgae as a short-lived record of recently
    # processed notification events.
    # TODO - standardizing on representation of request ids across
    # the platform would let us make make inbound_id more accurately typed.
     """
     CREATE UNLOGGED TABLE processed_events (
         inbound_id VARCHAR(64),
         event rule_event,
         at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         PRIMARY KEY (inbound_id, event)
       );
    """
  end

  defp log_and_clean_event_sql() do
    """
      CREATE OR REPLACE FUNCTION log_and_clean_event(
        id varchar(64),
        event_type rule_event,
        delete_older_than SMALLINT -- purge records older than this # of seconds
      )
      RETURNS BOOLEAN -- True if the event exists
      LANGUAGE plpgsql
    AS $$
      DECLARE
        already_processed BOOLEAN;
    BEGIN
        already_processed = false;
        -- First clean up old events:
        DELETE FROM processed_events
         WHERE at < (CURRENT_TIMESTAMP - (delete_older_than * interval '1 second'));

        -- Now try to insert - failure due to duplicate insert means we already
        -- processed the event.
        BEGIN
          INSERT INTO processed_events(inbound_id, event, at)
               VALUES (id, event_type, CURRENT_TIMESTAMP);
        EXCEPTION WHEN unique_violation THEN
          already_processed = true;
        END;
        RETURN already_processed;
      END;
      $$;
    """
  end
end
