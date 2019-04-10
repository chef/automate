defmodule Notifications.Data.A1MigratorTest do
  use ExUnit.Case, async: false
  import Mock

  test "migration fixture adds expected rules" do
    Temp.track!
    tmpdir = Temp.mkdir!
    rule_store_file = Path.join(tmpdir, "rule_store")
    migration_file = Path.join(tmpdir, "a1_migration_status")

    File.copy!(
      Path.expand("../test-data/rule_store", __DIR__),
      rule_store_file
    )


    with_mocks([
      {
        Notifications.Config, [],
        [
          rule_store_for_migration: fn() -> rule_store_file end,
          migration_status_file: fn() -> migration_file end
        ]
      },
      {
        Notifications.Data.Store, [], [
          add_rule: fn(rule) -> 
            case rule do
              %Notifications.Rule{action: {:SlackAlert, %Notifications.SlackAlert{url: "https://hooks.slack.com/services/FAKE/FAKE/THISISFAKE"}}, event: 0, name: "ccr-slack"} ->
                {:ok, "abc123"}
              %Notifications.Rule{action: {:WebhookAlert, %Notifications.WebhookAlert{url: "https://example.com"}}, event: 0, name: "ccr-webhook"} ->
                {:ok, "abc123"}
              %Notifications.Rule{action: {:SlackAlert, %Notifications.SlackAlert{url: "https://hooks.slack.com/services/FAKE/FAKE/THISISFAKE"}}, event: 2, name: "compliance-slack"} ->
                {:ok, "abc123"}
              %Notifications.Rule{action: {:WebhookAlert, %Notifications.WebhookAlert{url: "https://example.com"}}, event: 2, name: "compliance-webhook"} ->
                {:ok, "abc123"}
            end
          end
        ]
      }
    ]) do
      :success = Notifications.Data.A1Migrator.run()
      true = File.exists?(Notifications.Config.migration_status_file())
    end
  end
end

