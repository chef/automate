require Logger
defmodule Notifications.Data.A1Migrator do
  @moduledoc """
  This migrates the rule_store from A1 into the A2 database. When the
  migration completes, a file is written with its status (success | fail | skip)
  """

  def run() do
    status = try do
      if should_run?() do
        responses = read_rules_from_file() |> migrate_rules()
        case num_failed(responses) do
          0 ->
            Logger.info("A1 migration successful")
            :success
          c ->
            Logger.error("A1 Migration failed: Failed to migrate #{to_string(c)} of #{Enum.count(responses)} rules")
            :fail
        end
      else
        Logger.info("A1 migration skipped: complete=#{complete?()}; rule_store_exists=#{rule_store_exists?()}; rule_store_file=#{Notifications.Config.rule_store_for_migration()}")
        :skip
      end
    rescue
      e ->
        Logger.error("A1 Migration failed: #{Exception.format(:error, e)}")
        :fail
    end
    write_status(status)
  end

  defp should_run? do
    rule_store_exists?() && !complete?()
  end

  defp complete? do
    File.exists?(Notifications.Config.migration_status_file())
  end

  defp rule_store_exists? do
    File.exists?(Notifications.Config.rule_store_for_migration())
  end

  defp read_rules_from_file do
    case :dets.open_file(to_charlist(Notifications.Config.rule_store_for_migration()), [{:access, :read}]) do
      {:ok, db} ->
        res = :dets.foldl(fn ({_k, v}, vs) -> [v | vs] end, [], db) 
        :dets.close(db)
        res
      {:error, reason} ->
        raise "Could not open rules file: #{reason}"
    end
  end

  defp migrate_rules(rules) do
    Enum.map(
      rules, 
      fn(rule) ->
        rule |> to_pb_rule |> Notifications.Data.Store.add_rule()
      end
    )
  end

  defp to_pb_rule(rule) do
    case rule do
      %{name: name, target_type: target_type,
        target_url: target_url, event: event} ->
          %Notifications.Rule{
            name: name,
            event: convert_event(event),
            action: to_action(target_type, target_url)
          }
    end
  end

  defp convert_event(event) do
    case event do
      "compliance_failure" ->
        Notifications.Rule.Event.value(:ComplianceFailure)
      "ccr_failure" ->
        Notifications.Rule.Event.value(:CCRFailure)
    end
  end

  defp to_action(target_type, target_url) do
    case target_type do
      :slack -> {:SlackAlert, Notifications.SlackAlert.new(url: target_url)}
      :custom -> {:WebhookAlert, Notifications.WebhookAlert.new(url: target_url)}
    end
  end

  defp write_status(status) do
    File.write!(Notifications.Config.migration_status_file(), to_string(status))
    status
  end

  defp num_failed(responses) do
    Enum.count(
      responses, 
      fn(r) ->
        case r do
          {:error, _} -> true
          _ -> false
        end
      end
    )
  end
end
