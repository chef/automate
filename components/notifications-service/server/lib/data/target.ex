defmodule Notifications.Target do
  defstruct url: "", username: "", password: "", filter: true, format: &Notifications.Target.null_format/1, critical_controls_only: false

  def null_format(_) do 
    ""
  end
end

defmodule Notifications.TargetBuilder do
  alias Notifications.Formatters
  alias Notifications.Target
  alias Notifications.Data.SecretStore
  require Logger

  def create_target({_, %type{url: url, secret_id: secret_id, critical_controls_only: critical_controls_only}}, event) do
    {username, password} = case secret_id do
      "" -> {"", ""}
      _ -> get_target_username_password(secret_id)
    end

    {pre_filter, formatter} = case critical_controls_only do
      true -> {true, criticals_only_formatter(type)}
      _ -> {pre_filter(type, event), formatter(type)}
    end

    %Target{url: url, critical_controls_only: critical_controls_only, username: username, password: password, filter: pre_filter, format: formatter}
  end
  def create_target({_, %type{url: url}}, event) do
    %Target{url: url, username: "", password: "", filter: pre_filter(type, event), format: formatter(type)}
  end

  defp formatter(Notifications.WebhookAlert), do: &Formatters.Webhook.format/1
  defp formatter(Notifications.ServiceNowAlert), do: &Formatters.ServiceNow.format/1
  defp criticals_only_formatter(Notifications.ServiceNowAlert), do: &Formatters.ServiceNowCritical.Compliance.format/1
  defp formatter(Notifications.SlackAlert), do: &Formatters.Slack.format/1
  defp formatter(_), do: &Notifications.Target.null_format/1

  defp pre_filter(Notifications.ServiceNowAlert, event), do: event != 2
  defp pre_filter(_, _), do: true

  defp get_target_username_password(secret_id) do
    case SecretStore.get_target_username_password(secret_id) do
      {:ok, {username, password}} -> {username, password}
      {:error, _} ->
        Logger.error fn() -> "error retrieving target username and password for secret_id: #{secret_id}" end
        {"", ""}
    end
  end
end
