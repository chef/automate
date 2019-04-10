defmodule Notifications.Target do
  defstruct url: "", username: "", password: "", filter: true, format: &Notifications.Target.null_format/1

  def null_format(_) do 
    ""
  end
end

defmodule Notifications.TargetBuilder do
  alias Notifications.Formatters
  alias Notifications.Target
  alias Notifications.Data.SecretStore
  require Logger

  def create_target({_, %type{url: url, secret_id: secret_id}}, event) do
    {username, password} = case secret_id do
      "" -> {"", ""}
      _ -> get_target_username_password(secret_id)
    end

    %Target{url: url, username: username, password: password, filter: pre_filter(type, event), format: formatter(type)}
  end
  def create_target({_, %type{url: url}}, event) do
    %Target{url: url, username: "", password: "", filter: pre_filter(type, event), format: formatter(type)}
  end

  defp formatter(Notifications.WebhookAlert), do: &Formatters.Webhook.format/1
  defp formatter(Notifications.ServiceNowAlert), do: &Formatters.ServiceNow.format/1
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
