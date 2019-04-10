defmodule Notifications.Formatters.Behavior do
  @moduledoc "Behavior definition for a webhook formatter."
  @callback format(Notification.NotifyRequest) :: map
end
