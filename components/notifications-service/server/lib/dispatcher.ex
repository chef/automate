defmodule Notifications.Dispatcher do
  @moduledoc """
  Module that dispatches notifications to their correct notifier (webhook, slack) type.
  """

  require Logger
  use GenServer

  def init(init_arg) do
    {:ok, init_arg}
  end

  #####################
  # API
  #####################
  def start_link(_) do
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])
  end

  @spec process_notification(Notifications.notification, String.t) :: :ok
  def process_notification(notification, id) do
    Logger.debug fn() -> "Dispatching notification request #{id} for async processing" end
    GenServer.cast(__MODULE__, {:notification, notification, id})
  end

  #####################
  # Callbacks
  #####################
  def handle_cast({:notification, notification, id}, state) do
    Logger.debug fn() -> "Dispatcher received inbound async message #{id}." end
    Notifications.Dispatcher.Impl.process_notification(notification, id)
    {:noreply, state}
  end
end

# The actual handler - keeping it in its own defmodule means the behavior
# of the 'process_notification' call is unit-testable - with the added bonus that it's
# logically separated from the process that handles the messages vs being unexported module functions.
defmodule Notifications.Dispatcher.Impl do
  alias Notifications.Data.Store
  alias Notifications.Prefilter
  alias Notifications.Capture

  @moduledoc "Formats and sends alerts to their outbound targets"
  require Logger

  @type notification_result :: :ok
  @spec process_notification(Notifications.notification, String.t) :: notification_result
  def process_notification(%type{} = notification, id) do
    Logger.debug fn() -> "Processing #{type} notification request #{id}" end
    Capture.process_notification(notification)
    if Prefilter.process_notification(notification, id) == :continue do
      apply_rules(notification, type, id)
    end
  end

  defp apply_rules(notification, type, id) do
    case Store.get_targets_for_event(type) do
      :not_found -> :ok # No targests.
      {:ok, targets} ->  process_targets(targets, id, notification)
     end
  end

  # TODO - format for each target type only once, even if we have multiple rules for the
  #        same target action type that apply. (eg multiple slack rules for diff channels)
  defp process_targets([], _id, _notification), do: :ok
  defp process_targets([target | rest], id, notification) do
    if Prefilter.process_target(notification, id, target) == :continue do
      payload = apply(target.format, [notification])
      send_message(target, payload, id)
    end
    process_targets(rest, id, notification)
  end

  defp send_message(%{url: url, username: username, password: password}, payload, id) do
    Notifications.WebhookSender.post(url, payload, id, username, password)  
  end
end
