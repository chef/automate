defmodule Notifications.Prefilter do
  @moduledoc """
  Examines an inbound notification and determines if it should be forwarded
  on to the next stage of processing.  Runs a pipeline of filters that
  stops at first failed condition, and otherwise runs all the way through.

  Currently the following are checked:
   - is it a critical control failure (if compliance failure)
   - have we already processed this notification in another request?
   - do we have a formatter for this notification type?

  """
  require Logger
  alias Notifications.Prefilter.Impl

  def process_notification(%type{} = notification, id) do
    Logger.debug fn -> "Prefiltering notification #{id}" end
    result = Impl.mk_input(notification, id)
             |> Impl.filter(:type)
             |> Impl.filter(:duplicate)
    case result do
      {:continue, _} ->
      Logger.debug fn -> "Permitting event #{type} #{id} to continue for rules processing" end
        :continue
      {:skip, reason} ->
      Logger.debug fn -> "Event #{id} discarded in prefilter: #{inspect reason}" end
        :skip
    end
  end

  def process_target(%type{} = notification, id, target) do
    Logger.debug fn -> "Prefiltering notification #{id}" end
    result = Impl.mk_target_input(notification, id, target)
             |> Impl.filter(:type, :target)
    case result do
      {:continue, _} ->
      Logger.debug fn -> "Permitting event #{type} #{id} to continue for rules processing" end
        :continue
      {:skip, reason} ->
      Logger.debug fn -> "Event #{id} discarded in prefilter: #{inspect reason}" end
        :skip
    end
  end
end

defmodule Notifications.Prefilter.Impl do
  @moduledoc "Implementation for Prefilter"
  alias Notifications.Data.Store
  alias Notifications.{ComplianceFailure, CCRSuccess, ComplianceSuccess}

  # These notifications have no formatters to support them - no reason
  # to let the process all the way through.
  @unsupported_notifications [CCRSuccess, ComplianceSuccess]

  def mk_input(notification, id) do
    {:continue, {notification, id}}
  end

  def mk_target_input(notification, id, target) do
    {:continue, {notification, id, target}}
  end

  def filter({:continue, {notification, id}}, :type) do
    type_filter(notification, id)
  end

  def filter({:continue, {%type{} = notification, id}}, :duplicate) do
    case Store.event_processed?(type, id)  do
      true -> {:skip, :already_processed}
      false -> {:continue, {notification, id}}
    end
  end

  def filter({:continue, {notification, id, target}}, :type, :target) do
    case target.filter do
      true -> type_filter(notification, id, target)
      false -> {:continue, {notification, id, target}}
    end
  end

  # When used as part of the pipeline, ensures that any previous failure
  # stops us from evaluating further.
  def filter({:skip, _reason} = args, _any_type), do: args

  defp type_filter(%type{}, _id) when type in @unsupported_notifications, do: {:skip, :unsupported}
  defp type_filter(notification, id), do: {:continue, {notification, id}}

  defp type_filter(%ComplianceFailure{test_totals: %{critical_failed: num}} = notification, id, target) when num > 0 do
    {:continue, {notification, id, target}}
  end
  defp type_filter(%ComplianceFailure{}, _id, target) do
    {:skip, :no_critical_controls_failed}
  end
  defp type_filter(notification, id, target), do: {:continue, {notification, id, target}}

end


