defmodule H do
  @moduledoc "Various helper functions for use with Notifications in iex"
  @test_data_path "../testdata"

  @doc """
  Extract the slack url from the SLACK_URL environment variable.
  """
  def get_webhook_url() do
    case System.get_env("SLACK_URL") do
      nil ->
        :no_slack_url_env_var
      url ->
        {:ok, url}
    end
  end

  @doc """
  Set up rules for all event types that will post alerts to Set up rules for all event types
  that will post alerts to #a2-notifications-test"

  This function will do nothing if any rules already exist.
  """
  def init_default_slack_rules() do
    {:ok, webhook_url} = get_webhook_url()
    add_rule("ccr-failure-to-slack", :SlackAlert, :CCRFailure,  webhook_url)
    add_rule("ccr-success-to-slack", :SlackAlert, :CCRSuccess,  webhook_url)
    add_rule("comp-success-to-slack", :SlackAlert, :ComplianceSuccess, webhook_url)
    add_rule("comp-failure-to-slack", :SlackAlert, :ComplianceFailure, webhook_url)
    :ok
  end

  @doc """
  Add a notification alerting rule.

  ## Parameters
  name: the name of the rule
  type: :slack or :custom
  event: one of "ccr_failure", "ccr_success", "compliance_failure", "compliance_success", "Assets"
  url: url to post to when event is triggered

  ## Examples

  iex> add_rule("ccr-failure-webook", :WebhookAlert, :CCRFailure,  "http://localhost/capture")

  """
  def add_rule(name, type, event, url) do
    action = Module.concat(Notifications, type).new(url: url)
    r = Notifications.Rule.new(name: name,
                               event: Notifications.Rule.Event.value(event),
                               action: action)
    {:ok, _id} = Notifications.Data.Store.add_rule(r)
  end
  #
  @doc """
  Enable capture of inbound notifications.  They will be saved in ./log"
  """
  def enable_capture do
    Notifications.Config.capture_payloads!
  end

  @doc """
  Returns a GRPC channel suitable for use in calls to
  Notifications.Notifications.Stub.*

  The channel is cached in the process dictionary and will only be
  created if it does not exist.
  """
  def grpc_channel do
    case Process.get(:grpc_channel) do
      nil ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{Notifications.Config.grpc_port}")
        Process.put(:grpc_channel, channel)
        channel
      channel ->
        channel
    end
  end

  @doc """
  Displays a list of events available to be loaded from saved test data.
  These events can be loaded via name or number - see #load_notification
  and #load_event.

  ## Examples
    iex> H.ls_events
  """
  def ls_events do
  IO.puts """
    You can call H.load_event or H.load_notification using
    a name or number from the list below:
    """
    # Side effects ftw
    _  = List.foldl(events(), 1, fn(e, i) ->
                                   IO.puts("#{i}: #{e}")
                                   i + 1
                                 end)
    IO.puts("")
    :ok
  end

  @doc """
  Send a notification over GRPC

  ## Parameters
  event: event name or number (from #ls_events) or a Notifications.Event struct.

  ## Examples
  iex> H.ls_notifications
    1: sample-ccr
    2: sample-comp-fail

    iex> H.grpc_notify 1
    iex> H.grpc_notify "sample-ccr")
    iex> H.grpc_notify Event.new(ccr_failure: CCRFailure.new(), id: "id")

  """
  def grpc_notify(%Notifications.Event{} = event) do
    Notifications.Notifications.Stub.notify(H.grpc_channel, event)
  end
  def grpc_notify(event) when is_binary(event) or is_integer(event) do
    grpc_notify(load_event(event))
  end

  @doc """
  Returns a notification by name or number (as provided by #ls_events).

  ## Parameters
  - notification: the name or number of a notification as shown by ls_events

  ## Examples
  iex>  H.load_notification(1)
  """
  def load_notification(index) when is_integer(index) do
    load_notification(index_to_name(index))
  end
  def load_notification(name) do
    load_event(name) |> Notifications.extract
  end


  @doc """
  Returns a Notifications.Event by name or number (as provided by #ls_events).

  ## Parameters
  - notification: the name or number of a notification as shown by ls_events

  ## Example
  iex>  H.load_notification(1)
  """
  def load_event(index) when is_integer(index) do
    load_event(index_to_name(index))
  end
  def load_event(name) do
    IO.puts("Loading #{name}")
    path = Path.join([@test_data_path, "#{name}.pb"])
    {:ok, raw} = File.read(path)
    Notifications.Event.decode(raw)
  end

  def events do
    IO.puts "#{@test_data_path}/*.pb"
    results =  Path.wildcard("#{@test_data_path}/*.pb")
    for <<"#{@test_data_path}/", path :: binary>> <- results do
      path |> String.split(".") |> hd
    end
  end

  defp index_to_name(index) do
    e = events()
    case Enum.at(e, index - 1) do
      nil ->
        raise(ArgumentError, "No such event #{index}")
      name -> name
    end
  end

  @doc "Invoke this to make function help available via 'h'"
  def levelup do
    IEx.Helpers.c(".iex.exs", ".") # Store iex.exs.beam so that docs are available
    IO.puts """
    Help is here! Try it out:
        iex> h H.ls_events

    """
  end
end
alias Notifications.Event
alias Notifications.Notifications.Stub
alias Notifications.Data.Store
# Just to prevent warnings, reference the aliases we created :
Stub
RuleStore
Event

IEx.configure [inspect: [limit: :infinity]]
IO.puts """
The following modules have been aliased:

Notifications.Notifications.Stub -> Stub
Notifications.Data.Store -> DataStore
Notifications.Event -> Event
This iex shell has been augmented by a Helper!
Helper functions are in the module "H".  Type
"H.[tab]" for a list of available functions.


For best results, type this now:

iex> H.levelup
iex> import H

"""
