defmodule Notifications.Dispatcher.Test do
  use ExUnit.Case
  import Mock

  alias Notifications.Dispatcher
  alias Notifications.Data
  alias Notifications.{SlackAlert, WebhookAlert}

  describe "process_notification/2" do
    test "that it casts inbound messages for async handling" do
      with_mocks([
        # Kind of icky testing the genserver call, but the behavior we want to verify
        # is that the request is handled asynchronously. Our other option in exunit is to
        # go even lower level, looking at the internal messages used by genserver - this seemed the
        # lesser of two evils.
        {GenServer, [:passthrough], [cast: fn(_, _) -> :ok end]}
      ]) do
        notification = Notifications.CCRFailure.new
        id = "1"
        Dispatcher.process_notification(notification, id)
        assert called GenServer.cast(Dispatcher, {:notification, notification, id})
      end
    end
  end

  describe "handle_cast/2" do
    test "that received notifications are routed to Dispatcher.Impl" do
      with_mocks([
          {Dispatcher.Impl, [], [process_notification: fn(_, _) -> :ok end]}
      ]) do
        notification = Notifications.CCRFailure.new
        id = "1"
        Dispatcher.handle_cast({:notification, notification, id}, :unused_state)
        assert called Dispatcher.Impl.process_notification(notification, id)
      end
    end
  end
end

defmodule Notifications.Dispatcher.Impl.Test do
  use ExUnit.Case
  import Mock
  doctest Notifications.Dispatcher

  alias Notifications.{Formatters, WebhookSender, Capture, Prefilter, Data}
  alias Notifications.{Target, TargetBuilder}
  alias Notifications.{SlackAlert, WebhookAlert, ServiceNowAlert}

  setup do
    url = "https://testing.localhost"
    username = "bob"
    password = "super_secret"
    {:ok,
        url: url,
        notification: %Notifications.CCRFailure{run_id: "runid-1"},
        compliance_failure: %Notifications.ComplianceFailure{id: "id-1"},

        username: username,
        password: password,
        slack_target: %Target{url: url, username: "", password: "", filter: true, format: &Formatters.Slack.format/1},
        webhook_target: %Target{url: url, username: "", password: "", filter: true, format: &Formatters.Webhook.format/1},
        service_now_target: %Target{url: url, username: username, password: password, filter: true, format: &Formatters.ServiceNow.format/1},
        service_now_compliance_target: %Target{url: url, username: username, password: password, filter: false, format: &Formatters.ServiceNow.format/1}
    }
  end

  describe "process_notification/2" do
    test "runs the notification thorugh the complete pipeline when a matching webhook rule exists",
         %{url: url, notification: notification, webhook_target: target} do
      with_mocks([
        {Data.Store, [], [get_targets_for_event: fn(_) -> {:ok, [target]} end]},
        {WebhookSender, [], [post: fn(_url, _payload, _reqid, _username, _password) -> :ok end]},
        {Prefilter, [], [process_notification: fn(_notif, _reqid) -> :continue end]},
        {Prefilter, [], [process_target: fn(_notif, _reqid, _target) -> :continue end]},
        {Formatters.Webhook, [], [format: fn(_payload) -> %{}  end]},
        {Capture, [], [process_notification: fn(_event) -> :ok end]}
      ]) do
        Notifications.Dispatcher.Impl.process_notification(notification, "someid")
        assert called Capture.process_notification(notification)
        assert called Prefilter.process_notification(notification, "someid")
        assert called Prefilter.process_target(notification, "someid", target)
        assert called Formatters.Webhook.format(notification)
        assert called WebhookSender.post(url, %{}, "someid", "", "")
      end
    end

    test "runs the compliance failure through the complete pipeline when a matching webhook rule exists",
         %{url: url, compliance_failure: compliance_failure, webhook_target: target} do
      with_mocks([
        {Data.Store, [], [get_targets_for_event: fn(_) -> {:ok, [target]} end]},
        {WebhookSender, [], [post: fn(_url, _payload, _reqid, _username, _password) -> :ok end]},
        {Prefilter, [], [process_notification: fn(_notif, _reqid) -> :continue end]},
        {Prefilter, [], [process_target: fn(_notif, _reqid, _target) -> :continue end]},
        {Formatters.Webhook, [], [format: fn(_payload) -> %{}  end]},
        {Capture, [], [process_notification: fn(_event) -> :ok end]}
      ]) do
        Notifications.Dispatcher.Impl.process_notification(compliance_failure, "someid")
        assert called Capture.process_notification(compliance_failure)
        assert called Prefilter.process_notification(compliance_failure, "someid")
        assert called Prefilter.process_target(compliance_failure, "someid", target)
        assert called Formatters.Webhook.format(compliance_failure)
        assert called WebhookSender.post(url, %{}, "someid", "", "")
      end
    end

    test "runs the compliance failure through the complete pipeline when a matching service now  webhook rule exists",
         %{url: url, compliance_failure: compliance_failure, service_now_compliance_target: target, username: username, password: password} do
      with_mocks([
        {Data.Store, [], [get_targets_for_event: fn(_) -> {:ok, [target]} end]},
        {WebhookSender, [], [post: fn(_url, _payload, _reqid, _username, _password) -> :ok end]},
        {Prefilter, [], [process_notification: fn(_notif, _reqid) -> :continue end]},
        {Prefilter, [], [process_target: fn(_notif, _reqid, _target) -> :continue end]},
        {Formatters.ServiceNow.Compliance, [], [format: fn(_payload) -> %{}  end]},
        {Capture, [], [process_notification: fn(_event) -> :ok end]}
      ]) do
        Notifications.Dispatcher.Impl.process_notification(compliance_failure, "someid")
        assert called Capture.process_notification(compliance_failure)
        assert called Prefilter.process_notification(compliance_failure, "someid")
        assert called Prefilter.process_target(compliance_failure, "someid", target)
        assert called Formatters.ServiceNow.Compliance.format(compliance_failure)
        assert called WebhookSender.post(url, %{}, "someid", username, password)
      end
    end

    test "capture the request and dispatch to slack formatter prior to posting",
         %{url: url, notification: notification, slack_target: target} do
      with_mocks([
        {Data.Store, [], [get_targets_for_event: fn(Notifications.CCRFailure) -> {:ok, [target]} end]},
        {WebhookSender, [], [post: fn(_url, _payload, _reqid, _username, _password) -> :ok end]},
        {Formatters.Slack, [], [format: fn(_payload) -> %{} end]},
        {Prefilter, [], [process_notification: fn(_notif, _reqid) -> :continue end]},
        {Prefilter, [], [process_target: fn(_notif, _reqid, _target) -> :continue end]},
        {Capture, [], [process_notification: fn(_event) -> :ok end]}
      ]) do
        Notifications.Dispatcher.Impl.process_notification(notification, "someid")
        assert called Capture.process_notification(notification)
        assert called Prefilter.process_notification(notification, "someid")
        assert not called Prefilter.process_target(notification, "someid")
        assert called Formatters.Slack.format(notification)
        assert called WebhookSender.post(url, %{}, "someid", "", "")
      end
    end

    test "capture the request and dispatch to service now formatter prior to posting",
         %{url: url, username: username, password: password, notification: notification, service_now_target: target} do
      with_mocks([
        {Data.Store, [], [get_targets_for_event: fn(Notifications.CCRFailure) -> {:ok, [target]} end]},
        {WebhookSender, [], [post: fn(_url, _payload, _reqid, _username, _password) -> :ok end]},
        {Formatters.ServiceNow, [], [format: fn(_payload) -> %{} end]},
        {Prefilter, [], [process_notification: fn(_notif, _reqid) -> :continue end]},
        {Prefilter, [], [process_target: fn(_notif, _reqid, _target) -> :continue end]},
        {Capture, [], [process_notification: fn(_event) -> :ok end]}
      ]) do
        Notifications.Dispatcher.Impl.process_notification(notification, "someid")
        assert called Capture.process_notification(notification)
        assert called Prefilter.process_notification(notification, "someid")
        assert called Prefilter.process_target(notification, "someid", target)
        assert called Formatters.ServiceNow.format(notification)
        assert called WebhookSender.post(url, %{}, "someid", username, password)
      end
    end

    test "it stops processing a notification when prefilter tells it to, before applying rules",
         %{notification: notification, webhook_target: target} do
      with_mocks([
        {Data.Store, [], [get_targets_for_event: fn(Notifications.CCRFailure) -> {:ok, [target]} end]},
        {WebhookSender, [], [post: fn(_url, _payload, _reqid, _username, _password) -> :ok end]},
        {Prefilter, [], [process_notification: fn(_notif, _reqid) -> :skip end]},
      ]) do
        Notifications.Dispatcher.Impl.process_notification(notification, "someid")
        refute called WebhookSender.post
      end
    end

    test "Captures but does not send along events it has no rules for",
         %{notification: notification} do
      with_mocks([
        {Data.Store, [], [get_targets_for_event: fn(Notifications.CCRFailure) -> {:ok, []} end]},
        {Data.Store, [], [event_processed?: fn(_, _) -> false end]},
        {WebhookSender, [], [post: fn(_url, _payload, _reqid, _username, _password) -> :ok end]},
        {Capture, [], [process_notification: fn(_event) -> :ok end]},
        # {Prefilter, [], [process_notification: fn(_notif, _reqid) -> :continue end]},
      ]) do
        Notifications.Dispatcher.Impl.process_notification(notification, "someid100")
        assert called Capture.process_notification(notification)
        refute called WebhookSender.post
      end

    end

  end
end
