defmodule Notifications.Prefilter.Test do
  use ExUnit.Case
  import TestHelper
  import Mock
  alias Notifications.Prefilter.Impl

  @test_id  "an_id"

  describe "process_notification/2" do
    test "accepts the notification when all filters accept the notification" do
      # Using passthrough so that the :mk_input helper will not break:
        with_mocks([{Impl, [:passthrough], [filter: fn(args, _any) -> args end]}]) do
        notif = load_notif("inspec-report-single-failure")
        assert :continue == Notifications.Prefilter.process_notification(notif, @test_id)
      end
    end

    test "tell caller to stop processing the notification when type check fails" do
      with_mocks([{Impl, [:passthrough],
                   [filter: fn(_args, :type) -> {:skip, :some_reason}
                              (args, _filter_name) -> args
                            end]}]) do
        notif = load_notif("inspec-report-single-failure")
        assert :skip == Notifications.Prefilter.process_notification(notif, @test_id)
      end
    end

    test "tells caller to stop processing the notification when duplicate check fails" do
      with_mocks([{Impl, [:passthrough],
                   [filter: fn(args, :type) -> args
                              (_args, :duplicate) -> {:skip, :already_processed}
                            end]}]) do
        notif = load_notif("inspec-report-single-failure")
        assert :skip == Notifications.Prefilter.process_notification(notif, @test_id)
      end
    end
  end
end

defmodule Notifications.Prefilter.Impl.Test do
  use ExUnit.Case
  import TestHelper
  import Mock
  alias Notifications.Prefilter.Impl

  @test_id  "an_id"
  @service_now_compliance_target %Notifications.Target{url: "", username: "", password: "", filter: false, format: &Notifications.Formatters.ServiceNow.Compliance.format/1, critical_controls_only: false}
  @service_now_criticals_compliance_target %Notifications.Target{url: "", username: "", password: "", filter: true, format: &Notifications.Formatters.ServiceNow.Compliance.format/1, critical_controls_only: true}
  @webhook_target %Notifications.Target{url: "", username: "", password: "", filter: true, format: &Notifications.Formatters.Webhook.Compliance.format/1}

  def mock_event_processed(ret_val) do
    {Notifications.Data.Store, [], [event_processed?: fn(_, _) -> ret_val end]}
  end

  describe "filter/2" do
    test "does not processs input that has already been rejected" do
      assert {:skip, :reason} == Impl.filter({:skip, :reason}, :unused)
    end
  end

  describe "filter/3: (_, :type, :target}" do
    test "rejects compliance failures with no critical controls failed" do
      notif = load_notif("inspec-report-single-non-crit-control-failure")
      input = Impl.mk_target_input(notif, @test_id, @webhook_target)
      assert {:skip, :no_critical_controls_failed} == Impl.filter(input, :type, :target)
    end

    test "accepts compliance failures with no critical controls failed for servicenow" do
      notif = load_notif("inspec-report-single-non-crit-control-failure")
      input = Impl.mk_target_input(notif, @test_id, @service_now_compliance_target)
      assert input == Impl.filter(input, :type, :target)
    end

    test "rejects compliance failures with no critical controls failed for servicenow" do
      notif = load_notif("inspec-report-single-non-crit-control-failure")
      input = Impl.mk_target_input(notif, @test_id, @service_now_criticals_compliance_target)
      assert {:skip, :no_critical_controls_failed} == Impl.filter(input, :type, :target)
    end
  end

   describe "filter/2: (_, :type}" do
    test "accepts compliance failures with at least one critical control failed" do
      notif = load_notif("inspec-report-single-failure")
      input = Impl.mk_input(notif, @test_id)
      # In success cases, input will look identical to ouput, so that it
      # can be passed to the next stage of the filter.
      assert input == Impl.filter(input, :type)
    end

    test "accepts CCRFailure" do
      notif = load_notif("converge-failure-report")
      input = Impl.mk_input(notif, @test_id)
      assert input == Impl.filter(input, :type)
    end

    test "rejects unsupported type: CCRSuccess" do
      notif = load_notif("converge-success-report")
      input = Impl.mk_input(notif, @test_id)
      assert Impl.filter(input, :type) == {:skip, :unsupported}
    end
    test "rejects unsupported type: ComplianceSuccess" do
      notif = load_notif("inspec-all-success")
      input = Impl.mk_input(notif, @test_id)
      assert Impl.filter(input, :type) == {:skip, :unsupported}
    end
  end

  describe "filter/2: (_, :duplicate)" do
    test "it rejects duplicate notifications" do
      with_mocks([mock_event_processed(true)]) do
        notif = load_notif("converge-failure-report")
        input = Impl.mk_input(notif, @test_id)
        assert {:skip, :already_processed}== Impl.filter(input, :duplicate)
      end
    end
    test "it accepts non-duplicate notifications" do
      with_mocks([mock_event_processed(false)]) do
        notif = load_notif("converge-failure-report")
        input = Impl.mk_input(notif, @test_id)
        assert input == Impl.filter(input, :duplicate)
      end
    end
  end
end

