defmodule Notifications.CaptureTest do
  use ExUnit.Case, async: false
  import Mock

  describe "capture" do
    test "it verifies the capture flag is enabled before spawning a capture" do
      with_mocks([
        {Notifications.Config, [], [capture_payloads?: fn() -> false end]},
      ]) do
        ccr_failure = Notifications.CCRFailure.new(run_id: "a")
        Notifications.Capture.process_notification(ccr_failure)
        # This is the limit of what we can sanely verify in ExUnit at this
        # time without  artificially making capture_payload public to serve the test.
        assert called Notifications.Config.capture_payloads?
      end
    end
  end
end
