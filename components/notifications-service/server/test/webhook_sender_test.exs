defmodule Notifications.WebhookSender.Test do
  use ExUnit.Case, async: false
  import Mock
  alias Notifications.WebhookSender

  # Mock Helpers
  def config_mock(max_conns) do
    {Notifications.Config, [],
      [http_pool_config: fn() -> [max_connections: max_conns] end]}
  end

  def webhook_impl_mock(ret) do
    {Notifications.WebhookSender.Impl, [], [post: fn(_, _, _, _, _) -> ret end]}
  end
  def webhook_impl_mock_with_delay(ret, duration) do
    {Notifications.WebhookSender.Impl, [],
      [post: fn(_, _, _, _, _) -> :timer.sleep(duration); ret end]
    }
  end

  describe "post/5" do
    test "it initiates a send successfully when connections are available" do
      with_mocks([config_mock(1000), webhook_impl_mock(:ok) ]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        assert :ok == WebhookSender.post("http://example.com", %{}, "req-id-here", "", "")
      end
    end

    test "it fails to spawn a sender proc when all connections are in use" do
      with_mocks([config_mock(0), webhook_impl_mock(:ok) ]) do
      {:ok, _pid} = WebhookSender.start_link(:ignored)
        assert {:error, :no_connections} ==
          WebhookSender.post("http://example.com", %{}, "req-id-here", "", "")
      end
    end

    test "it returns :ok even if an upstream error occurs" do
      with_mocks([config_mock(1000),
                  webhook_impl_mock({:error, {"404", "details"}})]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        result = WebhookSender.post("http://nowhere", %{}, "req-id-here", "", "")
        assert :ok == result
      end
    end
  end

  describe "stats/0" do
    test "it reports stats correctly after rejecting a request" do
      with_mocks([config_mock(0), webhook_impl_mock(:ok) ]) do
      {:ok, _pid} = WebhookSender.start_link(:ignored)
        WebhookSender.post("http://example.com", %{}, "req-id-here", "", "")
        expected = {:ok, %{dropped: 1, in_use: 0, max: 0, serviced: 0}}
        assert expected == WebhookSender.stats
      end
    end
    test "it reports stats correctly after processing a request" do
      with_mocks([config_mock(1000), webhook_impl_mock(:ok) ]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        WebhookSender.post("http://example.com", %{}, "req-id-here", "", "")
        # Until our mocked sender func for WebhookSender.Impl executes,
        # this connection will show in use - but that could happen before or after we check it.
        # Sleep is messy, tried and true for these situations...
        :timer.sleep(250)
        expected = {:ok, %{dropped: 0, in_use: 0, max: 1000, serviced: 1}}
        assert expected == WebhookSender.stats
      end
    end

    test "it reports in-use and serviced counts corrently when a request is in flight" do
      with_mocks([config_mock(1000), webhook_impl_mock_with_delay(:ok, 500) ]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        WebhookSender.post("http://example.com", %{}, "req-id-here", "", "")
        expected = {:ok, %{dropped: 0, in_use: 1, max: 1000, serviced: 1}}
        assert expected == WebhookSender.stats
      end
    end

    # TODO - really just a sync request, maybe some renaming is in order
    test "serviced count is updated correctly when a webhook validation request is processed" do
      with_mocks([config_mock(1000), webhook_impl_mock(:ok)]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        :ok  = WebhookSender.validate_webhook("http://example.com", "", "")
        expected = {:ok, %{dropped: 0, in_use: 0, max: 1000, serviced: 1}}
        assert expected == WebhookSender.stats
      end
    end

    test "dropped count is updated correctly when a webhook validation request is not processed" do
      with_mocks([config_mock(0)]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        expected = {:ok, %{dropped: 1, in_use: 0, max: 0, serviced: 0}}
        {:error, :no_connections} = WebhookSender.validate_webhook("http://urlhere", "", "")
        assert expected == WebhookSender.stats
      end
    end
  end

  describe "validate_webhook/3" do
    test "it invokes a synchronous call to post a payload to the target url" do
      with_mocks([webhook_impl_mock(:ok)]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        assert :ok == WebhookSender.validate_webhook("http://example.com", "", "")
      end
    end

    test "it does not attempt to post to an invalid URL" do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
      assert {:error, :invalid_url} == WebhookSender.validate_webhook("this is not a url", "", "")
    end

    test "it reports no connections when all connections are in use" do
      with_mocks([config_mock(0)]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        assert {:error, :no_connections} == WebhookSender.validate_webhook("http://example.com", "", "")
      end
    end

    test "it returns an error tuple if upstream errors occur" do
      with_mocks([config_mock(1000),
                  webhook_impl_mock({:error, {"404", "details"}})]) do
        {:ok, _pid} = WebhookSender.start_link(:ignored)
        expected = {:error, {"404", "details"}}
        assert expected == WebhookSender.validate_webhook("http://nowhere", "", "")
      end
    end
  end

end
