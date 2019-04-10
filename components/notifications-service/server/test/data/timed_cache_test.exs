
#
defmodule Notifications.Data.TimedCache.Test do
  use ExUnit.Case, async: false
  alias Notifications.Data.TimedCache

  describe "start_cache/4" do
    setup do
      {:ok, _} = start_supervised(TimedCache)
      # StatefulLoader is a a test gen-server defined for validating
      # caching behavior - it and its tests at the bottom of this file.
      {:ok, _} = start_supervised(StatefulLoader)
      {:ok, []}
    end

    test "it immediately executes the provided loader function during cache init" do
      result = TimedCache.start_cache(:start_test, &StatefulLoader.identity/1,
                                           [:one],
                                           # Set high timeout so we don't
                                           # get a reload hit after init.
                                           100000)
      assert :one == result
      assert 1 == StatefulLoader.hits()
    end

    test "it returns the cached data when cache is alreasy started for a key, without reinvoking loader fun" do
      :one = TimedCache.start_cache(:start_test, &StatefulLoader.identity/1,
                                    [:one], 100000)
      :one = TimedCache.start_cache(:start_test, &StatefulLoader.identity/1,
                                    [:one], 100000)
      assert 1 == StatefulLoader.hits()
    end

    test "it returns the error when the loader returns an error tuple" do
      result = TimedCache.start_cache(:start_test, &StatefulLoader.error_tuple_loader/0,
                                      [], 100000)
      assert result == {:error, :failed}
      assert 1 == StatefulLoader.hits()
    end

    test "it returns the error when the loader crashes" do
      crash_data = TimedCache.start_cache(:start_test, &StatefulLoader.crash_loader/1,
                                          [:do_not_see_me], 100000)
      assert crash_data != :do_not_see_me
      assert is_tuple(crash_data)
    end

    test "when multiple processes initialize cache concurrently, loader is only run once and both receive correct data" do
      # Make sure we dont' get stuck waiting if the spawned proc
      # doesn't get its message out
      report_to = self()
      concurrent_load = fn() ->
        result = TimedCache.start_cache(:concurrent_test,
                                        &StatefulLoader.slow_identity/2,
                                        [200, :slow_value], 10000)
          send report_to, result
      end

      spawn(concurrent_load)
      spawn(concurrent_load)
      spawn(concurrent_load)
      spawn(concurrent_load)
      Process.sleep(200)
      result1 = receive do r -> r end
      result2 = receive do r -> r end
      result3 = receive do r -> r end
      result4 = receive do r -> r end

      # Two interleaved inits should be handled sanely, resulting in
      # only one call to the loader function.
      assert :slow_value == result1
      assert :slow_value == result2
      assert :slow_value == result3
      assert :slow_value == result4
      assert 1 == StatefulLoader.hits
    end
  end

  describe "get/1" do
    setup do
     {:ok, _} = start_supervised(TimedCache)
      {:ok, _} = start_supervised(StatefulLoader)
      {:ok, []}
    end

    test "it returns stale data when initial load succeeds but refresh fails" do
      :one = TimedCache.start_cache(:stale_test, &StatefulLoader.identity/1,
                                    [:one], 100)

                                    #StatefulLoader.force_next_reply({:error, :failed})
                                    #Process.sleep(100)


    end
    test "it returns cached data without refreshing via loader fun before the timeout" do
      # Long refresh interval so we can verify multiple calls without refresh running
      TimedCache.start_cache(:one_load_test, &StatefulLoader.identity/1,
                             [:some_value], 100000)
      for _ <- [1,2,3] do
        :some_value = TimedCache.get(:one_load_test)
      end
      assert 1 == StatefulLoader.hits()
    end

    test "it refreshes the data with the provided loader function at the specified interval" do
      # Short refresh interval to ensure multiple runs.
      # We won't really execute at one per 10 ms due to vm overhead, but we do
      # want to ensure we've refreshed close to the number of expected times
      TimedCache.start_cache(:refresh_test, &StatefulLoader.identity/1,
                             [:some_value], 10)
      :some_value = TimedCache.get(:refresh_test)
      Process.sleep(150)
      assert StatefulLoader.hits() >= 10
    end
  end

  describe "stop_cache/1" do
    setup do
      {:ok, _} = start_supervised(TimedCache)
      {:ok, _} = start_supervised(StatefulLoader)
      {:ok, []}
    end

    test "cache will no longer refresh after it's deleted" do
      TimedCache.start_cache(:one_load_test, &StatefulLoader.identity/1,
                             [:some_value], 100)
      TimedCache.stop_cache(:one_load_test)
      Process.sleep(200)
      # If we did disable the cache refresh, the timeout would have expired
      # resulting in a hit count > 1
      assert StatefulLoader.hits() == 1
    end
  end
end

# A simple gen-server that keeps tracks of how many times the 'identity'
# function is called in the processe's lifetime.
# Assumes a new instance is created for each test.
defmodule StatefulLoader do
  use GenServer
  # Returns how many times 'identity' has been invoked since startup
  def hits(), do: GenServer.call(__MODULE__, :hits)
  # Returns 'value'
  def start_link(_), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)
  def identity(value), do: GenServer.call(__MODULE__, {:identity, value})
  def slow_identity(time, value), do: GenServer.call(__MODULE__, {:slow_identity, time, value})
  def crash_loader(val), do: GenServer.call(__MODULE__, {:crash_loader, val})
  def error_tuple_loader(), do: GenServer.call(__MODULE__, :error_tuple_loader)
  def init(_), do: {:ok, 0}
  def handle_call(:hits, _, count), do: {:reply, count, count}
  def handle_call({:identity, value}, _, hits), do: {:reply, value, hits + 1}

  def handle_call({:crash_loader, value}, _, hits) do
    true = false
    {:reply, value, hits + 1}
  end

  def handle_call(:error_tuple_loader, _, hits) do
    {:reply, {:error, :failed}, hits + 1}
  end

  def handle_call({:slow_identity, time, value}, _, hits) do
    Process.sleep(time)
    {:reply, value, hits + 1}
  end

end

# Since we're relying on StatefulLoader to validate our
# TimedCache behaviors, let's do some sanity checks and test
# that StatefulLoader behaves as expected
defmodule StatefulLoader.Test do
  use ExUnit.Case, async: false
  describe "testing our testing code" do
    setup do
      {:ok, _} = start_supervised(StatefulLoader)
      {:ok, []}
    end
    test "ensure hit count 0 at startup" do
      assert 0 == StatefulLoader.hits()
    end


    test "ensure identity/1 returns its input every time" do
      assert 1 == StatefulLoader.identity(1)
      assert :one == StatefulLoader.identity(:one)
      assert {:one, 1} == StatefulLoader.identity({:one, 1})
    end

    test "ensure hit count is accurately tracked" do
      for call_number <- :lists.seq(1, 10) do
        StatefulLoader.identity(:ok)
        assert StatefulLoader.hits() == call_number
      end
    end
  end
end



