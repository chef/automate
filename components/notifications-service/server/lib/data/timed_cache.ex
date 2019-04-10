defmodule Notifications.Data.TimedCache do
  @moduledoc "A lightweight cache with timed refreshes per key"
  use GenServer
  require Logger
  # What do I need to track?
  #  each cached data bit:
  #  - the key:
  #   - data
  #   - the settings
  #   - initializing caller pid
  #  each running loader:
  #   - the pid of the loader
  #     -> the key into settings
  #

  defmodule State, do: defstruct [:caches,   # map of cache key -> CacheEntry
                                  :loaders]  # map of loader pid -> cache key
  defmodule CacheEntry, do: defstruct [:waiting_pids, :refresh_interval, :fun, :key,
                                       :args, :data, :loader_pid, :initial_load_complete]


  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    Process.flag(:trap_exit, true)
    {:ok, %State{caches: %{}, loaders: %{}}}
  end

  @doc """
  Kick off cache entry initialization and waits for the data to be
  ready in the caller's context before returning the loaded data.
  If the cache is already initialized, it will not be re-initialized - instead
  the cached data will be returned.
  """
  def start_cache(key, loader_fun, args, refresh_interval) do
    # This call occurs in the context of the caller, so self() will be
    # the caller's PID.  We will notify the caller when the initial cache load
    # is complete.
    cache_entry = %CacheEntry{refresh_interval: refresh_interval,
                              fun:  loader_fun,
                              args: args,
                              key: key,
                              initial_load_complete: false}
    GenServer.call(__MODULE__, {:start_cache, cache_entry})
    # To avoid having the caller need to handle one-off messages,
    # we'll wait for the response here - this effectively blocks the caller
    # (but not this GenServer) on the initial data load:
    Logger.debug fn() -> "#{inspect self()} Waiting for data load to complete for #{inspect key}" end
    receive do
      {:cache_ready, data} -> data
      :cache_load_canceled -> {:error, :cache_deleted}
    end
  end

  def stop_cache(key) do
    GenServer.call(__MODULE__, {:uncache, key})
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  def handle_call({:uncache, key}, _sender, state)  do
    case Map.get(state.caches, key) do
      nil ->
        Logger.info fn() -> "Received request to delete cache #{inspect key} but it is already gone." end
        {:reply, :ok, state}
      entry ->
        Logger.info fn() -> "Deleting cache entry for #{inspect key}" end
        notify_waiting_pids(entry.waiting_pids, :cache_load_canceled)
        new_state = %State{caches: Map.delete(state.caches, key),
                           loaders: Map.delete(state.loaders, entry.loader_pid)}
        {:reply, :ok, new_state}
    end
  end

  # TODO - why is this a sender,ref tupl;e?! wtf!
  def handle_call({:start_cache, cache_entry}, {sender, _ref}, state) do
    new_state = handle_start_cache(Map.get(state.caches, cache_entry.key), cache_entry, sender, state)
    {:reply, :ok, new_state}
  end

  def handle_call({:get, key}, _sender, state)  do
    Logger.debug fn() -> "Attempting to retrieve cached value for #{inspect key}" end
    results = case Map.get(state.caches, key) do
      nil -> {:error, :not_cached}
      cache_entry -> cache_entry.data
    end
    {:reply, results, state}
  end

  # It has not been initialized - let's do that now.
  defp handle_start_cache(nil = _existing, init_entry, sender, state) do
    Logger.info fn() -> "init cache for #{inspect init_entry.key}" end
    newpid = spawn_link(fn() -> populate_cache(init_entry) end)

    new_entry = %{init_entry | waiting_pids: [sender], loader_pid: newpid}
    %State{caches: Map.put(state.caches, init_entry.key, new_entry),
           loaders: Map.put(state.loaders, newpid, init_entry.key)}
  end

  # Initialized and initial load complete. Just send back a ready message with the data
  defp handle_start_cache(%CacheEntry{initial_load_complete: true} = existing_entry, _init_entry, sender, state) do
    send sender, {:cache_ready, existing_entry.data}
    state
  end

  # This item exists and the initial load is still running - we'll just
  # add the sender to the list of those who want to know about it. j
  defp handle_start_cache(%CacheEntry{} = existing_entry, _init_entry, sender, state) do
    Logger.debug fn() -> "Adding to wait list for existing entry: #{inspect sender}" end
    new_waitlist = [sender | existing_entry.waiting_pids]
    new_entry = %CacheEntry{existing_entry | waiting_pids: new_waitlist}
    %State{state | caches: Map.put(state.caches, new_entry.key, new_entry)}
  end

  # Let each pid who was waiting for initial data to arrive know that it's here.
  defp notify_waiting_pids(pid_list, message) do
    for pid <- pid_list, do: send pid, message
  end

  # We receive this when our timer for a given key expires
  def handle_info({:refresh, key}, state) do
    pid = spawn_link(fn() -> populate_cache(Map.get(state.caches, key)) end)
    {:noreply, %State{state | loaders: Map.put(state.loaders, pid, key)}}
  end

  def handle_info({:EXIT, sender, :normal}, state) do
    Logger.info("#{inspect sender} terminating without data, probably it was shut down due to cache deletion")
    {:noreply, state}
  end

  def handle_info({:EXIT, sender, {:normal, {:request_succeeded, data}}}, state) do
    case Map.get(state.loaders, sender) do
      nil -> {:noreply, state}  # cache canceled during run
      key ->
        Logger.debug fn() -> "Data load for #{inspect key} complete." end
        new_loaders = Map.delete(state.loaders, sender)
        cache_entry = Map.get(state.caches, key)
        Process.send_after(__MODULE__, {:refresh, key}, cache_entry.refresh_interval)
        notify_waiting_pids(cache_entry.waiting_pids, {:cache_ready, data})
        new_cache_entry = %{cache_entry | data: data,
                                          initial_load_complete: true,
                                          waiting_pids: []}
        {:noreply, %State{loaders: new_loaders,
                          caches: Map.put(state.caches, key, new_cache_entry)}}
    end
  end

  # This is hit when the loader function completes normally, but the result was an
  # error tuple.  'error' is the complete tuple as received from the loader fun.
  def handle_info({:EXIT, sender, {:normal, {:request_failed, error}}}, state) do
    handle_failure(sender, error, state)
  end

  # This run when the process goes down because of an exception
  # or forced exit.  TODO - may be DOWN
  def handle_info({type, sender, failure}, state) when type in [:DOWN, :EXIT] do
    handle_failure(sender, failure, state)
  end

  defp handle_failure(sender, error, state) do
    case Map.get(state.loaders, sender) do
      nil -> {:noreply, state}  # cache canceled during run
      key ->
        Logger.error("Load for #{inspect key} failed: #{inspect error}")
        new_loaders = Map.delete(state.loaders, sender)
        cache_entry = Map.get(state.caches, key)
        # Set up the next refresh:
        Process.send_after(__MODULE__, {:refresh, key}, cache_entry.refresh_interval)
        if cache_entry.initial_load_complete do
          {:noreply, %State{caches: state.caches, loaders: new_loaders}}
        else
          # Count this as data - otherwise caller who is initiailizing this
          # will wait forever for data (or until the next refresh w/ success result)
          notify_waiting_pids(cache_entry.waiting_pids, {:cache_ready, error})
          new_cache_entry = %{cache_entry | data: error ,
                                            initial_load_complete: true,
                                            waiting_pids: []}
          {:noreply, %State{loaders: new_loaders,
                                 caches: Map.put(state.caches, key, new_cache_entry)}}
        end
    end
  end

  defp populate_cache(nil) do
    Logger.info("Ignoring removed cache refresh request.")
  end

  defp populate_cache(cache_entry) do
    # Any exception in the loader_fun itself will cause this process to
    # terminate abnormally.  The termination message will be caught,
    # and the cache will handle appropriately
    result = case apply(cache_entry.fun, cache_entry.args) do
               {:error, _} = error ->
                 {:request_failed, error}
               data ->
                 {:request_succeeded, data}
             end

    # Use the exit message to send results back to the caller.
    Process.exit(self(), {:normal, result})
  end

end

