defmodule Notifications.WebhookSender do
  @moduledoc """

  This module provides fire-and-forget bounded asynchronous HTTP POST sending via post/5
  and synchronous webhook verfication function via validate_webhook/3

  When all connections are in use, an error is logged and the request is dropped.

  The maximum number of connections can be found in Notifications.Config.http_pool_config/0,
  and is shared with the initial pool configuraton at startup (see: Application.start/2)
  """

  require Logger

  use GenServer
  @module __MODULE__

  # TODO - if we expand to new types of alerts (email, etc)
  #        let's generalize this GenServer into something like
  #        BoundedTaskRunner that accepts max runner count and the
  #        MF to be run
  def init(_) do
    # Ensure that anything we spawn causes us to get notified
    # when it terminates.
    Process.flag(:trap_exit, true)
    max = Keyword.get(Notifications.Config.http_pool_config(), :max_connections)
    {:ok, %{in_use: 0, max: max, serviced: 0, dropped: 0}}
  end

  def start_link(_) do
    GenServer.start_link(@module, [], [name: @module])
  end

  @doc """
  Post the payload to the target URL asynchronously.  If all available
  HTTP pool connections are in use, the request is discarded and an
  error is logged.  Errors are not reported back to the caller.

  The call itself is synchronous, but it will spawn a worker to execute the request
  if connections are available.

  This is intended for fire-and-forget usage.
  """
  def post(url, payload, id, user, password) do
    send_if_valid_url(url, {:post, {url, payload, id, user, password}})
  end

  @doc """
  Post synchronously, returning the result or :error, no_connections if no connections
  are available
  """
  def validate_webhook(url, user, password) do
    send_if_valid_url(url, {:validate_webhook_target, url, user, password})
  end

  @doc """
  Returns a map of the following stats, gathered since the last start/restart of the webhooksender:

  in_use: The number of connections currently in use by in-flight requests
  max: The number of connections that can be in use before requests are dropped
  serviced: the number of requests that were successfully spawned
  dropped: the number of requests that were not spawned because all connections were in use.
  """
  def stats(), do: GenServer.call(@module, :stats)

  ### Callbacks

  # asynchronous request
  def handle_call({:post, args}, _sender, state) do
    {result, new_state} = maybe_spawn(args, state)
    {:reply, result, new_state}
  end

  # Handler for synchronous validation requests.
  # Note: this does not change the in_use count while it runs because it's a syncronous request -
  # when the call complets, the conn is s no longer in use.
  # Race condition should not be concerned because this request won't start unless there's
  # an available connection - and new async requests will wait in this
  # proc's mailbox until this message is processed.
  def handle_call({:validate_webhook_target, url, user, password}, _sender, state) do
    {result, new_state} = maybe_post({url, validation_payload(), user, password}, state)
    {:reply, result, new_state}
  end

  def handle_call(:stats, _sender, state) do
    {:reply, {:ok, state}, state}
  end

  # Update worker accounting when a worker exits
  def handle_info({:DOWN, _ref, _proc, pid, _reason}, %{in_use: count} = state) do
    Logger.debug fn -> "Worker #{inspect pid} terminated" end
    {:noreply, %{state | in_use: count - 1}}
  end
  def handle_info(msg, state) do
    Logger.debug fn -> "Unexpected message: #{inspect msg}" end
    {:noreply, state}
  end

  ### Internal

  defp send_if_valid_url(url, message) do
    if (Notifications.Validator.validate_uri(url) == :error) do
      {:error, :invalid_url}
    else
      GenServer.call(@module, message)
    end
  end

  # Spawns new HTTP post request if workers are available.
  # Returns an updated state reflecting current in_use, dropped, and serviced counts.
  defp maybe_spawn({_, _, id, _, _}, %{in_use: in_use, max: max, dropped: dropped} = state) when in_use >= max do
    Logger.error fn -> "Request #{id} could not be started: all connections in use" end
    {{:error, :no_connections}, %{state | dropped: dropped + 1}}
  end
  defp maybe_spawn({url, payload, id, user, password}, %{in_use: in_use, serviced: serviced} = state) do
    {pid, _} = spawn_monitor(Notifications.WebhookSender.Impl, :post, [url, payload, id, user, password])
    Logger.debug fn -> "Spawned #{inspect pid} to post outbound alert from #{id}" end
    {:ok, %{state | serviced: serviced + 1, in_use: in_use + 1}}
  end

  # Performs a synchrnous HTTP POST of the provided payload if workers are available.
  defp maybe_post({url, _, _, _}, %{in_use: in_use, max: max, dropped: dropped} = state) when in_use >= max do
    Logger.error fn -> "Synchronous request to #{url} could not be sent: all connections in use" end
    {{:error, :no_connections}, %{state | dropped: dropped + 1}}
  end
  defp maybe_post({url, payload, user, password}, %{serviced: serviced} = state) do
    response = Notifications.WebhookSender.Impl.post(url, payload, "no-id", user, password)
    {response, %{state | serviced: serviced + 1}}
  end

  defp validation_payload(), do: %{text: "TEST: Successful validation completed by Automate"}

end

defmodule Notifications.WebhookSender.Impl do
  require Logger

  @spec post(String.t, %{}, String.t, String.t, String.t) :: {:ok, {String.t, String.t}} | {:error, {:unknown | String.t}}
  def post(url, payload, req_id, user, password) do
    # TODO - include X-RequestId so we can get full tracing event to external systems?
    headers = [{"Content-Type", "application/json"}]
    options = build_options(url) ++ build_basic_auth_option(user, password)
    body = Poison.encode!(payload)
    Logger.info fn -> "POSTing to #{url} for id:#{req_id}" end
    result = HTTPoison.post(url, body, headers, options)
    capture_result(req_id, result)
  end

  defp capture_result(req_id, {:ok,  %HTTPoison.Response{:status_code => code,
                                                         :body => body}}) when code >= 200 and code <= 204 do
    Logger.debug fn -> "Alert for #{req_id} posted successfully" end
    {:ok, {code, body}}
  end

  defp capture_result(req_id, {:ok,  %HTTPoison.Response{:status_code => code} = response}) do
    body = case String.length(response.body) do
      len when len < 120 -> response.body
      _ -> "(suppressed - too long)"
    end
    Logger.error fn -> "Request #{req_id} failed to post. Code #{code}. Body: #{body} " end
    {:error, {code, body}}
  end

  defp capture_result(req_id, {:error,  %HTTPoison.Error{} = response}) do
    Logger.error fn -> "Request #{req_id} was not sent due to: #{inspect response.reason}" end
    {:error, {:unknown, response.reason}}
  end

  defp capture_result(req_id, {:error, reason}) do
    Logger.error fn -> "Request #{req_id} was not sent due to: #{inspect reason}" end
    {:error, {:unknown, reason}}
  end

  defp build_options(url) do
    # YOLO: Adding SSL flags to allow POSTs through a proxy based on advice from
    # https://github.com/edgurgel/httpoison/issues/164 and
    # http://ananthakumaran.in/2017/01/26/debugging.html
    host_name = hostname(url)
    proxy_options(url) ++ [
      ssl: [server_name_indication: to_charlist(host_name)],
      follow_redirect: true,
      pool: :webhook
    ]
  end

  defp build_basic_auth_option(user, password) do
    case user do
      "" -> []
      _ -> [hackney: [basic_auth: {user, password}]]
    end
  end

  @spec proxy_options(url :: String.t | nil) :: keyword({String.t, String.t | integer})
  defp proxy_options(url) do
    proxy = Notifications.Config.proxy_settings
    host_port = case {proxy.host, proxy.port_int, proxyable_url(url, proxy.no_proxy)} do
      # No proxy host - ignore everything else.
      {"", _, _} -> []
      # Invalid url host or matches url no_proxy blacklist.
      {_, _, false} -> []
      # Invalid proxy port provided.
      {_, :error, true} -> [proxy: {proxy.host, 80}]
      {_, {0, _}, true} -> [proxy: {proxy.host, 80}]
      # Valid proxy host and port available.
      {_, {port, _}, true} -> [proxy: {proxy.host, port}]
    end

    case {host_port, proxy.user} do
      # No proxy setup above.
      {[], _} -> []
      # No extra proxy authentication information.
      {_, ""} -> host_port
      # Username given - don't examine the password and just pass it along.
      {_, _} -> host_port ++ [proxy_auth: {proxy.user, proxy.password}]
    end
  end

  @spec proxyable_url(url :: String.t | nil, no_proxy :: String.t) :: boolean
  defp proxyable_url(url, no_proxy) do
    # We split on , which is theoretically safe - RFC1123 says a hostname
    # can't contain commas.
    no_proxy_hosts = String.split(no_proxy || "", ",")
    case hostname(url) do
      # Bad URL
      "" -> false
      # Is it in the proxy white list?
      url_host -> not Enum.member?(no_proxy_hosts, String.downcase(url_host))
    end
  end

  @spec hostname(url :: String.t | nil) :: String.t
  defp hostname(nil), do: ""
  defp hostname(""), do: ""
  defp hostname(url) do
    case URI.parse(url).host do
      nil -> ""
      host -> host
    end
  end
end
