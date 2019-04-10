defmodule Notifications.Formatters.Utils do
  @moduledoc """
  Utils module for commonly reused functions within the notifications namespace
  """

  use Timex

  @spec format_date_string(nil | String.t) :: String.t
  def format_date_string(date_string) when date_string == "" or date_string == nil, do: ""
  def format_date_string(date_string) do
    utc_timezone = Timezone.get("UTC", Timex.now)
    date_string
    |> Timex.parse!("{ISO:Extended:Z}")
    |> Timezone.convert(utc_timezone)
    |> Timex.format!("%FT%T.%06fZ", :strftime)
  end

  @spec truncate_slack_message(nil | String.t) :: String.t | nil
  @doc """
  Truncate a string to 1500 characters and append '...'
  Note that for edge cases (1500-1502 length) this will give us back a
  string slightly longer than the original because of the '...'
  """
  def truncate_slack_message(<<message :: binary-size(1500), _ :: binary>>), do: message <> "..."
  def truncate_slack_message(message), do: message

  def failed_critical_control?(%Notifications.Profile.Control{} = control) do
    critical_control?(control) &&
      Enum.any?(control.failed_results, &failed_test?(&1))
  end

  def critical_control?(%Notifications.Profile.Control{} = control), do: control.impact >= 0.7

  def failed_test?(%{status: "failed"}), do: true
  def failed_test?(_), do: false

  def maybe_markdown_url("", title), do: title
  def maybe_markdown_url(url, title), do: "<#{url}|#{title}>"

  @doc """
  This will take a container (list, struct, or map) and recursively convert
  any struct values it contains into maps while retaining non-structs as-is.

  Elixir provides Map.from_struct, but it will not make the conversion recursively.
  """
  def to_map(%{} = value), do: to_map(:ignore, value)
  def to_map(value) when is_list(value) do
    to_map(:ignore, value)
  end

  # Notes on the internals:
  #    Elixir map comprehension returns a list, not a map. The Enum.map
  #    function accepts maps, but also returns a list.  We'll dip down into
  #    erlang's "map" module: map:map constructs a new map;
  #    and for consistency we'll use the erlang list module for handling lists.
  defp to_map(_, []), do: []
  defp to_map(_, list) when is_list(list), do: :lists.map(&to_map(:ignore, &1), list)
  defp to_map(_, %_struct{} = struct), do:  to_map(:ignore, Map.from_struct(struct))
  defp to_map(_, %{} = map), do: :maps.map(&to_map(&1, &2), map)
  defp to_map(_, other), do: other
end
