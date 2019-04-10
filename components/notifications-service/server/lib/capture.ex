defmodule Notifications.Capture do
  @moduledoc """
  A module that knows how to capture Notification.Event and serialize it for later
  examination.

  If capture is configured, a new process is spawned to serialize the data.
  TODO: We'll want to move this to a genserver. I'd like to add ability to retrieve/reconstitute
        historical captures too.
  """
  require Logger

  @spec process_notification(Notification.notification) :: :ok
  def process_notification(notification) do
    if Notifications.Config.capture_payloads? == :true do
      spawn(fn -> capture_payload(notification) end)
    end
    :ok
  end

  defp capture_payload(%{:__struct__ => type} = notification) do
    blob = Notifications.Event.encode(notification)
    semi_unique_id = :binary.part(UUID.uuid1, {0, 5})
    file_name = "#{type}-#{semi_unique_id}.pb.raw"
    # TODO - I've found no way to get the default
    # config value for the log path out of Logger so far. Ideally
    # we'd use that instead of assuming ./log/ exists and is the correct location.
    path = Path.join("log", file_name)
    Logger.debug fn -> "Capturing #{type} Event payload to #{path}" end
    {:ok, file} = File.open(path, [:write])
    IO.binwrite(file, blob)
    File.close file
    :ok
  end
end
