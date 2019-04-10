# Don't overwrite the real rule store when we're running tests
System.put_env("RULE_STORE_PATH", "test_rule_store")

# Make sure these things are available at runtime for all tests:
Application.ensure_all_started(:timex)

ExUnit.start([exclude: :pend, no_start: true])

defmodule TestHelper do

  def load_notif(name) do
    path = Path.join(["../testdata", "#{name}.pb"])
    {:ok, raw} = File.read(path)
    notif =  Notifications.Event.decode(raw)
    {_type, event} = notif.event
    event
  end
  # A little helper until we update everything to use 'load_notif'
  # which is more accurately named...
  def load_failure(name), do: load_notif(name)
end
