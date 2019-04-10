defmodule Notifications.Config.Test do
  use ExUnit.Case
  alias Notifications.Config
  @env_var "NOTIF_CONFIG_TEST"
  setup do
    System.delete_env(@env_var)
  end
  test "#get/2 returns the value of the env var when present" do
    System.put_env(@env_var, "hello")
    assert Config.get(@env_var) == {:ok, "hello"}
  end
  test "#get/2 returns the default value when the env var is not  present" do
    assert Config.get(@env_var, "goodbye") == {:ok, "goodbye"}
  end
  test "#get/2 returns an error tuple when env var is missing and no default provided" do
    assert Config.get(@env_var) == {:error, {:missing_env_var, @env_var}}
  end
end
