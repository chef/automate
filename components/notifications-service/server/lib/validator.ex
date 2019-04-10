defmodule Notifications.Validator do
  alias Notifications.Rule
  alias Notifications.Validator
  def validate_rule(%Rule{} = rule, validation_type) do

    errors = []
             |> Validator.Rule.check(:name, rule.name)
             |> Validator.Rule.check(:action, rule.action)
             |> Validator.Rule.check({:id, validation_type}, rule.id)

    case length(errors) do
      0 -> :ok
      _ -> {:error, errors}
    end
  end

  def validate_uri(nil), do: :error
  def validate_uri(uri) do
    uri = URI.parse(uri)
    if uri.scheme == nil || uri.host == nil do
      :error
    else
      :ok
    end
  end
end

# Note that these validations do not include type checks or nil
# checks - protobuf assures that if the message is decoded without error
# the types are correct with appropriate edefault values.
defmodule Notifications.Validator.Rule do
  def check(acc, :name, ""), do: ["Rule name must be supplied."] ++ acc
  def check(acc, :name, _), do: acc
  def check(acc, :action, nil) do
    ["Action must be set."] ++ acc
  end
  def check(acc, :action, {_, %_{url: url}}) do
    case Notifications.Validator.validate_uri(url) do
      :error -> ["A valid action URL must be supplied"] ++ acc
      :ok -> acc
    end
  end
  def check(acc, {:id, :update}, "") do
    ["Rule ID must be included from the rule being modified"] ++ acc
  end
  def check(acc, {:id, :add}, id) when id != "" do
    ["Rule ID may not be included in an add-rule request"] ++ acc
  end
  def check(acc, {:id, _type}, _id), do: acc
end

