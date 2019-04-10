
defmodule Notifications.Validator.Test do
  use ExUnit.Case
  alias Notifications.Rule
  alias Notifications.Validator

  describe "validate_rule/2" do

    test "returns :error and a message per failed item when a rule is not valid" do
      invalid_rule =  %Rule{action: nil, event: 0, id: "", name: ""}
      # Three validated fields, all of which will fail when using
      # default values in an update request:
      {answer, messages} = Validator.validate_rule(invalid_rule, :update)
      assert answer == :error
      assert length(messages) == 3
    end

    test "returns :ok when the rule is valid" do
      slack_alert = {:SlackAlert, %Notifications.SlackAlert{url: "http://example.com"}}
      valid_rule = %Rule{event: 0, id: "1", name: "jiminy",
                         action: slack_alert}
      result = Validator.validate_rule(valid_rule, :update)
      assert result == :ok
    end
  end

  describe "validate_uri" do
    test "passes if the uri is valid" do
      assert Validator.validate_uri("https://example.com") == :ok
    end
    test "fails if the uri is missing a scheme prefix" do
      assert Validator.validate_uri("example.com") == :error
    end
    test "fails if the uri is missing a host" do
      assert Validator.validate_uri("https://") == :error
    end
    test "fails if the uri is blank" do
      assert Validator.validate_uri("") == :error
    end
    test "fails if the uri is nil" do
      assert Validator.validate_uri(nil) == :error
    end
  end

end

defmodule Notifications.Validator.Rule.Test do
  use ExUnit.Case
  alias Notifications.Validator

  describe "check(_, {:id, type}, _)" do
    test "empty value validating for type == :update fails" do
      messages = Validator.Rule.check([], {:id, :update}, "")
      assert validation_fails(messages)
    end

    test "non-empty value for type == :update passes" do
      messages = Validator.Rule.check([], {:id, :update}, "blah")
      assert validation_passes(messages)
    end

    test "empty value for type == :add passes" do
      messages = Validator.Rule.check([], {:id, :add}, "")
      assert validation_passes(messages)
    end

    test "non-empty value type == :add fails" do
      messages = Validator.Rule.check([], {:id, :add}, "blah")
      assert validation_fails(messages)
    end
  end

  describe "check(_, :name, _)" do
    test "empty value fails" do
      assert validation_fails(Validator.Rule.check([], :name, ""))
    end
    test "non-empty value passes" do
      assert validation_passes(Validator.Rule.check([], :name, "jiminy"))
    end
  end

  describe "check(_, :action, _)" do
    test "missing action fails" do
      assert validation_fails(Validator.Rule.check([], :action, nil))
    end
    test "action w/ bad url fails" do
      action = {:SlackAlert, Notifications.SlackAlert.new(url: "no")}
      assert validation_fails(Validator.Rule.check([], :action, action))
    end
    test "action w/ valid url passes" do
      action = {:SlackAlert, Notifications.SlackAlert.new(url: "https://example.com")}
      assert validation_passes(Validator.Rule.check([], :action, action))
    end
  end

  # An empty error accumulator means the thing being checked has passed.
  def validation_passes([]), do: true
  # Error accumulator with a message in it means failure
  def validation_passes([_|[]]), do: false

  def validation_fails([]), do: false
  def validation_fails([_|[]]), do: true
end

