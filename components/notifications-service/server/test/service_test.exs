defmodule ServiceTest do
  use ExUnit.Case
  import Mock

  alias Notifications.{Service, Dispatcher, CCRFailure, Event, Rule, Data, 
                      URLValidationRequest, URLValidationResponse, UsernamePassword, 
                      SecretId, Empty}
  alias Notifications.{RuleListResponse, RuleAddResponse, RuleGetResponse,
                      RuleUpdateResponse, RuleDeleteResponse, RuleIdentifier, 
                      WebhookSender, Data.SecretStore}

  describe "notify" do
    test "Unpacks and sends the notification over to Dispatcher" do
      with_mocks([
        {Dispatcher, [], [process_notification: fn(_payload, _id) -> :ok  end]}
      ]) do
        ccr_failure = CCRFailure.new(run_id: "a")
        event = %Event{event: {:CCRFailure, ccr_failure}, id: "someid"}
        %type{} = Service.notify(event, :ignore)
        assert type == Notifications.Response
        assert called Dispatcher.process_notification(ccr_failure, "id:someid")
      end
    end
  end

  describe "version" do
    # TODO Pended because the application behaves differently in test than it does
    # when running - so the version lookup in the test env fails, while on a live instance
    # it does the right thing.
    @tag :pend
    test "it returns the correct release version" do
      version = String.trim(File.read!("../VERSION"))
      response = Service.version(VersionRequest.new, :ignore)
      assert version == response.version
    end
  end

  describe "list_rules" do
    test "that when rules exists they are returned" do
      with_mocks([
        {Data.Store, [], [get_rules: fn() -> {:ok, [rule1(), rule2_slack()]} end]},
      ]) do
        response = Service.list_rules(:ignore, :ignore)
        expected = RuleListResponse.new(code: 0, messages: [],
                                        rules: [rule1(), rule2_slack()])

        assert response ==  expected
      end
    end

    test "that when no rules exist, the response includes an empty list of rules" do
      with_mocks([
        {Data.Store, [], [get_rules: fn() -> {:ok, []} end]},
      ]) do
        response = Service.list_rules(:ignore, :ignore)
        assert response == RuleListResponse.new(code: 0, messages: [], rules: [])
      end
    end
  end

  describe "get_rule" do
    test "replies with not found error when the requested rule is not found" do
      with_mocks([
        {Data.Store, [], [get_rule: fn(_) -> {:ok, []} end]},
      ]) do
        rule_request = %RuleIdentifier{id: "rule1"}
        response = Service.get_rule(rule_request, :ignore)
        assert response.code == RuleGetResponse.Code.value(:NOT_FOUND)
        assert length(response.messages) == 1
      end
    end

    test "replies with the rule when the rule exists" do
      with_mocks([
        {Data.Store, [], [get_rule: fn(%{id: "rule2"}) -> {:ok, [rule2_slack()]} end ]},
      ]) do
        rule_request = %RuleIdentifier{id: "rule2"}
        response = Service.get_rule(rule_request,  :ignore)
        expected = %RuleGetResponse{code: 0, messages: [],
                                    rule: rule2_slack()}
        assert response == expected
      end
    end
  end

  describe "delete_rule" do
    test "replies with not found error if the rule to be deleted doesn't exist" do
      with_mocks([
        {Data.Store, [], [delete_rule: fn(%{id: "rule1"}) -> :not_found end ]}
      ]) do
        rule_request = %RuleIdentifier{id: "rule1"}
        response = Service.delete_rule(rule_request, :ignore)
        assert response.code == RuleAddResponse.Code.value(:NOT_FOUND)
        assert length(response.messages) == 1
      end
    end

    test "deletes and replies with successful response when an existing rule is deleted" do
      with_mocks([
        {Data.Store, [], [delete_rule: fn(%{id: "rule1_ID"}) -> :ok end]},
      ]) do
        rule_request = %RuleIdentifier{id: "rule1_ID"}
        response = Service.delete_rule(rule_request,  :ignore)
        expected = %RuleDeleteResponse{code: 0, messages: []}
        assert response == expected
      end
    end
  end

  describe "add_rule" do
    test "that it saves a valid rule and replies correctly" do
      with_mocks([
        {Data.Store, [], [add_rule: fn(_) -> {:ok, "abc123"} end]},
      ]) do
        rule_request = %{rule1() | id: ""}
        response = Service.add_rule(rule_request,  :ignore)
        assert response.code == 0
        assert response.messages == []
        assert response.id == "abc123"
      end
    end

    test "that it rejects the rule with duplicate name if a rule with the proposed name already exists" do
      with_mocks([
        {Data.Store, [], [add_rule: fn(_) -> {:error, :conflict} end]},
      ]) do
        rule_request = %{rule1() | id: ""}
        response = Service.add_rule(rule_request,  :ignore)
        assert response.code == RuleAddResponse.Code.value(:DUPLICATE_NAME)
        assert length(response.messages) == 1
      end
    end


    test "that it rejects the rule with validation errors if the rule is invalid" do
      response = Service.add_rule(Rule.new(),  :ignore)
      assert response.code == RuleAddResponse.Code.value(:VALIDATION_ERROR)
      # The specific validations are covered in Validator.Rule.Test
      assert length(response.messages) > 0
    end
  end

  describe "update_rule" do
    test "replies correctly when a valid update is processed" do
      with_mocks([
        {Data.Store, [], [update_rule: fn(%Rule{id: "rule1_ID"}) -> :ok end]}
      ]) do
        response = Service.update_rule(rule1(),  :ignore)
        expected = RuleUpdateResponse.new()
        assert response == expected
      end
    end

    test "that it rejects the rule with duplicate name if a rule with that name exists" do
      update_rule_mock = fn(%Rule{id: "rule3_webhook_ID", name: "duplicate-name"}) ->
                           {:error, :conflict}
                         end
      with_mocks([
        {Data.Store, [], [update_rule: update_rule_mock]},
      ]) do
        update_request = %{rule3_webhook() | name: "duplicate-name"}
        response = Service.update_rule(update_request,  :ignore)
        assert response.code == RuleUpdateResponse.Code.value(:DUPLICATE_NAME)
        assert length(response.messages) == 1
      end
    end

    test "that it rejects the rule with validation errors if the rule is invalid" do
      update_request = Rule.new()
      response = Service.update_rule(update_request,  :ignore)
      assert response.code == RuleUpdateResponse.Code.value(:VALIDATION_ERROR)
      assert length(response.messages) > 0
    end
  end

  describe "validate_webhook" do
    test "with UsernamePassword case" do
      with_mocks([
        {WebhookSender, [], [validate_webhook: fn(_url, _username, _password) -> {:ok, {200, ""}} end]}
      ]) do
        username = "bob"
        url = "url"
        password = "super_secret"
        request = %URLValidationRequest{url: url, credentials: {:username_password, 
          %UsernamePassword{username: username, password: password}}}
        response = Service.validate_webhook(request, :ignore)
        expected = %URLValidationResponse{code: 0, messages: []}
        assert response == expected
        assert called WebhookSender.validate_webhook(url, username, password)
      end
    end

    test "with SecretId case" do
      username = "bob"
      url = "url"
      password = "super_secret"
      secret_id = "super_secret_id"
      with_mocks([
        {WebhookSender, [], [validate_webhook: fn(_url, _username, _password) -> {:ok, {200, ""}} end]},
        {SecretStore, [], [get_target_username_password: fn(_secret_id) -> {:ok, {username, password}} end]}
      ]) do
        request = %URLValidationRequest{url: url, credentials: {:secret_id, %SecretId{id: secret_id}}}
        response = Service.validate_webhook(request, :ignore)
        expected = %URLValidationResponse{code: 0, messages: []}
        assert response == expected
        assert called SecretStore.get_target_username_password(secret_id)
        assert called WebhookSender.validate_webhook(url, username, password)
      end
    end

    test "with Empty case" do
      with_mocks([
        {WebhookSender, [], [validate_webhook: fn(_url, _username, _password) -> {:ok, {200, ""}} end]}
      ]) do
        url = "url"
        request = %URLValidationRequest{url: url, credentials: {:none, %Empty{}}}
        response = Service.validate_webhook(request, :ignore)
        expected = %URLValidationResponse{code: 0, messages: []}
        assert response == expected
        assert called WebhookSender.validate_webhook(url, "", "")
      end
    end
  end

  defp rule1 do
    %Rule{action: {:SlackAlert, %Notifications.SlackAlert{url: "http://foo.bar:20/baz"}},
          event: 0,
          name: "rule1", id: "rule1_ID"}
  end

  defp rule2_slack do
    %Rule{action: {:SlackAlert, %Notifications.SlackAlert{url: "http://foo.bar:20/baz"}},
          event: 0,
      name: "rule2_slack", id: "rule2_slack_ID"}
  end

  defp rule3_webhook do
    %Rule{action: {:WebhookAlert, %Notifications.WebhookAlert{url: "http://foo.bar:20/baz"}},
          event: 0,
          name: "rule3_webhook", id: "rule3_webhook_ID"}
  end
end
