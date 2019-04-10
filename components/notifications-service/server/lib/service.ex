defmodule Notifications.Service do
  @moduledoc """
  This handles inbound GRPC requests to publish notifications.
  """
  require Logger
  use GRPC.Server, service: Notifications.Notifications.Service
  # protobuf modules
  alias Notifications.{URLValidationRequest, UsernamePassword, SecretId, Empty, URLValidationResponse, RuleGetResponse, RuleAddResponse}
  alias Notifications.{RuleDeleteResponse, RuleListResponse,  RuleUpdateResponse, Rule, RuleIdentifier}
  alias Notifications.{WebhookSender, Validator, Data.Store, Data.SecretStore}


  @spec notify(Notifications.Event.t, GRPC.Server.Stream.t) :: Notifications.Response
  def notify(%Notifications.Event{:id => id} = event, _stream) do
    # TODO - better and more consistent ID handling, let's put it in a header instead
    # of in the request itself if we can

    # Thread this formatted request ID in here, so that it's easy to find by searching
    # logs for "id:"
    id_for_log = "id:#{id}"
    Logger.metadata([request_id: id]) # TODO - this only works if we thread it through.
    Logger.debug fn() -> "Received inbound event #{id_for_log}" end
    {_int_field_name, notification} = event.event
    Notifications.Dispatcher.process_notification(notification, id_for_log)
    # The reply expected from this GRPC endpoint. Currently
    # no additional information is needed by callers, but it's here if we need
    # to expand it...
    Notifications.Response.new()
  end

  @spec version(Notifications.VersionRequest.t, GRPC.Server.Stream.t) :: Notifications.VersionResponse
  def version(_empty, _stream) do
    version = Application.spec(:notifications, :vsn)
              |> to_string
              |> String.trim
    Notifications.VersionResponse.new(version: version)
  end

  @spec validate_webhook(URLValidationRequest.t, GRPC.Server.Stream.t) :: Notifications.URLValidationResponse
  def validate_webhook(%URLValidationRequest{} = req, _stream)  do

    {username, password} = case req.credentials do
      {:secret_id, %SecretId{id: secret_id}} ->
        get_target_username_password(secret_id)
      {:username_password, %UsernamePassword{username: up_username, password: up_password}} ->
        {up_username, up_password}
      {:none, %Empty{}} -> {"", ""}
    end

    response = WebhookSender.validate_webhook(req.url, username, password)
    {code, messages} = case response do
                         {:ok, _detail} -> {:OK, []}
                         {:error, :invalid_url} ->
                           {:INVALID_URL, ["The provided URL is not valid."]}
                         {:error, :no_connections} ->
                           {:NOTIFICATIONS_UNAVAILABLE,
                             ["Notifications service temporarily unavailable"]}
                         {:error, _detail} ->
                           {:ERROR, ["Posting to the provided webhook failed"]}
                      end
    URLValidationResponse.new(code: enum_to_value_for_type(code, URLValidationResponse),
                              messages: messages)
    end

  @spec list_rules(Notifications.Empty.t, GRPC.Server.Stream.t) :: Notifications.RuleListResponse
  def list_rules(_empty, _stream) do
    Logger.debug("Fetching all rules")
    case Store.get_rules() do
      {:ok, rules}  -> RuleListResponse.new(rules: rules)
      {:error, other} -> response_for(:internal_error, RuleListResponse, other)
    end
  end

  @spec add_rule(Notifications.Rule, GRPC.Server.Stream.t) :: Notifications.RuleAddResponse
  def add_rule(%Notifications.Rule{} = rule, _stream) do
    Logger.info fn() -> "Adding new rule #{rule.name} #{rule.event}" end
    case Validator.validate_rule(rule, :add) do
      {:error, reasons} ->
        response_for(:validation_error, RuleAddResponse, reasons)
      :ok ->
        case Store.add_rule(rule) do
          {:ok, new_rule_id} -> RuleAddResponse.new(id: new_rule_id)
          {:error, :conflict} -> response_for(:duplicate, RuleAddResponse, rule)
          {:error, other} -> response_for(:internal_error, RuleAddResponse, other)
        end
    end
  end

  @spec delete_rule(RuleIdentifier.t, GRPC.Server.Stream.t) :: Notifications.RuleDeleteResponse
  def delete_rule(%RuleIdentifier{id: id} = ident, _stream) do
    # TODO - anything that accepts rule identifier must validate that an identifier is populatd.
    Logger.debug fn() -> "Deleting rule #{id}" end
    case Store.delete_rule(ident) do
      :ok -> RuleDeleteResponse.new()
      :not_found -> response_for(:not_found, RuleDeleteResponse, id)
      {:error, other} -> response_for(:internal_error, RuleDeleteResponse, other)
    end
  end

  @spec get_rule(RuleIdentifier.t, GRPC.Server.Stream.t) :: Notifications.RuleGetResponse
  def get_rule(%RuleIdentifier{id: id} = ident, _stream) do
    Logger.debug fn() -> "Fetching rule #{id}" end
    case Store.get_rule(ident) do
      {:ok, []}  -> response_for(:not_found, RuleGetResponse, id)
      {:ok, [rule]}  -> RuleGetResponse.new(rule: rule)
      {:error, other} -> response_for(:internal_error, RuleGetResponse, other)
    end
  end

  @spec update_rule(Rule.t, GRPC.Server.Stream.t) :: RuleUpdateResponse
  def update_rule(%Rule{id: id} = rule, _stream) do
    Logger.debug fn() -> "Updating rule #{id}" end
    case Validator.validate_rule(rule, :update) do
      :ok ->
        case Store.update_rule(rule)  do
          :ok -> RuleUpdateResponse.new()
          :not_found -> response_for(:not_found, RuleUpdateResponse, id)
          {:error, :conflict} -> response_for(:duplicate, RuleUpdateResponse, rule)
          {:error, other} -> response_for(:internal_error, RuleUpdateResponse, other)
        end
      {:error, reasons} ->
        response_for(:validation_error, RuleUpdateResponse, reasons)
    end
  end

  defp response_for(:internal_error, type_mod, error_detail) do
    Logger.error("An error occurred: #{inspect error_detail}")
    mk_response(type_mod, :INTERNAL_ERROR,
                ["An internal notifications service error has occurred"])
  end
  defp response_for(:not_found, type_mod, id) do
    Logger.error("Rule #{id} not found")
    mk_response(type_mod, :NOT_FOUND, ["The requested rule could not be found"])
  end
  defp response_for(:duplicate, type_mod, rule) do
    Logger.error("Operation failed because rule name '#{rule.name}' or rule id '#{rule.id}' already exists")
    mk_response(type_mod, :DUPLICATE_NAME, ["A rule with this name already exists"])
  end
  defp response_for(:validation_error, type_mod, errors) do
    mk_response(type_mod, :VALIDATION_ERROR, errors)
  end

  # These two functions rely on our commont Response message structures
  # for the Rules API. Each has a message code (Code enum) and a 'messages' repeating string.
  defp mk_response(type_mod, code, messages) do
    type_mod.new(code: enum_to_value_for_type(code, type_mod), messages: messages)
  end

  defp enum_to_value_for_type(code, type_mod) do
    Module.concat(type_mod, Code).value(code)
  end

  defp get_target_username_password(secret_id) do
    case SecretStore.get_target_username_password(secret_id) do
      {:ok, {username, password}} -> {username, password}
      {:error, _} ->
        Logger.error fn() -> "error retrieving target username and password for secret_id: #{secret_id}" end
        {"", ""}
    end
  end
end
