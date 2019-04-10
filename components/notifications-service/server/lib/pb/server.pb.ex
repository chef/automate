defmodule Notifications.Notifications.Service do
  @moduledoc false
  use GRPC.Service, name: "notifications.Notifications"

  rpc :Notify, Notifications.Event, Notifications.Response
  rpc :AddRule, Notifications.Rule, Notifications.RuleAddResponse
  rpc :DeleteRule, Notifications.RuleIdentifier, Notifications.RuleDeleteResponse
  rpc :UpdateRule, Notifications.Rule, Notifications.RuleUpdateResponse
  rpc :GetRule, Notifications.RuleIdentifier, Notifications.RuleGetResponse
  rpc :ListRules, Notifications.Empty, Notifications.RuleListResponse
  rpc :ValidateWebhook, Notifications.URLValidationRequest, Notifications.URLValidationResponse
  rpc :Version, Notifications.VersionRequest, Notifications.VersionResponse
end

defmodule Notifications.Notifications.Stub do
  @moduledoc false
  use GRPC.Stub, service: Notifications.Notifications.Service
end
