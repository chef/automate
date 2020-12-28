defmodule Notifications.Rule.Event do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :CCRFailure | :CCRSuccess | :ComplianceFailure | :ComplianceSuccess

  field :CCRFailure, 0
  field :CCRSuccess, 1
  field :ComplianceFailure, 2
  field :ComplianceSuccess, 3
end

defmodule Notifications.URLValidationResponse.Code do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :OK | :ERROR | :INVALID_URL | :NOTIFICATIONS_UNAVAIALBLE | :INTERNAL_ERROR

  field :OK, 0
  field :ERROR, 1
  field :INVALID_URL, 4
  field :NOTIFICATIONS_UNAVAIALBLE, 98
  field :INTERNAL_ERROR, 99
end

defmodule Notifications.RuleUpdateResponse.Code do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :OK | :DUPLICATE_NAME | :NOT_FOUND | :VALIDATION_ERROR | :INTERNAL_ERROR

  field :OK, 0
  field :DUPLICATE_NAME, 1
  field :NOT_FOUND, 2
  field :VALIDATION_ERROR, 4
  field :INTERNAL_ERROR, 99
end

defmodule Notifications.RuleDeleteResponse.Code do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :DELETED | :NOT_FOUND | :INTERNAL_ERROR

  field :DELETED, 0
  field :NOT_FOUND, 2
  field :INTERNAL_ERROR, 99
end

defmodule Notifications.RuleAddResponse.Code do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t ::
          integer
          | :ADDED
          | :DUPLICATE_NAME
          | :NOT_FOUND
          | :INVALID_ACTION_CONFIG
          | :VALIDATION_ERROR
          | :INTERNAL_ERROR

  field :ADDED, 0
  field :DUPLICATE_NAME, 1
  field :NOT_FOUND, 2
  field :INVALID_ACTION_CONFIG, 3
  field :VALIDATION_ERROR, 4
  field :INTERNAL_ERROR, 99
end

defmodule Notifications.RuleGetResponse.Code do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :OK | :NOT_FOUND | :INTERNAL_ERROR

  field :OK, 0
  field :NOT_FOUND, 2
  field :INTERNAL_ERROR, 99
end

defmodule Notifications.RuleListResponse.Code do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :OK | :INTERNAL_ERROR

  field :OK, 0
  field :INTERNAL_ERROR, 99
end

defmodule Notifications.Empty do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Notifications.SlackAlert do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          url: String.t()
        }
  defstruct [:url]

  field :url, 1, type: :string
end

defmodule Notifications.WebhookAlert do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          url: String.t()
        }
  defstruct [:url]

  field :url, 1, type: :string
end

defmodule Notifications.ServiceNowAlert do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          url: String.t(),
          secret_id: String.t(),
          critical_controls_only: boolean
        }
  defstruct [:url, :secret_id, :critical_controls_only]

  field :url, 1, type: :string
  field :secret_id, 2, type: :string, json_name: "secretId"
  field :critical_controls_only, 3, type: :bool, json_name: "criticalControlsOnly"
end

defmodule Notifications.Rule do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          action: {atom, any},
          id: String.t(),
          name: String.t(),
          event: Notifications.Rule.Event.t()
        }
  defstruct [:action, :id, :name, :event]

  oneof :action, 0

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :event, 3, type: Notifications.Rule.Event, enum: true
  field :SlackAlert, 4, type: Notifications.SlackAlert, oneof: 0
  field :WebhookAlert, 5, type: Notifications.WebhookAlert, oneof: 0
  field :ServiceNowAlert, 6, type: Notifications.ServiceNowAlert, oneof: 0
end

defmodule Notifications.UsernamePassword do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          username: String.t(),
          password: String.t()
        }
  defstruct [:username, :password]

  field :username, 1, type: :string
  field :password, 2, type: :string
end

defmodule Notifications.SecretId do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t()
        }
  defstruct [:id]

  field :id, 1, type: :string
end

defmodule Notifications.URLValidationRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          credentials: {atom, any},
          url: String.t()
        }
  defstruct [:credentials, :url]

  oneof :credentials, 0

  field :url, 1, type: :string

  field :username_password, 2,
    type: Notifications.UsernamePassword,
    json_name: "usernamePassword",
    oneof: 0

  field :secret_id, 3, type: Notifications.SecretId, json_name: "secretId", oneof: 0
  field :none, 4, type: Notifications.Empty, oneof: 0
end

defmodule Notifications.URLValidationResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: Notifications.URLValidationResponse.Code.t(),
          messages: [String.t()]
        }
  defstruct [:code, :messages]

  field :code, 1, type: Notifications.URLValidationResponse.Code, enum: true
  field :messages, 2, repeated: true, type: :string
end

defmodule Notifications.RuleIdentifier do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t()
        }
  defstruct [:id]

  field :id, 1, type: :string
end

defmodule Notifications.RuleUpdateResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: Notifications.RuleUpdateResponse.Code.t(),
          messages: [String.t()]
        }
  defstruct [:code, :messages]

  field :code, 1, type: Notifications.RuleUpdateResponse.Code, enum: true
  field :messages, 2, repeated: true, type: :string
end

defmodule Notifications.RuleDeleteResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: Notifications.RuleDeleteResponse.Code.t(),
          messages: [String.t()]
        }
  defstruct [:code, :messages]

  field :code, 1, type: Notifications.RuleDeleteResponse.Code, enum: true
  field :messages, 2, repeated: true, type: :string
end

defmodule Notifications.RuleAddResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: Notifications.RuleAddResponse.Code.t(),
          messages: [String.t()],
          id: String.t(),
          rule: Notifications.Rule.t() | nil
        }
  defstruct [:code, :messages, :id, :rule]

  field :code, 1, type: Notifications.RuleAddResponse.Code, enum: true
  field :messages, 2, repeated: true, type: :string
  field :id, 3, type: :string
  field :rule, 4, type: Notifications.Rule
end

defmodule Notifications.RuleGetResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: Notifications.RuleGetResponse.Code.t(),
          messages: [String.t()],
          rule: Notifications.Rule.t() | nil
        }
  defstruct [:code, :messages, :rule]

  field :code, 1, type: Notifications.RuleGetResponse.Code, enum: true
  field :messages, 2, repeated: true, type: :string
  field :rule, 3, type: Notifications.Rule
end

defmodule Notifications.RuleListResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: Notifications.RuleListResponse.Code.t(),
          messages: [String.t()],
          rules: [Notifications.Rule.t()]
        }
  defstruct [:code, :messages, :rules]

  field :code, 1, type: Notifications.RuleListResponse.Code, enum: true
  field :messages, 2, repeated: true, type: :string
  field :rules, 3, repeated: true, type: Notifications.Rule
end
