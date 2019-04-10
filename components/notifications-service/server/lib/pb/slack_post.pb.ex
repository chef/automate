defmodule Slack.Message do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          icon_url: String.t(),
          text: String.t(),
          username: String.t(),
          attachments: [Slack.Message.Attachment.t()]
        }
  defstruct [:icon_url, :text, :username, :attachments]

  field :icon_url, 1, type: :string
  field :text, 2, type: :string
  field :username, 3, type: :string
  field :attachments, 4, repeated: true, type: Slack.Message.Attachment
end

defmodule Slack.Message.Attachment do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          color: String.t(),
          fallback: String.t(),
          pretext: String.t(),
          text: String.t(),
          mrkdwn_in: String.t(),
          fields: [Slack.Message.Attachment.Field.t()]
        }
  defstruct [:color, :fallback, :pretext, :text, :mrkdwn_in, :fields]

  field :color, 1, type: :string
  field :fallback, 2, type: :string
  field :pretext, 3, type: :string
  field :text, 4, type: :string
  field :mrkdwn_in, 5, type: :string
  field :fields, 6, repeated: true, type: Slack.Message.Attachment.Field
end

defmodule Slack.Message.Attachment.Field do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          short: boolean,
          title: String.t(),
          value: String.t()
        }
  defstruct [:short, :title, :value]

  field :short, 1, type: :bool
  field :title, 2, type: :string
  field :value, 3, type: :string
end
