defmodule Slack.Message.Attachment.Field do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :short, 1, type: :bool
  field :title, 2, type: :string
  field :value, 3, type: :string
end

defmodule Slack.Message.Attachment do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :color, 1, type: :string
  field :fallback, 2, type: :string
  field :pretext, 3, type: :string
  field :text, 4, type: :string
  field :mrkdwn_in, 5, type: :string, json_name: "mrkdwnIn"
  field :fields, 6, repeated: true, type: Slack.Message.Attachment.Field
end

defmodule Slack.Message do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :icon_url, 1, type: :string, json_name: "iconUrl"
  field :text, 2, type: :string
  field :username, 3, type: :string
  field :attachments, 4, repeated: true, type: Slack.Message.Attachment
end