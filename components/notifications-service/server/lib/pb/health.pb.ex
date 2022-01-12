defmodule Notifications.VersionRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3
end
defmodule Notifications.VersionResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field :version, 1, type: :string
end
