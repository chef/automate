defmodule Chef.Automate.Api.Common.Query.SortOrder do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :ASC, 0
  field :DESC, 1
end

defmodule Chef.Automate.Api.Common.Query.Pagination do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :page, 1, type: :int32
  field :size, 2, type: :int32
end

defmodule Chef.Automate.Api.Common.Query.Sorting do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :field, 1, type: :string
  field :order, 2, type: Chef.Automate.Api.Common.Query.SortOrder, enum: true
end

defmodule Chef.Automate.Api.Common.Query.Suggestion do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :type, 1, type: :string
  field :text, 2, type: :string
  field :filter, 3, repeated: true, type: :string
end

defmodule Chef.Automate.Api.Common.Query.Filter do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :key, 20, type: :string
  field :exclude, 22, type: :bool
  field :values, 23, repeated: true, type: :string
end

defmodule Chef.Automate.Api.Common.Query.Kv do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end