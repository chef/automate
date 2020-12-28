defmodule Chef.Automate.Api.Common.Query.SortOrder do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :ASC | :DESC

  field :ASC, 0
  field :DESC, 1
end

defmodule Chef.Automate.Api.Common.Query.Pagination do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          page: integer,
          size: integer
        }
  defstruct [:page, :size]

  field :page, 1, type: :int32
  field :size, 2, type: :int32
end

defmodule Chef.Automate.Api.Common.Query.Sorting do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          field: String.t(),
          order: Chef.Automate.Api.Common.Query.SortOrder.t()
        }
  defstruct [:field, :order]

  field :field, 1, type: :string
  field :order, 2, type: Chef.Automate.Api.Common.Query.SortOrder, enum: true
end

defmodule Chef.Automate.Api.Common.Query.Suggestion do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          type: String.t(),
          text: String.t(),
          filter: [String.t()]
        }
  defstruct [:type, :text, :filter]

  field :type, 1, type: :string
  field :text, 2, type: :string
  field :filter, 3, repeated: true, type: :string
end

defmodule Chef.Automate.Api.Common.Query.Filter do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          key: String.t(),
          exclude: boolean,
          values: [String.t()]
        }
  defstruct [:key, :exclude, :values]

  field :key, 20, type: :string
  field :exclude, 22, type: :bool
  field :values, 23, repeated: true, type: :string
end

defmodule Chef.Automate.Api.Common.Query.Kv do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          key: String.t(),
          value: String.t()
        }
  defstruct [:key, :value]

  field :key, 1, type: :string
  field :value, 2, type: :string
end
