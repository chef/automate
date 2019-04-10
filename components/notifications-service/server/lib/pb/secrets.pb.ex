defmodule Chef.Automate.Api.Secrets.UpdateResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Chef.Automate.Api.Secrets.DeleteResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Chef.Automate.Api.Secrets.Id do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t()
        }
  defstruct [:id]

  field :id, 1, type: :string
end

defmodule Chef.Automate.Api.Secrets.Query do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          filters: [Chef.Automate.Api.Secrets.Filter.t()],
          order: atom | integer,
          sort: String.t(),
          page: integer,
          per_page: integer
        }
  defstruct [:filters, :order, :sort, :page, :per_page]

  field :filters, 20, repeated: true, type: Chef.Automate.Api.Secrets.Filter
  field :order, 21, type: Chef.Automate.Api.Secrets.Query.OrderType, enum: true
  field :sort, 22, type: :string
  field :page, 23, type: :int32
  field :per_page, 24, type: :int32
end

defmodule Chef.Automate.Api.Secrets.Query.OrderType do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  field :ASC, 0
  field :DESC, 1
end

defmodule Chef.Automate.Api.Secrets.Secret do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          type: String.t(),
          last_modified: Google.Protobuf.Timestamp.t() | nil,
          tags: [Chef.Automate.Api.Secrets.Kv.t()],
          data: [Chef.Automate.Api.Secrets.Kv.t()]
        }
  defstruct [:id, :name, :type, :last_modified, :tags, :data]

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :type, 3, type: :string
  field :last_modified, 20, type: Google.Protobuf.Timestamp
  field :tags, 21, repeated: true, type: Chef.Automate.Api.Secrets.Kv
  field :data, 22, repeated: true, type: Chef.Automate.Api.Secrets.Kv
end

defmodule Chef.Automate.Api.Secrets.Secrets do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          secrets: [Chef.Automate.Api.Secrets.Secret.t()],
          total: integer
        }
  defstruct [:secrets, :total]

  field :secrets, 1, repeated: true, type: Chef.Automate.Api.Secrets.Secret
  field :total, 20, type: :int32
end

defmodule Chef.Automate.Api.Secrets.Filter do
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

defmodule Chef.Automate.Api.Secrets.Kv do
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

defmodule Chef.Automate.Api.Secrets.SecretsService.Service do
  @moduledoc false
  use GRPC.Service, name: "chef.automate.api.secrets.SecretsService"

  rpc :Create, Chef.Automate.Api.Secrets.Secret, Chef.Automate.Api.Secrets.Id
  rpc :Read, Chef.Automate.Api.Secrets.Id, Chef.Automate.Api.Secrets.Secret
  rpc :Update, Chef.Automate.Api.Secrets.Secret, Chef.Automate.Api.Secrets.UpdateResponse
  rpc :Delete, Chef.Automate.Api.Secrets.Id, Chef.Automate.Api.Secrets.DeleteResponse
  rpc :List, Chef.Automate.Api.Secrets.Query, Chef.Automate.Api.Secrets.Secrets
end

defmodule Chef.Automate.Api.Secrets.SecretsService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Chef.Automate.Api.Secrets.SecretsService.Service
end
