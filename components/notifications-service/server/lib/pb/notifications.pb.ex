defmodule Notifications.ExceptionInfo do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          class: String.t(),
          title: String.t(),
          msg: String.t(),
          backtrace: [String.t()]
        }
  defstruct [:class, :title, :msg, :backtrace]

  field :class, 1, type: :string
  field :title, 2, type: :string
  field :msg, 3, type: :string
  field :backtrace, 4, repeated: true, type: :string
end

defmodule Notifications.TimeInfo do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          start_time: String.t(),
          end_time: String.t()
        }
  defstruct [:start_time, :end_time]

  field :start_time, 1, type: :string
  field :end_time, 2, type: :string
end

defmodule Notifications.Profile do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t(),
          title: String.t(),
          version: String.t(),
          summary: String.t(),
          maintainer: String.t(),
          license: String.t(),
          copyright: String.t(),
          copyright_email: String.t(),
          sha256: String.t(),
          supports: [Notifications.PlatformSupport.t()],
          attributes: [Notifications.Profile.Attribute.t()],
          failed_controls: [Notifications.Profile.Control.t()],
          stats: Notifications.Profile.ControlTotals.t() | nil
        }
  defstruct [
    :name,
    :title,
    :version,
    :summary,
    :maintainer,
    :license,
    :copyright,
    :copyright_email,
    :sha256,
    :supports,
    :attributes,
    :failed_controls,
    :stats
  ]

  field :name, 1, type: :string
  field :title, 2, type: :string
  field :version, 3, type: :string
  field :summary, 4, type: :string
  field :maintainer, 5, type: :string
  field :license, 6, type: :string
  field :copyright, 7, type: :string
  field :copyright_email, 8, type: :string
  field :sha256, 9, type: :string
  field :supports, 10, repeated: true, type: Notifications.PlatformSupport
  field :attributes, 11, repeated: true, type: Notifications.Profile.Attribute
  field :failed_controls, 12, repeated: true, type: Notifications.Profile.Control
  field :stats, 13, type: Notifications.Profile.ControlTotals
end

defmodule Notifications.Profile.Control do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t(),
          impact: float,
          title: String.t(),
          code: String.t(),
          desc: String.t(),
          source_location: Notifications.SourceLocation.t() | nil,
          refs: [Notifications.Refs.t()],
          failed_results: [Notifications.Profile.Control.Result.t()],
          stats: Notifications.Profile.Control.ResultTotals.t() | nil
        }
  defstruct [:id, :impact, :title, :code, :desc, :source_location, :refs, :failed_results, :stats]

  field :id, 1, type: :string
  field :impact, 2, type: :float
  field :title, 3, type: :string
  field :code, 4, type: :string
  field :desc, 5, type: :string
  field :source_location, 6, type: Notifications.SourceLocation
  field :refs, 7, repeated: true, type: Notifications.Refs
  field :failed_results, 9, repeated: true, type: Notifications.Profile.Control.Result
  field :stats, 10, type: Notifications.Profile.Control.ResultTotals
end

defmodule Notifications.Profile.Control.Result do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          status: String.t(),
          code_desc: String.t(),
          run_time: float,
          start_time: String.t(),
          message: String.t(),
          skip_message: String.t()
        }
  defstruct [:status, :code_desc, :run_time, :start_time, :message, :skip_message]

  field :status, 1, type: :string
  field :code_desc, 2, type: :string
  field :run_time, 3, type: :float
  field :start_time, 4, type: :string
  field :message, 5, type: :string
  field :skip_message, 6, type: :string
end

defmodule Notifications.Profile.Control.ResultTotals do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          num_tests: integer,
          num_failed_tests: integer,
          num_skipped_tests: integer,
          num_passed_tests: integer
        }
  defstruct [:num_tests, :num_failed_tests, :num_skipped_tests, :num_passed_tests]

  field :num_tests, 1, type: :int32
  field :num_failed_tests, 2, type: :int32
  field :num_skipped_tests, 3, type: :int32
  field :num_passed_tests, 4, type: :int32
end

defmodule Notifications.Profile.Attribute do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t(),
          options: Notifications.Profile.Attribute.Options.t() | nil
        }
  defstruct [:name, :options]

  field :name, 1, type: :string
  field :options, 2, type: Notifications.Profile.Attribute.Options
end

defmodule Notifications.Profile.Attribute.Options do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          description: String.t()
        }
  defstruct [:description]

  field :description, 1, type: :string
end

defmodule Notifications.Profile.ControlTotals do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          num_tests: integer,
          num_failed_tests: integer,
          num_skipped_tests: integer,
          num_passed_tests: integer
        }
  defstruct [:num_tests, :num_failed_tests, :num_skipped_tests, :num_passed_tests]

  field :num_tests, 1, type: :int32
  field :num_failed_tests, 2, type: :int32
  field :num_skipped_tests, 3, type: :int32
  field :num_passed_tests, 4, type: :int32
end

defmodule Notifications.SourceLocation do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          ref: String.t(),
          line: integer
        }
  defstruct [:ref, :line]

  field :ref, 1, type: :string
  field :line, 2, type: :int32
end

defmodule Notifications.Refs do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          ref: String.t(),
          uri: String.t(),
          url: String.t()
        }
  defstruct [:ref, :uri, :url]

  field :ref, 1, type: :string
  field :uri, 2, type: :string
  field :url, 3, type: :string
end

defmodule Notifications.PlatformSupport do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          inspec: String.t(),
          os_name: String.t(),
          os_family: String.t(),
          release: String.t()
        }
  defstruct [:inspec, :os_name, :os_family, :release]

  field :inspec, 1, type: :string
  field :os_name, 2, type: :string
  field :os_family, 3, type: :string
  field :release, 4, type: :string
end

defmodule Notifications.ComplianceSuccess do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t(),
          compliance_url: String.t(),
          node_name: String.t(),
          node_id: String.t(),
          end_time: String.t(),
          timestamp: String.t()
        }
  defstruct [:id, :compliance_url, :node_name, :node_id, :end_time, :timestamp]

  field :id, 1, type: :string
  field :compliance_url, 2, type: :string
  field :node_name, 3, type: :string
  field :node_id, 4, type: :string
  field :end_time, 5, type: :string
  field :timestamp, 6, type: :string
end

defmodule Notifications.ComplianceFailure do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t(),
          compliance_url: String.t(),
          node_name: String.t(),
          node_id: String.t(),
          inspec_version: String.t(),
          test_totals: Notifications.ComplianceFailure.ControlTotals.t() | nil,
          failed_profiles: [Notifications.Profile.t()],
          end_time: String.t(),
          timestamp: String.t()
        }
  defstruct [
    :id,
    :compliance_url,
    :node_name,
    :node_id,
    :inspec_version,
    :test_totals,
    :failed_profiles,
    :end_time,
    :timestamp
  ]

  field :id, 1, type: :string
  field :compliance_url, 2, type: :string
  field :node_name, 3, type: :string
  field :node_id, 4, type: :string
  field :inspec_version, 5, type: :string
  field :test_totals, 6, type: Notifications.ComplianceFailure.ControlTotals
  field :failed_profiles, 7, repeated: true, type: Notifications.Profile
  field :end_time, 8, type: :string
  field :timestamp, 9, type: :string
end

defmodule Notifications.ComplianceFailure.ControlTotals do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          skipped: integer,
          passed: integer,
          failed: integer,
          critical: integer,
          critical_failed: integer
        }
  defstruct [:skipped, :passed, :failed, :critical, :critical_failed]

  field :skipped, 1, type: :int32
  field :passed, 2, type: :int32
  field :failed, 3, type: :int32
  field :critical, 4, type: :int32
  field :critical_failed, 5, type: :int32
end

defmodule Notifications.CCRFailure do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          run_id: String.t(),
          node_name: String.t(),
          node_url: String.t(),
          run_url: String.t(),
          cookbook: String.t(),
          recipe: String.t(),
          time: Notifications.TimeInfo.t() | nil,
          exception: Notifications.ExceptionInfo.t() | nil,
          timestamp: String.t()
        }
  defstruct [
    :run_id,
    :node_name,
    :node_url,
    :run_url,
    :cookbook,
    :recipe,
    :time,
    :exception,
    :timestamp
  ]

  field :run_id, 1, type: :string
  field :node_name, 2, type: :string
  field :node_url, 3, type: :string
  field :run_url, 4, type: :string
  field :cookbook, 5, type: :string
  field :recipe, 6, type: :string
  field :time, 7, type: Notifications.TimeInfo
  field :exception, 8, type: Notifications.ExceptionInfo
  field :timestamp, 9, type: :string
end

defmodule Notifications.CCRSuccess do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          run_id: String.t(),
          node_name: String.t(),
          run_url: String.t(),
          time: Notifications.TimeInfo.t() | nil,
          updated_resource_count: integer,
          timestamp: String.t()
        }
  defstruct [:run_id, :node_name, :run_url, :time, :updated_resource_count, :timestamp]

  field :run_id, 1, type: :string
  field :node_name, 2, type: :string
  field :run_url, 3, type: :string
  field :time, 4, type: Notifications.TimeInfo
  field :updated_resource_count, 5, type: :int32
  field :timestamp, 6, type: :string
end

defmodule Notifications.Response do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Notifications.Event do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          event: {atom, any},
          id: String.t()
        }
  defstruct [:event, :id]

  oneof :event, 0
  field :id, 1, type: :string
  field :CCRSuccess, 2, type: Notifications.CCRSuccess, oneof: 0
  field :CCRFailure, 3, type: Notifications.CCRFailure, oneof: 0
  field :ComplianceSuccess, 4, type: Notifications.ComplianceSuccess, oneof: 0
  field :ComplianceFailure, 5, type: Notifications.ComplianceFailure, oneof: 0
end
