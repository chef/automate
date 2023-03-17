defmodule Notifications.ExceptionInfo do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :class, 1, type: :string
  field :title, 2, type: :string
  field :msg, 3, type: :string
  field :backtrace, 4, repeated: true, type: :string
end

defmodule Notifications.TimeInfo do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :start_time, 1, type: :string, json_name: "startTime"
  field :end_time, 2, type: :string, json_name: "endTime"
end

defmodule Notifications.Profile.Control.Result do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :status, 1, type: :string
  field :code_desc, 2, type: :string, json_name: "codeDesc"
  field :run_time, 3, type: :float, json_name: "runTime"
  field :start_time, 4, type: :string, json_name: "startTime"
  field :message, 5, type: :string
  field :skip_message, 6, type: :string, json_name: "skipMessage"
end

defmodule Notifications.Profile.Control.ResultTotals do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :num_tests, 1, type: :int32, json_name: "numTests"
  field :num_failed_tests, 2, type: :int32, json_name: "numFailedTests"
  field :num_skipped_tests, 3, type: :int32, json_name: "numSkippedTests"
  field :num_passed_tests, 4, type: :int32, json_name: "numPassedTests"
end

defmodule Notifications.Profile.Control do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
  field :impact, 2, type: :float
  field :title, 3, type: :string
  field :code, 4, type: :string
  field :desc, 5, type: :string
  field :source_location, 6, type: Notifications.SourceLocation, json_name: "sourceLocation"
  field :refs, 7, repeated: true, type: Notifications.Refs

  field :failed_results, 9,
    repeated: true,
    type: Notifications.Profile.Control.Result,
    json_name: "failedResults"

  field :stats, 10, type: Notifications.Profile.Control.ResultTotals
end

defmodule Notifications.Profile.Attribute.Options do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :description, 1, type: :string
end

defmodule Notifications.Profile.Attribute do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :options, 2, type: Notifications.Profile.Attribute.Options
end

defmodule Notifications.Profile.ControlTotals do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :num_tests, 1, type: :int32, json_name: "numTests"
  field :num_failed_tests, 2, type: :int32, json_name: "numFailedTests"
  field :num_skipped_tests, 3, type: :int32, json_name: "numSkippedTests"
  field :num_passed_tests, 4, type: :int32, json_name: "numPassedTests"
end

defmodule Notifications.Profile do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :title, 2, type: :string
  field :version, 3, type: :string
  field :summary, 4, type: :string
  field :maintainer, 5, type: :string
  field :license, 6, type: :string
  field :copyright, 7, type: :string
  field :copyright_email, 8, type: :string, json_name: "copyrightEmail"
  field :sha256, 9, type: :string
  field :supports, 10, repeated: true, type: Notifications.PlatformSupport
  field :attributes, 11, repeated: true, type: Notifications.Profile.Attribute

  field :failed_controls, 12,
    repeated: true,
    type: Notifications.Profile.Control,
    json_name: "failedControls"

  field :stats, 13, type: Notifications.Profile.ControlTotals
end

defmodule Notifications.SourceLocation do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :ref, 1, type: :string
  field :line, 2, type: :int32
end

defmodule Notifications.Refs do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :uri, 2, type: :string
  field :url, 3, type: :string
end

defmodule Notifications.PlatformSupport do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :inspec, 1, type: :string
  field :os_name, 2, type: :string, json_name: "osName"
  field :os_family, 3, type: :string, json_name: "osFamily"
  field :release, 4, type: :string
end

defmodule Notifications.ComplianceSuccess do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
  field :compliance_url, 2, type: :string, json_name: "complianceUrl"
  field :node_name, 3, type: :string, json_name: "nodeName"
  field :node_id, 4, type: :string, json_name: "nodeId"
  field :end_time, 5, type: :string, json_name: "endTime"
  field :timestamp, 6, type: :string
end

defmodule Notifications.ComplianceFailure.ControlTotals do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :skipped, 1, type: :int32
  field :passed, 2, type: :int32
  field :failed, 3, type: :int32
  field :critical, 4, type: :int32
  field :critical_failed, 5, type: :int32, json_name: "criticalFailed"
end

defmodule Notifications.ComplianceFailure do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
  field :compliance_url, 2, type: :string, json_name: "complianceUrl"
  field :node_name, 3, type: :string, json_name: "nodeName"
  field :node_id, 4, type: :string, json_name: "nodeId"
  field :inspec_version, 5, type: :string, json_name: "inspecVersion"

  field :test_totals, 6,
    type: Notifications.ComplianceFailure.ControlTotals,
    json_name: "testTotals"

  field :failed_profiles, 7,
    repeated: true,
    type: Notifications.Profile,
    json_name: "failedProfiles"

  field :end_time, 8, type: :string, json_name: "endTime"
  field :timestamp, 9, type: :string
end

defmodule Notifications.CCRFailure do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :run_id, 1, type: :string, json_name: "runId"
  field :node_name, 2, type: :string, json_name: "nodeName"
  field :node_url, 3, type: :string, json_name: "nodeUrl"
  field :run_url, 4, type: :string, json_name: "runUrl"
  field :cookbook, 5, type: :string
  field :recipe, 6, type: :string
  field :time, 7, type: Notifications.TimeInfo
  field :exception, 8, type: Notifications.ExceptionInfo
  field :timestamp, 9, type: :string
end

defmodule Notifications.CCRSuccess do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :run_id, 1, type: :string, json_name: "runId"
  field :node_name, 2, type: :string, json_name: "nodeName"
  field :run_url, 3, type: :string, json_name: "runUrl"
  field :time, 4, type: Notifications.TimeInfo
  field :updated_resource_count, 5, type: :int32, json_name: "updatedResourceCount"
  field :timestamp, 6, type: :string
end

defmodule Notifications.Response do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3
end

defmodule Notifications.Event do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :event, 0

  field :id, 1, type: :string
  field :CCRSuccess, 2, type: Notifications.CCRSuccess, oneof: 0
  field :CCRFailure, 3, type: Notifications.CCRFailure, oneof: 0
  field :ComplianceSuccess, 4, type: Notifications.ComplianceSuccess, oneof: 0
  field :ComplianceFailure, 5, type: Notifications.ComplianceFailure, oneof: 0
end