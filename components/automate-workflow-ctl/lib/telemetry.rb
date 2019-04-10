add_command "telemetry", "Configure the Chef telemetry preferences", 2 do
  require "telemetry/command"
  Telemetry::Command.new(ARGV[3..-1]).run
end
