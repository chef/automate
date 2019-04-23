add_command "telemetry", "Configure the Chef telemetry preferences" do
  require "telemetry/command"
  Telemetry::Command.new(ARGV[1..-1]).run
end
