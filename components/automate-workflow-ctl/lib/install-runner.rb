add_command "install-runner", "Configure a new runner in Automate for job dispatch.", 2 do
  require 'runner/install/command'
  Runner::Install::Command.new(ARGV[3..-1]).run
end
