add_command "install-runner", "Configure a new runner in Automate for job dispatch." do
  require 'runner/install/command'
  Runner::Install::Command.new(ARGV[1..-1]).run
end
