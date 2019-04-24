add_command "delete-runner", "Delete a runner in Automate." do
  require 'runner/delete/command'
  Runner::Delete::Command.new(ARGV[1..-1]).run
end
