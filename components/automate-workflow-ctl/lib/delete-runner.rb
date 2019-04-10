add_command "delete-runner", "Delete a runner in Automate.", 2 do
  require 'runner/delete/command'
  Runner::Delete::Command.new(ARGV[3..-1]).run
end
