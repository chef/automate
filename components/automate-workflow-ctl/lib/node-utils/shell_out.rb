require "mixlib/shellout"

module NodeUtils
  module ShellOut
    def shell_out(*command)
      cmd = Mixlib::ShellOut.new(*command)
      cmd.run_command
      cmd
    end
    module_function :shell_out
  end
end
