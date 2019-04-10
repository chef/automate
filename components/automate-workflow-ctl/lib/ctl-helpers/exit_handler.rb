module CtlHelpers
  module ExitHandler
    def self.handle_exit
      yield
      exit 0
    rescue => e
      if $stderr.tty?
        $stderr.puts "\e[31mERROR:\e[0m #{e.message} \n"
      else
        $stderr.puts "ERROR: #{e.message} \n"
      end
      exit 1
    end
  end
end
