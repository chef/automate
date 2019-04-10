module NodeUtils
  module Log
    def info(msg = "")
      $stdout.puts msg
    end
    module_function :info

    def error(msg = "")
      $stderr.puts msg
    end
    module_function :error
  end
end
