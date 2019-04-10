module CtlHelpers
  class ErrorHandler
    def self.log_stack_trace(log_file, exception)
      log = File.new(log_file, "a+")
      log.puts "Exception received: #{exception.class}"
      log.puts "Error message: #{exception.message}"
      log.puts "Stack Trace:\n#{exception.backtrace.join('\n')}"
    end
  end
end
