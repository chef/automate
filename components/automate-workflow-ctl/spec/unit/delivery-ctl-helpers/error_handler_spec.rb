require 'spec_helper'
require 'omnibus-ctl'
require 'ctl-helpers/error_handler'

describe CtlHelpers::ErrorHandler do
  describe 'log_stack_trace' do
    it 'appends to the log file' do
      file = instance_double(File)
      file_name = "log_file.txt"
      exception_text = "Oh no, Mr Bill!"
      backtrace = ["lots", "of stuff goes here"]
      exception = Exception.new(exception_text)
      expect(exception).to receive(:backtrace).and_return(backtrace)
      expect(File).to receive(:new).with(file_name, "a+").and_return(file)
      expect(file).to receive(:puts).with("Exception received: Exception")
      expect(file).to receive(:puts).with("Error message: #{exception_text}")
      expect(file).to receive(:puts).with("Stack Trace:\n#{backtrace.join('\n')}")
      CtlHelpers::ErrorHandler.log_stack_trace(file_name, exception)
    end
  end
end
