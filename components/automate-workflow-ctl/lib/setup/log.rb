module Setup
  module Log
    # Log directory /var/log/delivery-ctl is created by Delivery postinst script.
    # Don't reinitialize on tests
    LOG_LOCATION ||= "/var/log/delivery-ctl/setup.log"
    FULL_LOG_LOCATION ||= "/var/log/delivery-ctl/setup-full.log"

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
