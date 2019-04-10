module DeliveryCtlHelpers
  class Help
    def self.help_asked_for?
      %w(help -h --help).include?(ARGV[3])
    end
  end
end
