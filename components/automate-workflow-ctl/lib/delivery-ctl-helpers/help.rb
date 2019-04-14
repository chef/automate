module DeliveryCtlHelpers
  class Help
    def self.help_asked_for?
      %w(help -h --help).include?(ARGV[1])
    end
  end
end
