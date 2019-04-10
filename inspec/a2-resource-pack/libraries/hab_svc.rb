class HabSvc < Inspec.resource(1)
  name 'hab_svc'
  desc 'Use the hab_svc InSpec audit resource to test if the named service (controlled by hab) is installed, running / transient or persistent'
  example "
    describe hab_svc('chef/automate-postgresql') do
      it { should be_installed }
      it { should be_transient }
      it { should be_running }
    end
  "

  attr_reader :service_name

  def initialize(service_name)
    @service_name = service_name
    @cache = nil
  end

  def info
    @info ||= begin
      # get the status of hab svc
      cmd = inspec.command("hab svc status #{service_name}")

      installed = cmd.exit_status == 0
      if installed
        status_line = cmd.stdout.lines[1]
        status = status_line.split(/\s+/)
        running = status[2] == "up"
      else
        running = false
      end

      {
        name: service_name,
        description: nil,
        installed: installed,
        running: running,
        type: 'hab',
      }
    end
  end

  # returns the service name from info
  def name
    return service_name if info.nil?
    info[:name]
  end

  # returns the service description from info
  def description
    return nil if info.nil?
    info[:description]
  end

  def to_s
    "Habitat Service #{service_name}"
  end

  # verifies if the service is persistent
  def persistent?(_level = nil)
    return false if info.nil?
    info[:persistent]
  end

  # verifies if the service is transient
  def transient?(_level = nil)
    return false if info.nil?
    info[:transient]
  end

  # verifies the service is registered
  def installed?(_name = nil, _version = nil)
    return false if info.nil?
    info[:installed]
  end

  # verifies the service is currently running
  def running?(_under = nil)
    return false if info.nil?
    info[:running]
  end
end

# TODO: wire this into the existing Service resource

# require 'resources/service'

# class Habitat < Resources::ServiceManager
#   def initialize(service_name, service_ctl = nil)
#     @service_ctl = service_ctl || 'hab'
#     super
#   end

#   # rubocop:disable Style/DoubleNegation
#   def info(service_name)
#     # get the status of hab svc
#     cmd = inspec.command("#{service_ctl} sup status #{service_name}")

#     installed = cmd.exit_status == 0
#     running = installed && !!(cmd.stdout =~ /state:up/)
#     enabled = installed && (running || (cmd.stdout =~ /style:persistent/))

#     {
#       name: service_name,
#       description: nil,
#       installed: installed,
#       running: running,
#       enabled: enabled,
#       type: 'hab',
#     }
#   end
# end

# class HabSvc < Resources::Service
#   name 'hab_svc'
#   desc 'Use the hab_svc InSpec audit resource to test if the named service (controlled by hab) is installed, running and/or enabled.'
#   example "
#     describe hab_svc('chef/automate-postgresql') do
#       it { should be_installed }
#       it { should be_enabled }
#       it { should be_running }
#     end
#   "

#   def select_service_mgmt
#     Habitat.new(inspec, service_ctl)
#   end
# end
