# TODO: remove this custom matcher once this is resolved
# https://github.com/inspec/inspec/issues/4059#issuecomment-499397982
RSpec::Matchers.define :be_up do
  match do |service|
    service.running? == true
  end

  chain :under do |_under|
    raise "[UNSUPPORTED] under is not supported"
  end

  failure_message do |service|
    "expected that `#{service}` is up and running"
  end
end

class HabSvc < Inspec.resource(1)
  name 'hab_svc'
  desc 'Use the hab_svc InSpec audit resource to test if the named service (controlled by hab) is installed, running / transient or persistent'
  example "
    describe hab_svc('chef/automate-ha-postgresql') do
      it { should be_installed }
      it { should be_transient }
      it { should be_up }
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
      cmd = inspec.command("HAB_LICENSE=accept-no-persist hab svc status #{service_name}")

      installed = cmd.exit_status == 0
      if installed
        status_line = cmd.stdout.lines[1]
        status = status_line.split(/\s+/)
        running = status[3] == "up" && status[4].to_i >= 60 # service is not flapping
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
