require 'json'

class AutomateLifecycle < Inspec.resource(1)
  name 'automate_lifecycle'

  desc 'Get lifecycle settings from automate'

  example <<~EXAMPLE
    describe automate_lifecycle('') do
      its("available_percent.min") { should cmp > 10 }
    end
  EXAMPLE

  def initialize(admin_token)
    @admin_token = admin_token
    @url = 'https://localhost/api/v0/data-lifecycle/status'
  end

  def jobs_for(group)
    services = lifecycle_data.dig(group, 'jobs') || []

    AutomateLifecycleJobs.new(group, services)
  end

  private

  def lifecycle_data
    @lifecycle_data ||= JSON.parse(read_lifecycle_data)
  end

  def read_lifecycle_data
    inspec.command(%Q[ curl -k -H "api-token: #{@admin_token}" #{@url} ]).stdout
  end
end

class AutomateLifecycleJobs
  POSSIBlE_JOBS = %w{
    missing_nodes missing_nodes_for_deletion periodic_purge_timeseries periodic_purge
    delete_nodes
  }.freeze

  def initialize(group, data = [])
    @group = group
    @jobs = {}

    data.each do |job|
      @jobs[job['name'].to_sym] = AutomateLifecycleJob.new(job['name'], job)
    end
  end

  def method_missing(name)
    return @jobs[name] || AutomateLifecycleJob.new(name) if POSSIBlE_JOBS.include?(name.to_s)
    super
  end

  def exist?
    !@jobs.keys.empty?
  end

  def to_s
    "Automate lifecycle settings for #{@group}"
  end
end

class AutomateLifecycleJob
  def initialize(name, data = {})
    @name = name.to_s
    @data = data
  end

  def to_s
    @name
  end

  def disabled?
    @data.fetch('disabled', true)
  end

  def enabled?
    !disabled?
  end
end
