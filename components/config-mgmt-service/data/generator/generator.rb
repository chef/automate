require './lib/data_generator'
require 'faker'
require './lib/data_generator/elasticsearch_insert_client'
require './lib/data_generator/generate_static_functional_data'

# These hardcoded lists lifted directly from original data generator.
# TODO: when adding multi-server/org generator, simulate the relationship
# between chef-server/org and envs/roles. (different envs/roles exist on
# different servers/orgs)
ALL_ENVIRONMENTS = ['_default', 'acceptance-org-proj-master']
ALL_ROLES = ['admin', 'windows_builder', 'stash']
ALL_PLATFORMS = ['centos', 'ubuntu', 'oracle', 'solaris', 'windows']
ALL_ORGS = ['org1', 'org2']
ALL_EVENT_ACTIONS = ['created', 'updated', 'unknown']
ALL_POLICY_NAMES = ['policy1', 'policy2', 'policy3']
ALL_POLICY_GROUPS = ['dev', 'prod', 'audit']

NODES_TO_GENERATE = 100
HOURS_BETWEEN_CCRS = 6
# some constants for timings, in seconds
CONVERGE_HISTORY = 14 * 24 * 60 * 60
TIME_BETWEEN_CONVERGES = HOURS_BETWEEN_CCRS * 60 * 60 # in seconds
CONVERGES_PER_NODE = CONVERGE_HISTORY / TIME_BETWEEN_CONVERGES


class NodeState
  attr_reader :latest_converge

  def initialize
    checkin = some_time_in_the_last_6_hours
    node_name = Faker::Lorem.words(3).join('-')

    if [0,1].sample == 0
       node_name = node_name.capitalize
    end

    # MAYBE: the logstash `@` keys like `@timestamp`
    @doc = {
      entity_uuid: SecureRandom.uuid,
      exists: true,
      node_name: node_name,
      fqdn: "#{node_name}.#{DataGenerator::ENTERPRISE_DOMAIN_NAME}",
      ipaddress: Faker::Internet.ip_v4_address,
      checkin: checkin.utc.iso8601,
      uptime_seconds: (Time.now - Faker::Time.backward).to_i,  # range is 365 days, in seconds
      source_fqdn: "chef-server.#{DataGenerator::ENTERPRISE_DOMAIN_NAME}",
      organization_name: ALL_ORGS.sample,

      # Possible enhancement: derive `platform_family` and `platform_version` values
      # from the chosen `platform` value. For now, hardcode some nonsense values.
      platform: ALL_PLATFORMS.sample,
      platform_family: 'robinson',
      platform_version: '0.0.42',
      expanded_run_list:  {
        run_list: get_run_list_item.sample(2),
        id: "acceptance-chef-products-automate-master",
     },
    }

    # have half be policyfile nodes if desired
    if [0,1].sample == 0 || ENV['GENERATE_POLICYFILE_NODES'] == 'false'
      @doc[:environment] = ALL_ENVIRONMENTS.sample
      @doc[:roles] = ALL_ROLES.sample(Faker::Number.between(0, ALL_ROLES.length).to_i)
    else
      @doc[:policy_name] = ALL_POLICY_NAMES.sample
      @doc[:policy_group] = ALL_POLICY_GROUPS.sample
      @doc[:policy_revision] = Faker::Crypto.sha256
    end

    @latest_converge = ConvergeFinished.new(@doc, checkin)
    @doc[:latest_run_id] = @latest_converge.run_id
    @doc[:status] = @latest_converge.status
  end

  def save!(client)
    client.create index: 'node-state',
                  type: 'node-state',
                  id: @doc[:entity_uuid],
                  body: @doc
    puts "wrote node name #{@doc[:node_name]} with id #{@doc[:entity_uuid]}"
  end

  def get_run_list_item
    [
      {
         name: "cd-infrastructure-base::default",
         type: "recipe",
         skipped: false
      },
      {
         name: "automate-deploy::runner",
         type: "recipe",
         skipped: false
      },
      {
        name: "apache::default",
        type: "recipe",
        skipped: false
      },
      {
        name: "mysql::runner",
        type: "recipe",
        skipped: false
      },
      {
        name: "postgres::default",
        type: "recipe",
        skipped: false
      },
      {
        name: "java::runner",
        type: "recipe",
        skipped: false
      },
      {
        name: "erlang::default",
        type: "recipe",
        skipped: false
      },
      {
        name: "ruby::runner",
        type: "recipe",
        skipped: false
      },
   ]
  end
end

class ConvergeFinished
  attr_reader :status, :run_id

  def initialize(node, end_time)
    @node = node
    @end_time = end_time
    @status = [ 'success', 'failure' ].sample
    @run_id = SecureRandom.uuid
    @resources = self.get_resources.sample(10)

    @doc = {
      event_type: 'converge',
      event_action: 'finished',
      node_name: @node[:node_name],
      entity_uuid: @node[:entity_uuid],
      organization_name: @node[:organization_name],
      run_id: @run_id,
      status: @status,
      start_time: (end_time - Faker::Number.between(1, 30 * 60)).utc.iso8601,
      end_time: @end_time.utc.iso8601,
      resources: @resources
    }

    @doc['error'] = get_error_message if @status == 'failure'

    [:entity_uuid, :environment, :platform, :uptime_seconds, :ipaddress,
     :fqdn, :policy_name, :policy_group, :policy_revision, :expanded_run_list].each do |key|
      @doc[key] = @node[key] if @node[key]
    end
  end

  def get_error_message
    {
      "class": "Error class",
      "message": "Error message",
      "backtrace": [
        "/opt/chef/embedded/lib/ruby/gems/2.3.0/gems/chef-12.21.3/bin/chef-client:26:in '<top (required)>'",
        "/usr/bin/chef-client:57:in 'load'",
        "/usr/bin/chef-client:57:in '<main>'",
      ],
      "description": {
        "title": "Error title",
        "sections": [
          {
              "Mixlib::ShellOut::ShellCommandFailed": "Expected process to exit with [0], but received ''\n---- Begin output of /bin/systemctl --system stop chef-client ----\nSTDOUT: \nSTDERR: \n---- End output of /bin/systemctl --system stop chef-client ----\nRan /bin/systemctl --system stop chef-client returned "
          },
          {
              "Resource Declaration:": "# In /var/chef/cache/cookbooks/chef-client/recipes/systemd_service.rb\n\n 62: service 'chef-client' do\n 63:   supports status: true, restart: true\n 64:   action(timer ? [:disable, :stop] : [:enable, :start])\n 65: end\n 66: \n"
          },
          {
              "Compiled Resource:": "# Declared in /var/chef/cache/cookbooks/chef-client/recipes/systemd_service.rb:62:in `from_file'\n\nservice(\"chef-client\") do\n  action [:disable, :stop]\n  default_guard_interpreter :default\n  service_name \"chef-client\"\n  enabled false\n  running true\n  masked false\n  pattern \"chef-client\"\n  declared_type :service\n  cookbook_name \"chef-client\"\n  recipe_name \"systemd_service\"\n  supports {:status=>true, :restart=>true}\nend\n"
          },
       ],
      }
    }
  end

  def save!(client)
    client.create index: "converge-history-#{@end_time.utc.strftime("%Y.%m.%d")}",
                  type: 'converge',
                  body: @doc,
                  id: @doc[:run_id]
    #puts "wrote converge #{@status} at #{@end_time.localtime} with run_id: #{@doc[:run_id]}"
  end

  def previous_converge
    ConvergeFinished.new @node, @end_time - TIME_BETWEEN_CONVERGES
  end

  def get_resources
    [
      {
         "type": "chef_gem",
         "name": "chef-sugar",
         "id": "chef-sugar",
         "duration": "186",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "chef-sugar",
         "cookbook_version": "3.4.0",
         "recipe_name": "default"
      },
      {
         "type": "file",
         "name": "/var/lib/apt/periodic/update-success-stamp",
         "id": "/var/lib/apt/periodic/update-success-stamp",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "nothing",
         "status": "skipped",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default",
         "conditional": "not_if { action == :nothing }"
      },
      {
         "type": "apt_update",
         "name": "periodic",
         "id": "periodic",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "periodic",
         "status": "up-to-date",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default"
      },
      {
         "type": "execute",
         "name": "apt-get update",
         "id": "apt-get update",
         "duration": "0",
         "delta": "",
         "ignore_failure": true,
         "result": "nothing",
         "status": "skipped",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default",
         "conditional": "not_if { action == :nothing }"
      },
      {
         "type": "execute",
         "name": "apt-get autoremove",
         "id": "apt-get -y autoremove",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "nothing",
         "status": "skipped",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default",
         "conditional": "not_if { action == :nothing }"
      },
      {
         "type": "execute",
         "name": "apt-get autoclean",
         "id": "apt-get -y autoclean",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "nothing",
         "status": "skipped",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default",
         "conditional": "not_if { action == :nothing }"
      },
      {
         "type": "directory",
         "name": "/var/cache/local",
         "id": "/var/cache/local",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default"
      },
      {
         "type": "directory",
         "name": "/var/cache/local/preseeding",
         "id": "/var/cache/local/preseeding",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default"
      },
      {
         "type": "template",
         "name": "/etc/apt/apt.conf.d/10dpkg-options",
         "id": "/etc/apt/apt.conf.d/10dpkg-options",
         "duration": "5",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default"
      },
      {
         "type": "template",
         "name": "/etc/apt/apt.conf.d/10recommends",
         "id": "/etc/apt/apt.conf.d/10recommends",
         "duration": "3",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default"
      },
      {
         "type": "apt_package",
         "name": "apt-transport-https",
         "id": "apt-transport-https",
         "duration": "35",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "apt",
         "cookbook_version": "6.1.3",
         "recipe_name": "default"
      },
      {
         "type": "apt_package",
         "name": "ntp",
         "id": "ntp",
         "duration": "34",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default"
      },
      {
         "type": "apt_package",
         "name": "Remove ntpdate",
         "id": "ntpdate",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "remove",
         "status": "skipped",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default",
         "conditional": "only_if { #code block }"
      },
      {
         "type": "directory",
         "name": "/var/lib/ntp",
         "id": "/var/lib/ntp",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default"
      },
      {
         "type": "directory",
         "name": "/var/log/ntpstats/",
         "id": "/var/log/ntpstats/",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default"
      },
      {
         "type": "cookbook_file",
         "name": "/etc/ntp.leapseconds",
         "id": "/etc/ntp.leapseconds",
         "duration": "5",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default"
      },
      {
         "type": "service",
         "name": "apparmor",
         "id": "apparmor",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "nothing",
         "status": "skipped",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "apparmor",
         "conditional": "not_if { action == :nothing }"
      },
      {
         "type": "cookbook_file",
         "name": "/etc/apparmor.d/usr.sbin.ntpd",
         "id": "/etc/apparmor.d/usr.sbin.ntpd",
         "duration": "4",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "apparmor"
      },
      {
         "type": "template",
         "name": "/etc/ntp.conf",
         "id": "/etc/ntp.conf",
         "duration": "28",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default"
      },
      {
         "type": "execute",
         "name": "Force sync hardware clock with system clock",
         "id": "hwclock --systohc",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "run",
         "status": "skipped",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default",
         "conditional": "only_if { #code block }"
      },
      {
         "type": "service",
         "name": "ntp",
         "id": "ntp",
         "duration": "66",
         "delta": "",
         "ignore_failure": false,
         "result": "enable",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default"
      },
      {
         "type": "service",
         "name": "ntp",
         "id": "ntp",
         "duration": "66",
         "delta": "",
         "ignore_failure": false,
         "result": "start",
         "status": "up-to-date",
         "cookbook_name": "ntp",
         "cookbook_version": "3.5.1",
         "recipe_name": "default"
      },
      {
         "type": "chef_gem",
         "name": "chef-sugar",
         "id": "chef-sugar",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "chef-sugar",
         "cookbook_version": "3.4.0",
         "recipe_name": "default"
      },
      {
         "type": "apt_package",
         "name": "sudo",
         "id": "sudo",
         "duration": "14",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "skipped",
         "cookbook_name": "sudo",
         "cookbook_version": "3.5.3",
         "recipe_name": "default",
         "conditional": "not_if \"which sudo\""
      },
      {
         "type": "directory",
         "name": "/etc/sudoers.d",
         "id": "/etc/sudoers.d",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "sudo",
         "cookbook_version": "3.5.3",
         "recipe_name": "default"
      },
      {
         "type": "cookbook_file",
         "name": "/etc/sudoers.d/README",
         "id": "/etc/sudoers.d/README",
         "duration": "6",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "sudo",
         "cookbook_version": "3.5.3",
         "recipe_name": "default"
      },
      {
         "type": "template",
         "name": "/etc/sudoers",
         "id": "/etc/sudoers",
         "duration": "7",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "sudo",
         "cookbook_version": "3.5.3",
         "recipe_name": "default"
      },
      {
         "type": "apt_package",
         "name": "bash",
         "id": "bash",
         "duration": "34",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "oc-users",
         "cookbook_version": "1.0.2",
         "recipe_name": "default"
      },
      {
         "type": "apt_package",
         "name": "zsh",
         "id": "zsh",
         "duration": "34",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "oc-users",
         "cookbook_version": "1.0.2",
         "recipe_name": "default"
      },
      {
         "type": "users_manage",
         "name": "sysadmin",
         "id": "sysadmin",
         "duration": "2424",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "oc-users",
         "cookbook_version": "1.0.2",
         "recipe_name": "default"
      },
      {
         "type": "sudo",
         "name": "sysadmin",
         "id": "sysadmin",
         "duration": "40",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "oc-users",
         "cookbook_version": "1.0.2",
         "recipe_name": "default"
      },
      {
         "type": "sudo",
         "name": "vagrant",
         "id": "vagrant",
         "duration": "4",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "skipped",
         "cookbook_name": "oc-users",
         "cookbook_version": "1.0.2",
         "recipe_name": "default",
         "conditional": "only_if { #code block }"
      },
      {
         "type": "sudo",
         "name": "ubuntu",
         "id": "ubuntu",
         "duration": "39",
         "delta": "",
         "ignore_failure": false,
         "result": "install",
         "status": "up-to-date",
         "cookbook_name": "oc-users",
         "cookbook_version": "1.0.2",
         "recipe_name": "default"
      },
      {
         "type": "directory",
         "name": "/etc/chef/client.d",
         "id": "/etc/chef/client.d",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "cia_infra",
         "cookbook_version": "0.18.9",
         "recipe_name": "base"
      },
      {
         "type": "file",
         "name": "/etc/chef/client.d/viz.rb",
         "id": "/etc/chef/client.d/viz.rb",
         "duration": "0",
         "delta": "",
         "ignore_failure": false,
         "result": "delete",
         "status": "up-to-date",
         "cookbook_name": "cia_infra",
         "cookbook_version": "0.18.9",
         "recipe_name": "base"
      },
      {
         "type": "directory",
         "name": "/var/run/chef",
         "id": "/var/run/chef",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "chef-client",
         "cookbook_version": "8.1.8",
         "recipe_name": "init_service"
      },
      {
         "type": "directory",
         "name": "/var/cache/chef",
         "id": "/var/cache/chef",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "chef-client",
         "cookbook_version": "8.1.8",
         "recipe_name": "init_service"
      },
      {
         "type": "directory",
         "name": "/var/lib/chef",
         "id": "/var/lib/chef",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "chef-client",
         "cookbook_version": "8.1.8",
         "recipe_name": "init_service"
      },
      {
         "type": "directory",
         "name": "/var/log/chef",
         "id": "/var/log/chef",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "chef-client",
         "cookbook_version": "8.1.8",
         "recipe_name": "init_service"
      },
      {
         "type": "directory",
         "name": "/etc/chef",
         "id": "/etc/chef",
         "duration": "2",
         "delta": "",
         "ignore_failure": false,
         "result": "create",
         "status": "up-to-date",
         "cookbook_name": "chef-client",
         "cookbook_version": "8.1.8",
         "recipe_name": "init_service"
      },
   ]
  end

end

### main

def some_time_in_the_last_6_hours
  Time.now - Faker::Number.between(1, (60*60*HOURS_BETWEEN_CCRS))
end

client = DataGenerator::ElasticsearchInsertClient.new
(NODES_TO_GENERATE).times do
  node = NodeState.new
  node.save!(client)

  converge = node.latest_converge
  (1..CONVERGES_PER_NODE).each do
    converge.save!(client)
    converge = converge.previous_converge
  end
  client.write
end
