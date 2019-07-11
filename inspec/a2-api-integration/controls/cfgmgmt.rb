# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved
title 'config-mgmt-service CCR REST API integration tests'

control 'config-mgmt-ccr-1' do
  title 'Submit config-mgmt fixture data'
  desc 'Verifies that we can submit data'

  %w(
    chefdk-debian-7-tester-2d206b
    chefdk-debian-8-tester-79e320
    chefdk-mac_os_x-10.10-tester-cd52f5
    chefdk-mac_os_x-10.11-tester-24f21f
    chefdk-macos-10.12-tester-f53697
    chefdk-macos-10.13-tester-46912e
    chefdk-rhel-6-tester-6d2a67
    chefdk-rhel-7-tester-6bbaaf
    chefdk-sles-11-tester-9a38fc
    chefdk-sles-12-tester-a51a61
    chefdk-ubuntu-14.04-tester-05b95c
    chefdk-ubuntu-16.04-tester-91bc78
    chefdk-windows-2008r2-tester-aace65
    chefdk-windows-2012r2-tester-030cda
    chefdk-windows-2016-tester-d543a0
    converge-failure-report
    ccr_with_run_list_role
    run_with_ignore_resource
  ).each do |node_name|
    %w(
      run_start
      run_converge
    ).each do |message_type|
      describe "POST /data-collector/v0 #{message_type} for #{node_name}" do
        let(:api_request) do
          automate_api_request(
            '/data-collector/v0',
            http_method: 'POST',
            request_body: inspec.profile.file("fixtures/converge/#{node_name}_#{message_type}.json")
          )
        end

        it 'should ingest the data successfully' do
          expect(api_request.http_status).to eq 200
        end
      end
    end
  end

  %w(
    run_with_ignore_resource
  ).each do |node_name|
    %w(
      run_start
      run_converge
    ).each do |message_type|
      describe "POST /api/v0/ingest/events/chef/run #{message_type} for #{node_name}" do
        let(:api_request) do
          automate_api_request(
            '/api/v0/ingest/events/chef/run',
            http_method: 'POST',
            request_body: inspec.profile.file("fixtures/converge/#{node_name}_#{message_type}.json")
          )
        end

        it 'should ingest the data successfully' do
          expect(api_request.parsed_response_body).to eq({})
          expect(api_request.http_status).to eq 200
        end
      end
    end
  end

  # Wait for data to be indexed
  describe command('sleep 15') do
    its('exit_status') { should eq 0 }
  end
end

control 'config-mgmt-ccr-2' do
  title '/api/v0/cfgmgmt/nodes'

  describe 'GET /api/v0/cfgmgmt/nodes' do
    let(:api_request) do
      automate_api_request(
        '/api/v0/cfgmgmt/nodes',
        request_params: request_params,
        http_method: 'GET'
      )
    end

    let(:request_params) do
      {
        'pagination.size': 18, # default pagination size is only 10
      }
    end

    let(:api_runs_request) do
      automate_api_request(
        "/api/v0/cfgmgmt/nodes/#{request_node_id}/runs/#{request_run_id}",
        http_method: 'GET'
      )
    end

    let(:request_node_id) do
      '82760210-4686-497e-b039-efca78dee64b'
    end

    let(:request_run_id) do
      '639844f4-2ce6-42ba-8c9d-853db69adff3'
    end

    #######################################################################
    # All nodes
    #######################################################################
    describe 'when all successful nodes exist' do
      it 'should return 18 nodes' do
        expect(api_request.http_status).to eq 200
        expect(api_request.parsed_response_body).to include(
          include(name: 'chefdk-debian-7-tester-2d206b'),
          include(name: 'chefdk-debian-8-tester-79e320'),
          include(name: 'chefdk-mac_os_x-10.10-tester-cd52f5'),
          include(name: 'chefdk-mac_os_x-10.11-tester-24f21f'),
          include(name: 'chefdk-macos-10.12-tester-f53697'),
          include(name: 'chefdk-macos-10.13-tester-46912e'),
          include(name: 'chefdk-rhel-6-tester-6d2a67'),
          include(name: 'chefdk-rhel-7-tester-6bbaaf'),
          include(name: 'chefdk-sles-11-tester-9a38fc'),
          include(name: 'chefdk-sles-12-tester-a51a61'),
          include(name: 'chefdk-ubuntu-14.04-tester-05b95c'),
          include(name: 'chefdk-ubuntu-16.04-tester-91bc78'),
          include(name: 'chefdk-windows-2008r2-tester-aace65'),
          include(name: 'chefdk-windows-2012r2-tester-030cda'),
          include(name: 'chefdk-windows-2016-tester-d543a0'),
          include(name: 'insights.chef.co'),
          include(name: 'chef-client-dev.test'),
          include(name: 'chef-client-dev-demo.test'),
        )
      end
    end

    #######################################################################
    # Basic filtering
    #######################################################################
    describe 'when the filter `platform:mac_os_x` is provided' do
      let(:request_params) do
        {
          filter: 'platform:mac_os_x*',
        }
      end

      it 'should return only macOS nodes' do
        expect(api_request.http_status).to eq 200
        expect(api_request.parsed_response_body).to contain_exactly(
          include(name: 'chefdk-mac_os_x-10.10-tester-cd52f5'),
          include(name: 'chefdk-mac_os_x-10.11-tester-24f21f'),
          include(name: 'chefdk-macos-10.12-tester-f53697'),
          include(name: 'chefdk-macos-10.13-tester-46912e'),
          include(name: 'insights.chef.co'),
        )
      end
    end

    #######################################################################
    # Verifying returned values
    #######################################################################
    describe 'cfgmgmt/nodes' do
      let(:request_params) do
        {
          filter: 'node_id:5c3de3cf-fc36-492c-9148-c03e2312e105',
        }
      end

      it 'should match correct values' do
        expect(api_request.http_status).to eq 200
        expect(api_request.parsed_response_body.length).to eq 1
        node = api_request.parsed_response_body[0]
        expect(node[:id]).to eq '5c3de3cf-fc36-492c-9148-c03e2312e105'
        expect(node[:name]).to eq 'chefdk-debian-8-tester-79e320'
        expect(node[:fqdn]).to eq 'ip-121-215-9-40.us-west-2.compute.internal'
        expect(node[:uptime_seconds]).to eq 5489143
        expect(node[:organization]).to eq 'chef'
        expect(node[:environment]).to eq 'manhattan'
        expect(node[:platform]).to eq 'debian 8.0'
        expect(node[:platform_family]).to eq 'debian'
        expect(node[:platform_version]).to eq '8.0'
        # expect(node[:status]).to eq 'success' // not checking because it could change to 'missing'
        expect(node[:source_fqdn]).to eq 'chef-server.chef.co'
        expect(node[:latest_run_id]).to eq '326cfc2a-c675-4d87-932a-3cc944ad4305'
        expect(node[:has_runs_data]).to eq true
        expect(node[:last_ccr_received]).to eq '2017-11-01T21:00:26Z'
        expect(node[:policy_name]).to eq nil
        expect(node[:policy_group]).to eq nil
        expect(node[:policy_revision]).to eq nil
        expect(node[:has_deprecations]).to eq true
        expect(node[:deprecations_count]).to eq 10
      end
    end

    describe '/cfgmgmt/nodes/{node_id}/runs/{run_id} failed run' do
      let(:request_node_id) do
        '0271e125-97dd-498a-b026-8448ll60aafe'
      end
  
      let(:request_run_id) do
        'ba6acb91-1eaa-4c84-8d68-f19ee641e606'
      end

      it 'should match correct values' do
        expect(api_runs_request.http_status).to eq 200
        run = api_runs_request.parsed_response_body

        expect(run[:id]).to eq 'ba6acb91-1eaa-4c84-8d68-f19ee641e606'

        expect(run[:error][:class]).to eq 'Chef::Exceptions::EnclosingDirectoryDoesNotExist'
        expect(run[:error][:message]).to eq "file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist."
        expect(run[:error][:backtrace].length).to eq 53
        expect(run[:error][:backtrace]).to eq [
          "/Users/bob/projects/chef/lib/chef/mixin/why_run.rb:240:in `run'",
          "/Users/bob/projects/chef/lib/chef/mixin/why_run.rb:321:in `block in run'",
          "/Users/bob/projects/chef/lib/chef/mixin/why_run.rb:320:in `each'",
          "/Users/bob/projects/chef/lib/chef/mixin/why_run.rb:320:in `run'",
          "/Users/bob/projects/chef/lib/chef/provider.rb:155:in `process_resource_requirements'",
          "/Users/bob/projects/chef/lib/chef/provider.rb:133:in `run_action'",
          "/Users/bob/projects/chef/lib/chef/resource.rb:591:in `run_action'",
          "/Users/bob/projects/chef/lib/chef/runner.rb:69:in `run_action'",
          "/Users/bob/projects/chef/lib/chef/runner.rb:97:in `block (2 levels) in converge'",
          "/Users/bob/projects/chef/lib/chef/runner.rb:97:in `each'",
          "/Users/bob/projects/chef/lib/chef/runner.rb:97:in `block in converge'",
          "/Users/bob/projects/chef/lib/chef/resource_collection/resource_list.rb:94:in `block in execute_each_resource'",
          "/Users/bob/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:116:in `call'",
          "/Users/bob/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:116:in `call_iterator_block'",
          "/Users/bob/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:85:in `step'",
          "/Users/bob/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:104:in `iterate'",
          "/Users/bob/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:55:in `each_with_index'",
          "/Users/bob/projects/chef/lib/chef/resource_collection/resource_list.rb:92:in `execute_each_resource'",
          "/Users/bob/projects/chef/lib/chef/runner.rb:96:in `converge'",
          "/Users/bob/projects/chef/lib/chef/client.rb:669:in `block in converge'",
          "/Users/bob/projects/chef/lib/chef/client.rb:664:in `catch'",
          "/Users/bob/projects/chef/lib/chef/client.rb:664:in `converge'",
          "/Users/bob/projects/chef/lib/chef/client.rb:703:in `converge_and_save'",
          "/Users/bob/projects/chef/lib/chef/client.rb:283:in `run'",
          "/Users/bob/projects/chef/lib/chef/application.rb:286:in `block in fork_chef_client'",
          "/Users/bob/projects/chef/lib/chef/application.rb:274:in `fork'",
          "/Users/bob/projects/chef/lib/chef/application.rb:274:in `fork_chef_client'",
          "/Users/bob/projects/chef/lib/chef/application.rb:239:in `block in run_chef_client'",
          "/Users/bob/projects/chef/lib/chef/local_mode.rb:44:in `with_server_connectivity'",
          "/Users/bob/projects/chef/lib/chef/application.rb:227:in `run_chef_client'",
          "/Users/bob/projects/chef/lib/chef/application/client.rb:456:in `sleep_then_run_chef_client'",
          "/Users/bob/projects/chef/lib/chef/application/client.rb:443:in `block in interval_run_chef_client'",
          "/Users/bob/projects/chef/lib/chef/application/client.rb:442:in `loop'",
          "/Users/bob/projects/chef/lib/chef/application/client.rb:442:in `interval_run_chef_client'",
          "/Users/bob/projects/chef/lib/chef/application/client.rb:426:in `run_application'",
          "/Users/bob/projects/chef/lib/chef/application.rb:59:in `run'",
          "/Users/bob/projects/chef/bin/chef-client:26:in `<top (required)>'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/bin/chef-client:22:in `load'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/bin/chef-client:22:in `<top (required)>'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli/exec.rb:63:in `load'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli/exec.rb:63:in `kernel_load'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli/exec.rb:24:in `run'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli.rb:304:in `exec'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor/command.rb:27:in `run'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor/invocation.rb:126:in `invoke_command'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor.rb:359:in `dispatch'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor/base.rb:440:in `start'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli.rb:11:in `start'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/exe/bundle:27:in `block in <top (required)>'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/friendly_errors.rb:98:in `with_friendly_errors'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/exe/bundle:19:in `<top (required)>'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/bin/bundle:22:in `load'",
          "/Users/bob/.chefdk/gem/ruby/2.1.0/bin/bundle:22:in `<main>'"
       ]
        expect(run[:error][:description][:title]).to eq "Error executing action `create` on resource 'file[/failed/file/resource]'"
        expect(run[:error][:description][:sections].length).to eq 3
        expect(run[:error][:description][:sections][0][:"Chef::Exceptions::EnclosingDirectoryDoesNotExist"]).to eq "Parent directory /failed/file does not exist."
        expect(run[:error][:description][:sections].any? do | section |
          section.has_key?(:"Chef::Exceptions::EnclosingDirectoryDoesNotExist") && 
          section[:"Chef::Exceptions::EnclosingDirectoryDoesNotExist"] == "Parent directory /failed/file does not exist."
        end).to be true

        expect(run[:error][:description][:sections].any? do | section |
          section.has_key?(:"Resource Declaration:") && 
          section[:"Resource Declaration:"] == "# In /Users/bob/file_cache_path/cookbooks/insights-test/recipes/default.rb\n\n 26: file '/failed/file/resource' do\n 27:   action :create\n 28:   not_if { ENV['MID_RUN_FAILURE_IS_OK'].nil? }\n 29: end\n 30: \n"
        end).to be true

        expect(run[:error][:description][:sections].any? do | section |
          section.has_key?(:"Compiled Resource:") && 
          section[:"Compiled Resource:"] == "# Declared in /Users/bob/file_cache_path/cookbooks/insights-test/recipes/default.rb:26:in `from_file'\n\nfile(\"/failed/file/resource\") do\n  action [:create]\n  retries 0\n  retry_delay 2\n  default_guard_interpreter :default\n  declared_type :file\n  cookbook_name \"insights-test\"\n  recipe_name \"default\"\n  path \"/failed/file/resource\"\n  atomic_update true\n  not_if { #code block }\nend\n"
        end).to be true
      end
    end

    describe '/cfgmgmt/nodes/{node_id}/runs/{run_id} testing ignore_failure resource with quit' do
      let(:request_node_id) do
        'de8e5e71-f5f4-4d8b-85bb-41c85a88b339'
      end

      let(:request_run_id) do
        '90fab2b8-55d1-4f73-bbc5-4f4eb682c0dc'
      end

      it 'should match correct values' do
        expect(api_runs_request.http_status).to eq 200
        run = api_runs_request.parsed_response_body

        expect(run[:resources][0][:ignore_failure]).to eq true # quiet
        expect(run[:resources][1][:ignore_failure]).to eq false # missing
        expect(run[:resources][2][:ignore_failure]).to eq false # boolean
        expect(run[:resources][3][:ignore_failure]).to eq true # boolean
        expect(run[:resources][4][:ignore_failure]).to eq true # string
        expect(run[:resources][5][:ignore_failure]).to eq false # string
      end
    end

    describe '/cfgmgmt/nodes/{node_id}/runs/{run_id} successful run' do
      let(:request_node_id) do
        '82760210-4686-497e-b039-efca78dee64b'
      end
  
      let(:request_run_id) do
        '639844f4-2ce6-42ba-8c9d-853db69adff3'
      end

      it 'should match correct values' do
        expect(api_runs_request.http_status).to eq 200
        run = api_runs_request.parsed_response_body
        expect(run[:id]).to eq '639844f4-2ce6-42ba-8c9d-853db69adff3'
        expect(run[:node_id]).to eq '82760210-4686-497e-b039-efca78dee64b'
        expect(run[:node_name]).to eq 'chefdk-debian-7-tester-2d206b'
        expect(run[:organization]).to eq 'chef'
        expect(run[:start_time]).to eq '2017-11-01T20:58:52Z'
        expect(run[:end_time]).to eq '2017-11-01T20:59:31Z'
        expect(run[:source]).to eq 'chef_client'
        # expect(run[:status]).to eq 'success' // not checking because it could change to 'missing'
        expect(run[:total_resource_count]).to eq 352
        expect(run[:updated_resource_count]).to eq 11
        expect(run[:chef_version]).to eq '12.21.3'
        expect(run[:uptime_seconds]).to eq 12864819
        expect(run[:environment]).to eq 'manhattan'
        expect(run[:fqdn]).to eq 'ip-242-58-187-217.us-west-2.compute.internal'
        expect(run[:source_fqdn]).to eq 'chef-server.chef.co'
        expect(run[:ipaddress]).to eq '242.58.187.217'

        expect(run[:resources][12][:type]).to eq 'directory'
        expect(run[:resources][12][:name]).to eq '/export/home'
        expect(run[:resources][12][:id]).to eq '/export/home'
        expect(run[:resources][12][:duration]).to eq '0'
        expect(run[:resources][12][:delta]).to eq ''
        expect(run[:resources][12][:cookbook_name]).to eq 'omnibus'
        expect(run[:resources][12][:cookbook_version]).to eq '5.2.2'
        expect(run[:resources][12][:status]).to eq 'skipped'
        expect(run[:resources][12][:recipe_name]).to eq '_user'
        expect(run[:resources][12][:result]).to eq 'create'
        expect(run[:resources][12][:conditional]).to eq 'only_if { #code block }'
        expect(run[:resources][12][:ignore_failure]).to eq false

        found_resource = run[:resources].find { | resource | resource[:id] == "/home/jenkins/.gitconfig" }
        expect(found_resource).not_to be nil
        expect(found_resource[:type]).to eq 'file'
        expect(found_resource[:name]).to eq '/home/jenkins/.gitconfig'
        expect(found_resource[:duration]).to eq '11'
        expect(found_resource[:delta]).to eq "--- /home/jenkins/.gitconfig\t2017-11-01 20:55:38.580713329 +0000\\n+++ /home/jenkins/.chef-.gitconfig20171101-6763-kzpg7e.gitconfig\t2017-11-01 20:59:12.492966103 +0000\\n@@ -20,6 +20,4 @@\\n   autosetuprebase = always\\n [pull]\\n   rebase = preserve\\n-[http]\\n-\tsslCAinfo = /opt/omnibus-toolchain/embedded/ssl/certs/cacert.pem"
        expect(found_resource[:cookbook_name]).to eq 'omnibus'
        expect(found_resource[:cookbook_version]).to eq '5.2.2'
        expect(found_resource[:status]).to eq 'updated'
        expect(found_resource[:recipe_name]).to eq '_git'
        expect(found_resource[:result]).to eq 'create'
        expect(found_resource[:conditional]).to eq ''
        expect(found_resource[:ignore_failure]).to eq false

        expect(run[:run_list]).to eq ["recipe[opscode-ci::slave]"]

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == 'Chef::Platform.set is deprecated' && 
          depercation[:url] == 'https://docs.chef.io/deprecations_chef_platform_methods.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/jenkins/libraries/plugin.rb:443:in `<top (required)>'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == "Chef::Platform.set is deprecated" && 
          depercation[:url] == 'https://docs.chef.io/deprecations_chef_platform_methods.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/jenkins/libraries/slave.rb:410:in `<top (required)>'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == "Chef::Platform.set is deprecated" && 
          depercation[:url] == "https://docs.chef.io/deprecations_chef_platform_methods.html" && 
          depercation[:location] == "/var/chef/cache/cookbooks/jenkins/libraries/user.rb:183:in `<top (required)>'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == 'Property `sensitive` of resource `rhsm_register` overwrites an existing method. Please use a different property name. This will raise an exception in Chef 13.' && 
          depercation[:url] == 'https://docs.chef.io/deprecations_property_name_collision.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/redhat_subscription_manager/libraries/rhsm_register.rb:34:in `<class:RhsmRegister>'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == "method access to node attributes (node.foo.bar) is deprecated and will be removed in Chef 13, please use bracket syntax (node[\"foo\"][\"bar\"])" &&
          depercation[:url] == 'https://docs.chef.io/deprecations_attributes.html' && 
          depercation[:location] == "/opt/chef/embedded/lib/ruby/gems/2.3.0/gems/chef-sugar-3.6.0/lib/chef/sugar/node.rb:162:in `method_missing'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == 'Property `sensitive` of resource `aws_s3_file` overwrites an existing method. Please use a different property name. This will raise an exception in Chef 13.' && 
          depercation[:url] == 'https://docs.chef.io/deprecations_property_name_collision.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/aws/resources/s3_file.rb:36:in `class_from_file'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == 'Property `sensitive` of resource `yum_repository` overwrites an existing method. Please use a different property name. This will raise an exception in Chef 13.' && 
          depercation[:url] == 'https://docs.chef.io/deprecations_property_name_collision.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/yum/resources/repository.rb:59:in `class_from_file'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == "Cloning resource attributes for directory[/usr/local/bin] from prior resource\nPrevious directory[/usr/local/bin]: /var/chef/cache/cookbooks/opscode-ci/recipes/_platform_tweaks.rb:23:in `from_file'\nCurrent  directory[/usr/local/bin]: /var/chef/cache/cookbooks/omnibus/recipes/_common.rb:49:in `from_file'" && 
          depercation[:url] == 'https://docs.chef.io/deprecations_resource_cloning.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/omnibus/recipes/_common.rb:49:in `from_file'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == "Cloning resource attributes for apt_repository[openjdk-r] from prior resource\nPrevious apt_repository[openjdk-r]: /var/chef/cache/cookbooks/opscode-ci/recipes/_platform_tweaks.rb:37:in `from_file'\nCurrent  apt_repository[openjdk-r]: /var/chef/cache/cookbooks/opscode-ci/recipes/_java.rb:22:in `from_file'" &&
          depercation[:url] == 'https://docs.chef.io/deprecations_resource_cloning.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/opscode-ci/recipes/_java.rb:22:in `from_file'"
        end).to be true

        expect(run[:deprecations].any? do | depercation |
          depercation[:message] == "Cloning resource attributes for directory[/home/jenkins/.ssh] from prior resource\nPrevious directory[/home/jenkins/.ssh]: /var/chef/cache/cookbooks/opscode-ci/recipes/_github.rb:12:in `from_file'\nCurrent  directory[/home/jenkins/.ssh]: /var/chef/cache/cookbooks/opscode-ci/recipes/slave.rb:58:in `from_file'" && 
          depercation[:url] == 'https://docs.chef.io/deprecations_resource_cloning.html' && 
          depercation[:location] == "/var/chef/cache/cookbooks/opscode-ci/recipes/slave.rb:58:in `from_file'"
        end).to be true

        expect(run[:error][:class]).to eq ''
        expect(run[:error][:message]).to eq ''
        expect(run[:error][:backtrace]).to eq []
        expect(run[:error][:description]).to be nil

        expect(run[:tags]).to eq []

        expected_resource_names = ['chef-sugar', 'chef-sugar', '/usr/local/bin', '/usr/local/bin/git', 'openjdk-r', 'update', 'wget', '/etc/profile.d/rbenv.sh', '/opt/rbenv', '/opt/rubies', '/home/jenkins/.bashrc.d/chruby-default.sh', 'omnibus-toolchain',
          '/export/home', 'create jenkins group', 'chsec_login_shell', 'jenkins', 'jenkins', '/home/jenkins', '/var/chef/cache', '/usr/local/bin', '/var/cache/omnibus', 'install_packages', '/home/jenkins/.gitconfig',
          '/opt/omnibus-toolchain/bin/git config --global http.sslCAinfo /opt/omnibus-toolchain/embedded/ssl/certs/cacert.pem', 'disable strict host key checking for github.com', 'make sudo honor ssh_auth_sock',
          'devscripts', 'dpkg-dev', 'ncurses-dev', 'zlib1g-dev', 'fakeroot', '/home/jenkins/load-omnibus-toolchain.sh', '/home/jenkins/.ssh', '/home/jenkins/.ssh/known_hosts', '/home/jenkins/.ssh/github', '/home/jenkins/.ssh/config', 'ntp', 
          'Remove ntpdate', '/var/lib/ntp', '/var/log/ntpstats/', '/etc/ntp.leapseconds', '/etc/ntp.conf', 'Force sync hardware clock with system clock', 'ntp', 'ntp', 'Package signing not implemented for this platform_family', '/home/jenkins/.gem',
          '/home/jenkins/.gem/credentials', 'global', '/home/jenkins/.gemrc', '/home/jenkins/.bundle/config', '/home/jenkins/.aws', '/home/jenkins/.aws/credentials',
          'root', 'sudo', '/etc/sudoers.d', '/etc/sudoers.d/README', '/etc/sudoers', 'jenkins', 'sysadmin', 'build', 'vagrant', 'ubuntu', 'ec2-user', 'openjdk-r', 'openjdk-7-jdk', '/var/chef/cache/artifactory', 'build-artifactory-gem', 'install-artifactory-gem', '/var/chef/cache/omnibus', 'build-omnibus-gem', 'install-omnibus-gem', '/home/jenkins/omnibus-publish.rb', '/home/jenkins/.ssh', '/home/jenkins/.ssh/authorized_keys', '/var/chef/cache/jenkins-cli.jar', '/var/chef/cache/update-center.json', '/var/chef/cache/extracted-update-center.json', '/var/chef/cache/jenkins-key', 'jenkins', 'chefdk-debian-7-tester-2d206b', 'chefautomate', '/var/opt/chef/bin', '/var/opt/chef/etc', '/var/log/chef/automate-liveness-agent', '/var/opt/chef/bin/automate-liveness-agent', '/var/opt/chef/etc/config.json', '/etc/init.d/automate-liveness-agent', 'automate-liveness-agent', 'automate-liveness-agent']
        expect(run[:resource_names].length).to eq expected_resource_names.length
        expect(run[:resource_names]).to eq expected_resource_names

        expected_recipes = [
          "opscode-ci::slave",
          "opscode-ci::_build_support",
          "chef-sugar::default",
          "opscode-ci::_platform_tweaks",
          "opscode-ci::_migration",
          "omnibus::default",
          "omnibus::_common",
          "omnibus::_user",
          "omnibus::_omnibus_toolchain",
          "omnibus::_compile",
          "build-essential::default",
          "omnibus::_git",
          "omnibus::_github",
          "omnibus::_libffi",
          "omnibus::_packaging",
          "omnibus::_selinux",
          "omnibus::_environment",
          "opscode-ci::_github",
          "opscode-ci::_ntp",
          "ntp::default",
          "opscode-ci::_package_signing",
          "opscode-ci::_rubygems",
          "opscode-ci::_aws",
          "opscode-ci::_users",
          "opscode-ci::_sudo",
          "sudo::default",
          "opscode-ci::_java",
          "aws::default",
          "opscode-ci::_omnibus"
        ]
        expected_recipes.each { | c | expect( run[:recipes] ).to include( c ) }
        run[:recipes].each { | c | expect( expected_recipes ).to include( c ) }

        expected_chef_tags = [
          "chefdk",
          "slave",
          "tester"
        ]

        expected_chef_tags.each { | c | expect( run[:chef_tags] ).to include( c ) }
        run[:chef_tags].each { | c | expect( expected_chef_tags ).to include( c ) }

        expected_cookbooks = ['sudo', 'yum', 'rubygems', 'wix', 'packagecloud', 'chef_nginx', 'ohai', 'omnibus', 'remote_install', 
          'redhat_subscription_manager', 'opscode-ci', 'git', 'dmg', 'runit', 'mingw', 'chef-ingredient', 
          'windows-sdk', 'ntp', 'aws', 'chef-sugar', 'freebsd', 'lvm', 'compat_resource', 'apt', 'seven_zip', 
          'homebrew', 'jenkins', 'bluepill', 'build-essential', 'yum-epel', 'route53', 'rsyslog', 'zypper', 'windows']

        expected_cookbooks.each { | c | expect( run[:cookbooks] ).to include( c ) }
        run[:cookbooks].each { | c | expect( expected_cookbooks ).to include( c ) }
         
        expect(run[:platform]).to eq 'debian 7.8'
        expect(run[:platform_family]).to eq 'debian'
        expect(run[:platform_version]).to eq '7.8'
        expect(run[:roles]).to eq [
          "test",
          "test1"
        ]
        expect(run[:policy_name]).to eq 'policy_name1'
        expect(run[:policy_group]).to eq 'policy_group1'
        expect(run[:policy_revision]).to eq 'policy_revision1'
        expect(run[:expanded_run_list][:id]).to eq 'manhattan'
        expect(run[:expanded_run_list][:run_list].length).to eq 1
        expect(run[:expanded_run_list][:run_list][0][:type]).to eq 'recipe'
        expect(run[:expanded_run_list][:run_list][0][:name]).to eq 'opscode-ci::slave'
        expect(run[:expanded_run_list][:run_list][0][:version]).to eq ''
        expect(run[:expanded_run_list][:run_list][0][:skipped]).to eq false
      end
    end

    describe '/cfgmgmt/nodes/{node_id}/runs/{run_id} node with role run' do
      let(:request_node_id) do
        'f6a5c33f-bef5-433b-815e-a8f6e69e6b1b'
      end
  
      let(:request_run_id) do
        '7188b88b-2236-4e27-a875-d3a10a70c497'
      end

      it 'should match correct values' do
        expect(api_runs_request.http_status).to eq 200
        run = api_runs_request.parsed_response_body

        expect(run[:expanded_run_list][:id]).to eq '_default'
        expect(run[:expanded_run_list][:run_list].length).to eq 1
        expect(run[:expanded_run_list][:run_list][0][:type]).to eq 'role'
        expect(run[:expanded_run_list][:run_list][0][:name]).to eq 'web'
        expect(run[:expanded_run_list][:run_list][0][:version]).to eq ''
        expect(run[:expanded_run_list][:run_list][0][:skipped]).to eq false
        run_list_children = run[:expanded_run_list][:run_list][0][:children]
        expect(run_list_children.length).to eq 3
        expect(run_list_children[0][:type]).to eq 'recipe'
        expect(run_list_children[0][:name]).to eq 'chef-client::default'
        expect(run_list_children[0][:version]).to eq ''
        expect(run_list_children[0][:skipped]).to eq false
        expect(run_list_children[1][:type]).to eq 'recipe'
        expect(run_list_children[1][:name]).to eq 'chef-client::delete_validation'
        expect(run_list_children[1][:version]).to eq ''
        expect(run_list_children[1][:skipped]).to eq false
        expect(run_list_children[2][:type]).to eq 'recipe'
        expect(run_list_children[2][:name]).to eq 'lamp2::default'
        expect(run_list_children[2][:version]).to eq ''
        expect(run_list_children[2][:skipped]).to eq false
      end
    end

    #######################################################################
    # Deleting
    # #####################################################################
    describe 'ingest/events/chef/node-multiple-deletes' do
      let(:delete_request_params) do
        {
          'node_ids': [
            '5c3de3cf-fc36-492c-9148-c03e2312e105', 
            'f6a5c33f-bef5-433b-815e-a8f6e69e6b1b',
            '82760210-4686-497e-b039-efca78dee64b',
            '479c5129-61f5-4fb7-81db-686faf2ebdd9',
            '1002de74-07fd-464a-9b7e-170022df2056',
            '867cbcdd-f1d0-4878-8ce7-adcf565eb2d8',
            'd348b61a-a37a-4653-8786-b90739ea6cc5',
            'fcd0f65c-09b9-4430-8358-ebf145df12d5',
            'f02d1dc5-79d5-4067-892d-b8e754c12b68',
            'fe19731c-705e-4b0d-9e19-bee86116063e',
            '9c139ad0-89a5-44bc-942c-d7f248b155ba',
            '6453a764-2415-4934-8cee-2a008834a74a',
            '05309d34-fb13-4587-8dc7-948807e50090',
            'cc6b66ae-a047-4818-b234-65ef3a873649',
            '88910647-2a58-43b7-88ce-7a957a1f3aed',
            '0c4fe6a0-4e17-4a89-bc6d-7ca61e4f0c7b',
            '0271e125-97dd-498a-b026-8448ll60aafe',
            'de8e5e71-f5f4-4d8b-85bb-41c85a88b339',
          ],
        }
      end
      let(:api_delete_request) do
        automate_api_request(
          '/api/v0/ingest/events/chef/node-multiple-deletes',
          request_body: delete_request_params.to_json,
          http_method: 'POST'
        )
      end
      # This test should be ran last because it deletes nodes that are used in the above tests
      it 'Should remove all nodes' do
        expect(api_delete_request.http_status).to eq 200
        expect(api_request.http_status).to eq 200
        expect(api_request.parsed_response_body).to eq []
      end
    end
  end
end
