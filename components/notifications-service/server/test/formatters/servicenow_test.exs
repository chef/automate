defmodule Notifications.Formatters.ServiceNow.Test do
  use ExUnit.Case, async: false
  doctest Notifications.Formatters.ServiceNow

  alias Notifications.Formatters.ServiceNow

  describe "#format" do
    test "that a populated CCR notification creates the correct webhook paylaod" do
      exception_backtrace = [
            "/Users/aleff/projects/chef/lib/chef/mixin/why_run.rb:240:in `run'",
            "/Users/aleff/projects/chef/lib/chef/mixin/why_run.rb:321:in `block in run'",
            "/Users/aleff/projects/chef/lib/chef/mixin/why_run.rb:320:in `each'",
            "/Users/aleff/projects/chef/lib/chef/mixin/why_run.rb:320:in `run'",
            "/Users/aleff/projects/chef/lib/chef/provider.rb:155:in `process_resource_requirements'",
            "/Users/aleff/projects/chef/lib/chef/provider.rb:133:in `run_action'",
            "/Users/aleff/projects/chef/lib/chef/resource.rb:591:in `run_action'",
            "/Users/aleff/projects/chef/lib/chef/runner.rb:69:in `run_action'",
            "/Users/aleff/projects/chef/lib/chef/runner.rb:97:in `block (2 levels) in converge'",
            "/Users/aleff/projects/chef/lib/chef/runner.rb:97:in `each'",
            "/Users/aleff/projects/chef/lib/chef/runner.rb:97:in `block in converge'",
            "/Users/aleff/projects/chef/lib/chef/resource_collection/resource_list.rb:94:in `block in execute_each_resource'",
            "/Users/aleff/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:116:in `call'",
            "/Users/aleff/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:116:in `call_iterator_block'",
            "/Users/aleff/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:85:in `step'",
            "/Users/aleff/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:104:in `iterate'",
            "/Users/aleff/projects/chef/lib/chef/resource_collection/stepable_iterator.rb:55:in `each_with_index'",
            "/Users/aleff/projects/chef/lib/chef/resource_collection/resource_list.rb:92:in `execute_each_resource'",
            "/Users/aleff/projects/chef/lib/chef/runner.rb:96:in `converge'",
            "/Users/aleff/projects/chef/lib/chef/client.rb:669:in `block in converge'",
            "/Users/aleff/projects/chef/lib/chef/client.rb:664:in `catch'",
            "/Users/aleff/projects/chef/lib/chef/client.rb:664:in `converge'",
            "/Users/aleff/projects/chef/lib/chef/client.rb:703:in `converge_and_save'",
            "/Users/aleff/projects/chef/lib/chef/client.rb:283:in `run'",
            "/Users/aleff/projects/chef/lib/chef/application.rb:286:in `block in fork_chef_client'",
            "/Users/aleff/projects/chef/lib/chef/application.rb:274:in `fork'",
            "/Users/aleff/projects/chef/lib/chef/application.rb:274:in `fork_chef_client'",
            "/Users/aleff/projects/chef/lib/chef/application.rb:239:in `block in run_chef_client'",
            "/Users/aleff/projects/chef/lib/chef/local_mode.rb:44:in `with_server_connectivity'",
            "/Users/aleff/projects/chef/lib/chef/application.rb:227:in `run_chef_client'",
            "/Users/aleff/projects/chef/lib/chef/application/client.rb:456:in `sleep_then_run_chef_client'",
            "/Users/aleff/projects/chef/lib/chef/application/client.rb:443:in `block in interval_run_chef_client'",
            "/Users/aleff/projects/chef/lib/chef/application/client.rb:442:in `loop'",
            "/Users/aleff/projects/chef/lib/chef/application/client.rb:442:in `interval_run_chef_client'",
            "/Users/aleff/projects/chef/lib/chef/application/client.rb:426:in `run_application'",
            "/Users/aleff/projects/chef/lib/chef/application.rb:59:in `run'",
            "/Users/aleff/projects/chef/bin/chef-client:26:in `<top (required)>'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/bin/chef-client:22:in `load'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/bin/chef-client:22:in `<top (required)>'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli/exec.rb:63:in `load'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli/exec.rb:63:in `kernel_load'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli/exec.rb:24:in `run'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli.rb:304:in `exec'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor/command.rb:27:in `run'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor/invocation.rb:126:in `invoke_command'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor.rb:359:in `dispatch'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/vendor/thor/lib/thor/base.rb:440:in `start'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/cli.rb:11:in `start'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/exe/bundle:27:in `block in <top (required)>'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/lib/bundler/friendly_errors.rb:98:in `with_friendly_errors'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/gems/bundler-1.12.5/exe/bundle:19:in `<top (required)>'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/bin/bundle:22:in `load'",
            "/Users/aleff/.chefdk/gem/ruby/2.1.0/bin/bundle:22:in `<main>'"
          ]

      notification = %Notifications.CCRFailure{
        run_id: "",
        node_name: "insights.chef.co",
        node_url: "chef-server.insights.co",
        run_url: "https://localhost/nodes/0271e125-97dd-498a-b026-8448ee60aafe/runs/ba6acb91-1eaa-4c84-8d68-f19ee641e606",
        cookbook: "test-cookbook",
        recipe: "",
        time: %Notifications.TimeInfo{start_time: "2016-06-28T20:05:29.000000Z", end_time: "2016-06-28T20:05:30.000000Z"},
        timestamp: "2016-06-28T20:05:31.000000Z",
        exception: %Notifications.ExceptionInfo{class: "", 
          title: "Error executing action `create` on resource 'file[/failed/file/resource]'", 
          msg: "file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist.",
          backtrace: exception_backtrace}}

      expected =
        %{automate_failure_url: "https://localhost/nodes/0271e125-97dd-498a-b026-8448ee60aafe/runs/ba6acb91-1eaa-4c84-8d68-f19ee641e606",
          automate_fqdn: "http://localhost",
          cookbook: "test-cookbook",
          end_time_utc: "2016-06-28T20:05:30.000000Z",
          exception_message: "file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist.",
          exception_title: "Error executing action `create` on resource 'file[/failed/file/resource]'",
          failure_snippet: "Chef client run failure on [chef-server.insights.co] insights.chef.co : https://localhost/nodes/0271e125-97dd-498a-b026-8448ee60aafe/runs/ba6acb91-1eaa-4c84-8d68-f19ee641e606\nError executing action `create` on resource 'file[/failed/file/resource]'\nfile[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist. \n",
          node_name: "insights.chef.co", start_time_utc: "2016-06-28T20:05:29.000000Z",
          type: "converge_failure",
          timestamp_utc: "2016-06-28T20:05:31.000000Z",
          exception_backtrace: exception_backtrace
        }

      assert expected == ServiceNow.format(notification)
    end
  end
end
