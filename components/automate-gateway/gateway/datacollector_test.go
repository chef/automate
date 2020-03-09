// +build !mockgen

//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package gateway_test

import (
	"io/ioutil"
	"testing"

	structpb "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"

	ingestProto "github.com/chef/automate/api/external/ingest/request"
	inspecEvent "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	subject "github.com/chef/automate/components/automate-gateway/gateway"
)

func readExample(path string) []byte {
	content, err := ioutil.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return content
}

func TestDataCollectorUnmarshalProtoFromString(t *testing.T) {
	str := "{\"id\":\"1\"}"
	var pbStruct structpb.Struct
	err := subject.UnmarshalProtoFromString(str, &pbStruct)
	assert.Nil(t, err)
	expectedDataField := structpb.Struct{
		Fields: map[string]*structpb.Value{
			"id": {Kind: &structpb.Value_StringValue{"1"}},
		},
	}
	assert.Equal(t, pbStruct, expectedDataField)
}

func TestDataCollectorUnmarshalProtoFromBytes(t *testing.T) {
	bytes := []byte(`{"id":"1"}`)
	var pbStruct structpb.Struct
	err := subject.UnmarshalProtoFromBytes(bytes, &pbStruct)
	assert.Nil(t, err)
	expectedDataField := structpb.Struct{
		Fields: map[string]*structpb.Value{
			"id": {Kind: &structpb.Value_StringValue{"1"}},
		},
	}
	assert.Equal(t, pbStruct, expectedDataField)
}

func TestDataCollectorParseBytesToChefRunFailure(t *testing.T) {
	r := readExample("../../ingest-service/examples/converge-failure-report.json")
	run := subject.ParseBytesToChefRun(r)
	assert.Equal(t, run.Id, "ba6acb91-1eaa-4c84-8d68-f19ee641e606")
	assert.Equal(t, run.RunId, "ba6acb91-1eaa-4c84-8d68-f19ee641e606")
	assert.Equal(t, run.EntityUuid, "0271e125-97dd-498a-b026-8448ee60aafe")
	assert.Equal(t, run.MessageVersion, "1.0.0")
	assert.Equal(t, run.MessageType, "run_converge")
	assert.Equal(t, run.NodeName, "insights.chef.co")
	assert.Equal(t, run.StartTime, "2016-06-28T20:05:29Z")
	assert.Equal(t, run.ChefServerFqdn, "example.com")
	assert.Equal(t, run.EndTime, "2016-06-28T20:05:30Z")
	assert.Equal(t, run.Status, "failure")
	assert.Equal(t, run.UpdatedResourceCount, int32(4))
	// Verify the 'Error' field
	assert.Equal(t, run.Error.Class, "Chef::Exceptions::EnclosingDirectoryDoesNotExist")
	assert.Equal(t, run.Error.Message, "file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist.")
	expectedBacktrace := []string{
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
		"/Users/bob/.chefdk/gem/ruby/2.1.0/bin/bundle:22:in `<main>'",
	}
	assert.Equal(t, expectedBacktrace, run.Error.Backtrace)
	expectedDescription := &ingestProto.Description{
		Title: "Error executing action `create` on resource 'file[/failed/file/resource]'",
		Sections: []*structpb.Struct{
			{
				Fields: map[string]*structpb.Value{
					"Chef::Exceptions::EnclosingDirectoryDoesNotExist": {
						Kind: &structpb.Value_StringValue{"Parent directory /failed/file does not exist."}},
				},
			},
			{
				Fields: map[string]*structpb.Value{
					"Resource Declaration:": {
						Kind: &structpb.Value_StringValue{"# In /Users/bob/file_cache_path/cookbooks/insights-test/recipes/default.rb\n\n 26: file '/failed/file/resource' do\n 27:   action :create\n 28:   not_if { ENV['MID_RUN_FAILURE_IS_OK'].nil? }\n 29: end\n 30: \n"}},
				},
			},
			{
				Fields: map[string]*structpb.Value{
					"Compiled Resource:": {
						Kind: &structpb.Value_StringValue{"# Declared in /Users/bob/file_cache_path/cookbooks/insights-test/recipes/default.rb:26:in `from_file'\n\nfile(\"/failed/file/resource\") do\n  action [:create]\n  retries 0\n  retry_delay 2\n  default_guard_interpreter :default\n  declared_type :file\n  cookbook_name \"insights-test\"\n  recipe_name \"default\"\n  path \"/failed/file/resource\"\n  atomic_update true\n  not_if { #code block }\nend\n"}},
				},
			},
		},
	}
	assert.Equal(t, expectedDescription, run.Error.Description)

	// Verify the 'Resources' field
	assert.Len(t, run.Resources, 7)

	res1 := run.Resources[0]
	assert.Equal(t, "file", res1.Type)
	assert.Equal(t, "/tmp/test.txt", res1.Name)
	assert.Equal(t, "/tmp/test.txt", res1.Id)
	expectedAfterForRes1 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"owner": {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"group": {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"mode":  {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"path":  {Kind: &structpb.Value_StringValue{"/tmp/test.txt"}},
		},
	}
	assert.Equal(t, expectedAfterForRes1, res1.After)
	assert.Equal(t, &structpb.Struct{Fields: map[string]*structpb.Value{}}, res1.Before)
	assert.Equal(t, "0", res1.Duration)
	assert.Equal(t, "", res1.Delta)
	assert.Equal(t, "insights-test", res1.CookbookName)
	assert.Equal(t, "0.1.1", res1.CookbookVersion)
	assert.Equal(t, true, res1.IgnoreFailure.GetBoolValue())
	assert.Equal(t, "skipped", res1.Status)
	assert.Equal(t, "", res1.RecipeName)
	assert.Equal(t, "not_if { action == :nothing }", res1.Conditional)
	assert.Equal(t, "nothing", res1.Result)

	res2 := run.Resources[1]
	assert.Equal(t, "execute", res2.Type)
	assert.Equal(t, "ls", res2.Name)
	assert.Equal(t, "ls", res2.Id)
	expectedAfterForRes2 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"command": {Kind: &structpb.Value_StringValue{"ls"}},
		},
	}
	assert.Equal(t, expectedAfterForRes2, res2.After)
	assert.Equal(t, &structpb.Struct{Fields: map[string]*structpb.Value{}}, res2.Before)
	assert.Equal(t, "10", res2.Duration)
	assert.Equal(t, "", res2.Delta)
	assert.Equal(t, "insights-test", res2.CookbookName)
	assert.Equal(t, "0.1.1", res2.CookbookVersion)
	assert.Equal(t, false, res2.IgnoreFailure.GetBoolValue())
	assert.Equal(t, "updated", res2.Status)
	assert.Equal(t, "", res2.RecipeName)
	assert.Equal(t, "", res2.Conditional)
	assert.Equal(t, "run", res2.Result)

	res3 := run.Resources[2]
	assert.Equal(t, "file", res3.Type)
	assert.Equal(t, "/tmp/test.txt", res3.Name)
	assert.Equal(t, "/tmp/test.txt", res3.Id)
	expectedAfterForRes3 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"mode":          {Kind: &structpb.Value_StringValue{"0644"}},
			"path":          {Kind: &structpb.Value_StringValue{"/tmp/test.txt"}},
			"verifications": {Kind: &structpb.Value_ListValue{&structpb.ListValue{Values: []*structpb.Value{}}}},
			"checksum":      {Kind: &structpb.Value_StringValue{"8bb7b24c616a8d563e5424cb7d971041731e8201d4900dc634681cacd76ab302"}},
			"owner":         {Kind: &structpb.Value_StringValue{"bob"}},
			"group":         {Kind: &structpb.Value_StringValue{"wheel"}},
		},
	}
	assert.Equal(t, expectedAfterForRes3, res3.After)
	expectedBeforeForRes3 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"owner":    {Kind: &structpb.Value_StringValue{"bob"}},
			"group":    {Kind: &structpb.Value_StringValue{"wheel"}},
			"mode":     {Kind: &structpb.Value_StringValue{"0644"}},
			"path":     {Kind: &structpb.Value_StringValue{"/tmp/test.txt"}},
			"checksum": {Kind: &structpb.Value_StringValue{"aca43b3116075c26dccd2d183f550782a06928a0dc1cc5c6f4619e32702f9c8a"}},
		},
	}
	assert.Equal(t, expectedBeforeForRes3, res3.Before)
	assert.Equal(t, "13", res3.Duration)
	assert.Equal(t, "--- /tmp/test.txt\t2016-06-28 15:48:44.000000000 -0400\\n+++ /tmp/.chef-test.txt20160628-86667-1aox2pp\t2016-06-28 16:05:30.000000000 -0400\\n@@ -1,2 +1,2 @@\\n-570919d6-994a-4b73-8fd9-be8553e83ebc\\n+636f6f69-35ba-457b-9b02-16baca0fd99e", res3.Delta)
	assert.Equal(t, "insights-test", res3.CookbookName)
	assert.Equal(t, "0.1.1", res3.CookbookVersion)
	assert.Equal(t, false, res3.IgnoreFailure.GetBoolValue())
	assert.Equal(t, "updated", res3.Status)
	assert.Equal(t, "", res3.RecipeName)
	assert.Equal(t, "", res3.Conditional)
	assert.Equal(t, "create", res3.Result)

	res4 := run.Resources[3]
	assert.Equal(t, "file", res4.Type)
	assert.Equal(t, "/tmp/test.txt", res4.Name)
	assert.Equal(t, "/tmp/test.txt", res4.Id)
	expectedAfterForRes4 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"mode":          {Kind: &structpb.Value_StringValue{"0644"}},
			"path":          {Kind: &structpb.Value_StringValue{"/tmp/test.txt"}},
			"verifications": {Kind: &structpb.Value_ListValue{&structpb.ListValue{Values: []*structpb.Value{}}}},
			"checksum":      {Kind: &structpb.Value_StringValue{"8bb7b24c616a8d563e5424cb7d971041731e8201d4900dc634681cacd76ab302"}},
			"owner":         {Kind: &structpb.Value_StringValue{"bob"}},
			"group":         {Kind: &structpb.Value_StringValue{"wheel"}},
		},
	}
	assert.Equal(t, expectedAfterForRes4, res4.After)
	expectedBeforeForRes4 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"owner":    {Kind: &structpb.Value_StringValue{"bob"}},
			"group":    {Kind: &structpb.Value_StringValue{"wheel"}},
			"mode":     {Kind: &structpb.Value_StringValue{"0644"}},
			"path":     {Kind: &structpb.Value_StringValue{"/tmp/test.txt"}},
			"checksum": {Kind: &structpb.Value_StringValue{"8bb7b24c616a8d563e5424cb7d971041731e8201d4900dc634681cacd76ab302"}},
		},
	}
	assert.Equal(t, expectedBeforeForRes4, res4.Before)
	assert.Equal(t, "7", res4.Duration)
	assert.Equal(t, "", res4.Delta)
	assert.Equal(t, "insights-test", res4.CookbookName)
	assert.Equal(t, "0.1.1", res4.CookbookVersion)
	assert.Equal(t, false, res4.IgnoreFailure.GetBoolValue())
	assert.Equal(t, "up-to-date", res4.Status)
	assert.Equal(t, "", res4.RecipeName)
	assert.Equal(t, "", res4.Conditional)
	assert.Equal(t, "create", res4.Result)

	res5 := run.Resources[4]
	assert.Equal(t, "file", res5.Type)
	assert.Equal(t, "/tmp/always-updated.txt", res5.Name)
	assert.Equal(t, "/tmp/always-updated.txt", res5.Id)
	expectedAfterForRes5 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"mode":          {Kind: &structpb.Value_StringValue{"0644"}},
			"path":          {Kind: &structpb.Value_StringValue{"/tmp/always-updated.txt"}},
			"verifications": {Kind: &structpb.Value_ListValue{&structpb.ListValue{Values: []*structpb.Value{}}}},
			"checksum":      {Kind: &structpb.Value_StringValue{"5e8ba96dbd319f4294e95fff16c1bacfc20fdd33310ca027d9d52414efeb705c"}},
			"owner":         {Kind: &structpb.Value_StringValue{"bob"}},
			"group":         {Kind: &structpb.Value_StringValue{"wheel"}},
		},
	}
	assert.Equal(t, expectedAfterForRes5, res5.After)
	expectedBeforeForRes5 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"owner":    {Kind: &structpb.Value_StringValue{"bob"}},
			"group":    {Kind: &structpb.Value_StringValue{"wheel"}},
			"mode":     {Kind: &structpb.Value_StringValue{"0644"}},
			"path":     {Kind: &structpb.Value_StringValue{"/tmp/always-updated.txt"}},
			"checksum": {Kind: &structpb.Value_StringValue{"4a4268f12ec873565596c4531f93232b94d4efb233c2b192fc7a03a67683c908"}},
		},
	}
	assert.Equal(t, expectedBeforeForRes5, res5.Before)
	assert.Equal(t, "10", res5.Duration)
	assert.Equal(t, "--- /tmp/always-updated.txt\t2016-06-28 15:48:44.000000000 -0400\\n+++ /tmp/.chef-always-updated.txt20160628-86667-xawnof\t2016-06-28 16:05:30.000000000 -0400\\n@@ -1,2 +1,2 @@\\n-b5a16b75-edf9-4f77-95e2-b0072f4bf4f4\\n+773bdf5e-cd2d-4d2e-b82e-eb46770ba3d4", res5.Delta)
	assert.Equal(t, "insights-test", res5.CookbookName)
	assert.Equal(t, "0.1.1", res5.CookbookVersion)
	assert.Equal(t, false, res5.IgnoreFailure.GetBoolValue())
	assert.Equal(t, "updated", res5.Status)
	assert.Equal(t, "", res5.RecipeName)
	assert.Equal(t, "", res5.Conditional)
	assert.Equal(t, "create", res5.Result)

	res6 := run.Resources[5]
	assert.Equal(t, "file", res6.Type)
	assert.Equal(t, "/failed/file/resource", res6.Name)
	assert.Equal(t, "/failed/file/resource", res6.Id)
	expectedAfterForRes6 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"owner": {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"group": {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"mode":  {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"path":  {Kind: &structpb.Value_StringValue{"/failed/file/resource"}},
		},
	}
	assert.Equal(t, expectedAfterForRes6, res6.After)
	assert.Equal(t, &structpb.Struct{Fields: map[string]*structpb.Value{}}, res6.Before)
	assert.Equal(t, "5", res6.Duration)
	assert.Equal(t, "", res6.Delta)
	assert.Equal(t, false, res6.IgnoreFailure.GetBoolValue())
	assert.Equal(t, "create", res6.Result)
	assert.Equal(t, "failed", res6.Status)
	assert.Equal(t, "insights-test", res6.CookbookName)
	assert.Equal(t, "0.1.1", res6.CookbookVersion)

	res7 := run.Resources[6]
	assert.Equal(t, "file", res7.Type)
	assert.Equal(t, "/tmp/do-not-write.txt", res7.Name)
	assert.Equal(t, "/tmp/do-not-write.txt", res7.Id)
	expectedAfterForRes7 := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"owner": {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"group": {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"mode":  {Kind: &structpb.Value_NullValue{structpb.NullValue_NULL_VALUE}},
			"path":  {Kind: &structpb.Value_StringValue{"/tmp/do-not-write.txt"}},
		},
	}
	assert.Equal(t, expectedAfterForRes7, res7.After)
	assert.Equal(t, &structpb.Struct{Fields: map[string]*structpb.Value{}}, res7.Before)
	assert.Equal(t, "", res7.Duration)
	assert.Equal(t, "", res7.Delta)
	assert.Equal(t, "insights-test", res7.CookbookName)
	assert.Equal(t, "0.1.1", res7.CookbookVersion)
	assert.Equal(t, false, res7.IgnoreFailure.GetBoolValue())
	assert.Equal(t, "unprocessed", res7.Status)
	assert.Equal(t, "", res7.RecipeName)
	assert.Equal(t, "", res7.Conditional)
	assert.Equal(t, "create", res7.Result)

	// The content should be exactly the same as the bytes array read from the body
	assert.Equal(t, run.Content, r)
}

func TestDataCollectorParseBytesToChefRunSuccess(t *testing.T) {
	r := readExample("../../ingest-service/examples/chef_client_run.json")
	run := subject.ParseBytesToChefRun(r)
	assert.Equal(t, run.Id, "e2d05569-dfeb-4ff0-a5a2-863ce12cc13f")
	assert.Equal(t, run.RunId, "e2d05569-dfeb-4ff0-a5a2-863ce12cc13f")
	assert.Equal(t, run.EntityUuid, "0271e125-97dd-498a-b026-8448ee60aafe")
	assert.Equal(t, run.MessageVersion, "1.0.0")
	assert.Equal(t, run.MessageType, "run_converge")
	assert.Equal(t, run.NodeName, "example.com")
	assert.Equal(t, run.StartTime, "2016-06-28T15:13:21Z")
	assert.Equal(t, run.ChefServerFqdn, "chef-server.insights.co")
	assert.Equal(t, run.EndTime, "2016-06-28T15:13:22Z")
	assert.Equal(t, run.Status, "success")
	assert.Equal(t, run.UpdatedResourceCount, int32(4))
	var pErr *ingestProto.Error
	assert.Equal(t, run.Error, pErr)
	// The content should be exactly the same as the bytes array read from the body
	assert.Equal(t, run.Content, r)
}

func TestDataCollectorParseBytesToChefAction(t *testing.T) {
	a := readExample("../../ingest-service/examples/actions/item_bag_create.json")
	action := subject.ParseBytesToChefAction(a)
	assert.Equal(t, action.Id, "7ff39429-fe80-4818-b269-0c253e9e22ea")
	assert.Equal(t, action.MessageType, "action")
	assert.Equal(t, action.MessageVersion, "0.1.1")
	assert.Equal(t, action.EntityName, "1")
	assert.Equal(t, action.EntityType, "item")
	assert.Equal(t, action.Task, "create")
	assert.Equal(t, action.OrganizationName, "testing")
	assert.Equal(t, action.RemoteHostname, "127.0.0.1")
	assert.Equal(t, action.RecordedAt, "2018-03-28T21:25:38Z")
	assert.Equal(t, action.RemoteRequestId, "34928507-18a1-45ac-bfc8-ed1e8064cb27")
	assert.Equal(t, action.RequestId, "g3IAA2QAEGVyY2hlZkAxMjcuMC4wLjEBAAERRkRMAAIAAAAA")
	assert.Equal(t, action.RequestorName, "mkrasnow")
	assert.Equal(t, action.RequestorType, "user")
	assert.Equal(t, action.ServiceHostname, "chef-server.chef.co")
	assert.Equal(t, action.UserAgent, "Chef Manage/11.16.2 (ruby-2.3.5-p376; ohai-7.4.1; x86_64-linux; +http://opscode.com)")
	assert.Equal(t, action.ParentType, "bag")
	assert.Equal(t, action.ParentName, "big_old_bag")
	assert.Equal(t, action.Content, a)
}

func TestDataCollectorParseBytesToLivenessPing(t *testing.T) {
	l := readExample("../../ingest-service/examples/liveness_ping.json")
	liveness := subject.ParseBytesToLivenessPing(l)
	assert.Equal(t, liveness.EventType, "node_ping")
	assert.Equal(t, liveness.EntityUuid, "0271e125-97dd-498a-b026-8448ee60aafe")
	assert.Equal(t, liveness.ChefServerFqdn, "chef-server.opsworks.com")
	assert.Equal(t, liveness.Source, "liveness_agent")
	assert.Equal(t, liveness.MessageVersion, "0.0.1")
	assert.Equal(t, liveness.OrganizationName, "happytrees")
	assert.Equal(t, liveness.NodeName, "leaf")
}

func TestDataCollectorParseBytesToComplianceReport(t *testing.T) {
	// TODO Should we do the big one? -.- (compliance-failure-big-report)
	c := readExample("../../compliance-service/ingest/examples/compliance-success-tiny-report.json")
	report := subject.ParseBytesToComplianceReport(c)

	assert.Equal(t, "3.0.12", report.Version)
	expectedPlatform := &inspecEvent.Platform{Name: "centos", Release: "6.6"}
	assert.Equal(t, expectedPlatform, report.Platform)
	expectedStatistics := &inspecEvent.Statistics{Duration: float32(0.004522604)}
	assert.Equal(t, expectedStatistics, report.Statistics)
	assert.Equal(t, []string{}, report.OtherChecks)
	assert.Equal(t, "34567890-36d6-439e-ac70-a41504242605", report.ReportUuid)
	assert.Equal(t, "4567abcd-c628-4f82-86df-61c43866db6a", report.NodeUuid)
	assert.Equal(t, "12345678-36d6-439e-ac70-a41504242605", report.JobUuid)
	assert.Equal(t, "My Compliant Node", report.NodeName)
	assert.Equal(t, "DevSec Prod Beta", report.Environment)
	assert.Equal(t, []string{"base_linux", "apache_linux", "linux-hardening-prod", "dot.role"}, report.Roles)
	assert.Equal(t, []string{"apache_extras", "apache_extras::harden", "java::default", "nagios::fix"}, report.Recipes)
	assert.Equal(t, "2018-10-25T10:18:41+01:00", report.EndTime)
	assert.Equal(t, "", report.Type)
	assert.Equal(t, "", report.SourceId)
	assert.Equal(t, "", report.SourceRegion)
	assert.Equal(t, "", report.SourceAccountId)

	// Verify the 'Profiles' field
	assert.Len(t, report.Profiles, 3)

	p1 := report.Profiles[0]
	assert.Equal(t, "mylinux-success", p1.Name)
	assert.Equal(t, "My Demo Linux successful profile", p1.Title)
	assert.Equal(t, "1.8.9", p1.Version)
	assert.Equal(t, "Demonstrates the use of InSpec Compliance Profile", p1.Summary)
	assert.Equal(t, "Chef Software, Inc.", p1.Maintainer)
	assert.Equal(t, "Apache 2 license", p1.License)
	assert.Equal(t, "Chef Software, Inc.", p1.Copyright)
	assert.Equal(t, "support@chef.io", p1.CopyrightEmail)
	assert.Equal(t, "loaded", p1.Status)
	assert.Equal(t, "", p1.SkipMessage)
	assert.Equal(t, "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", p1.Sha256)
	assert.Len(t, p1.Controls, 2)
	expectedP1Support := []*inspecEvent.Support{{Inspec: "~> 1.0"}}
	assert.Len(t, p1.Supports, 1)
	assert.Equal(t, expectedP1Support, p1.Supports)
	expectedP1Attributes := []*inspecEvent.Attribute{
		{
			Name: "role_name",
			Options: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"default":     {Kind: &structpb.Value_StringValue{"base"}},
					"description": {Kind: &structpb.Value_StringValue{"Chef Role"}},
				},
			},
		},
		{
			Name: "profile_id",
			Options: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"default":     {Kind: &structpb.Value_NumberValue{1}},
					"description": {Kind: &structpb.Value_StringValue{"An int id"}},
				},
			},
		},
		{
			Name: "do.this?",
			Options: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"default":     {Kind: &structpb.Value_BoolValue{true}},
					"description": {Kind: &structpb.Value_StringValue{"A bool flag"}},
				},
			},
		},
		{
			Name: "take_this",
			Options: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"default": {Kind: &structpb.Value_ListValue{
						&structpb.ListValue{Values: []*structpb.Value{
							{Kind: &structpb.Value_StringValue{"oh"}},
							{Kind: &structpb.Value_StringValue{"hear"}},
						}},
					}},
					"description": {Kind: &structpb.Value_StringValue{"A bloody array"}},
				},
			},
		},
		{
			Name: "bloody_hash",
			Options: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"default": {Kind: &structpb.Value_StructValue{
						&structpb.Struct{
							Fields: map[string]*structpb.Value{
								"oh": {Kind: &structpb.Value_StringValue{"god"}},
							},
						},
					}},
				},
			},
		},
		{
			Name: "no_default",
			Options: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"description": {Kind: &structpb.Value_StringValue{"Default is for lazies!"}},
				},
			},
		},
	}
	assert.Len(t, p1.Attributes, 6)
	assert.Equal(t, expectedP1Attributes, p1.Attributes)
	expectedP1Dependencies := make([]*inspecEvent.Dependency, 0)
	assert.Equal(t, expectedP1Dependencies, p1.Depends)
	expectedP1Groups := []*inspecEvent.Group{
		{
			Id:    "controls/success.rb",
			Title: "",
			Controls: []string{
				"/etc/passwd must exist",
				"/etc/group must exist",
			},
		},
	}
	assert.Len(t, p1.Groups, 1)
	assert.Equal(t, expectedP1Groups, p1.Groups)
	assert.Equal(t, "", p1.ParentProfile)

	p2 := report.Profiles[1]
	assert.Equal(t, "mylinux-success2", p2.Name)
	assert.Equal(t, "My Demo Linux successful profile 2", p2.Title)
	assert.Equal(t, "1.0.0", p2.Version)
	assert.Equal(t, "Demonstrates the use of InSpec Compliance Profile", p2.Summary)
	assert.Equal(t, "Chef Software, Inc.", p2.Maintainer)
	assert.Equal(t, "Apache 2 license", p2.License)
	assert.Equal(t, "Chef Software, Inc.", p2.Copyright)
	assert.Equal(t, "support@chef.io", p2.CopyrightEmail)
	assert.Equal(t, "9490b16f32922b284a82a36d4f111e1474fcd9b53c4689f77de7ef68a1664487", p2.Sha256)
	assert.Equal(t, "loaded", p2.Status)
	assert.Equal(t, "", p2.SkipMessage)
	expectedP2Controls := []*inspecEvent.Control{
		{
			Id:     "/etc/services must exist",
			Title:  "Checking for /etc/services",
			Code:   "control '/etc/services must exist' do\n  title 'Checking for /etc/services'\n  desc 'Checking for /etc/services desc'\n  impact 0.3\n  describe file('/etc/services') do\n    it { should be_file }\n  end\nend\n",
			Desc:   "Checking for /etc/services desc",
			Impact: float32(0.3),
			Tags: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"our_criticality": {Kind: &structpb.Value_NumberValue{98}},
					"reviewed":        {Kind: &structpb.Value_BoolValue{true}},
					"cce":             {Kind: &structpb.Value_StringValue{"CCE-27072-8"}},
				},
			},
			Refs: []*structpb.Struct{
				{
					Fields: map[string]*structpb.Value{
						"url": {Kind: &structpb.Value_StringValue{"https://www.nsa.gov/ia/_files/os/redhat/rhel5-guide-i731.pdf"}},
						"ref": {Kind: &structpb.Value_StringValue{"NSA-RH6-STIG - Section 3.5.2.1"}},
					},
				},
				{
					Fields: map[string]*structpb.Value{
						"ref": {Kind: &structpb.Value_StringValue{"http://people.redhat.com/swells/scap-security-guide/RHEL/6/output/ssg-centos6-guide-C2S.html"}},
					},
				},
			},
			Results: []*inspecEvent.Result{
				{
					Status:    "passed",
					CodeDesc:  "File /etc/services should be file",
					RunTime:   float32(0.000076582),
					StartTime: "2017-10-18 09:18:40 +0000",
				},
			},
			SourceLocation: &inspecEvent.SourceLocation{
				Ref:  "controls/success.rb",
				Line: int32(2),
			},
		},
	}
	assert.Len(t, p2.Controls, 1)
	assert.Equal(t, expectedP2Controls, p2.Controls)
	expectedP2Support := []*inspecEvent.Support{}
	assert.Len(t, p2.Supports, 0)
	assert.Equal(t, expectedP2Support, p2.Supports)
	expectedP2Attributes := []*inspecEvent.Attribute{}
	assert.Equal(t, expectedP2Attributes, p2.Attributes)
	expectedP2Dependencies := make([]*inspecEvent.Dependency, 0)
	assert.Len(t, p2.Depends, 0)
	assert.Equal(t, expectedP2Dependencies, p2.Depends)
	expectedP2Groups := []*inspecEvent.Group{
		{
			Id:       "controls/success.rb",
			Title:    "",
			Controls: []string{"/etc/services must exist"},
		},
	}
	assert.Len(t, p2.Groups, 1)
	assert.Equal(t, expectedP2Groups, p2.Groups)
	assert.Equal(t, "", p2.ParentProfile)

	p3 := report.Profiles[2]
	assert.Equal(t, "mywindows", p3.Name)
	assert.Equal(t, "My Demo Windows Profile", p3.Title)
	assert.Equal(t, "skipped", p3.Status)
	assert.Equal(t, "Skipping profile: 'mywindows' on unsupported platform: 'mac_os_x/17.7.0'.", p3.SkipMessage)
}
