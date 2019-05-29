package backend_test

import (
	"encoding/json"
	"io/ioutil"
	"testing"

	subject "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/assert"
)

func ReadCCRJsonDefault() (subject.ChefClientRun, error) {
	return ReadCCRJson("../examples/chef_client_run.json")
}

func ReadCCRJson(filePath string) (subject.ChefClientRun, error) {
	exampleCCR, _ := ioutil.ReadFile(filePath)
	var data subject.ChefClientRun
	err := json.Unmarshal(exampleCCR, &data)
	return data, err
}

func TestJsonMarshalling(t *testing.T) {
	_, err := ReadCCRJsonDefault()
	assert.Equal(t, nil, err)
}

func recursiveFlatten(m map[string]interface{}) map[string]interface{} {
	o := make(map[string]interface{})

	for k, v := range m {
		switch child := v.(type) {
		case map[string]interface{}:
			nm := recursiveFlatten(child)
			for nk, nv := range nm {
				o[k+"."+nk] = nv
			}
		default:
			o[k] = v
		}
	}
	return o
}

func TestFlatten1(t *testing.T) {
	jsonMap := map[string]interface{}{"foo": "f", "boo": "bo"}
	m := subject.Flatten(jsonMap)
	expect := recursiveFlatten(jsonMap)
	assert.Equal(t, expect, m)
}

func TestFlatten2(t *testing.T) {
	jsonMap := map[string]interface{}{"foo": map[string]interface{}{"first": 1}, "boo": map[string]interface{}{"second": 2}}
	m := subject.Flatten(jsonMap)
	expect := recursiveFlatten(jsonMap)
	assert.Equal(t, expect, m)
}

func TestFlatten3(t *testing.T) {
	jsonMap := map[string]interface{}{
		"foo": map[string]interface{}{"first": map[string]interface{}{"ab": 1}},
		"boo": map[string]interface{}{"second": map[string]interface{}{"cd": map[string]interface{}{"fg": 2}}},
		"c":   3,
	}
	m := subject.Flatten(jsonMap)
	expect := recursiveFlatten(jsonMap)
	assert.Equal(t, expect, m)
}

func TestFailedCCRToNode(t *testing.T) {
	run, err := ReadCCRJson("../examples/converge-failure-report.json")
	assert.NoError(t, err)

	translatedNode, err := run.ToNode()
	assert.NoError(t, err)

	expectedMessage := "file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist."

	assert.Equal(t, expectedMessage, translatedNode.ErrorMessage)
	assert.Equal(t, "failure", translatedNode.Status)
	assert.Equal(t, subject.ChefError{}, translatedNode.Error)
}

func TestCCRToNode(t *testing.T) {
	run, _ := ReadCCRJsonDefault()
	translatedNode, err := run.ToNode()
	assert.Nil(t, err)
	assert.Equal(t, run.EntityUUID, translatedNode.EntityUuid)
	assert.Equal(t, run.NodePayload.PolicyName, translatedNode.PolicyName)
	assert.Equal(t, run.NodePayload.PolicyGroup, translatedNode.PolicyGroup)
	assert.Equal(t, run.NodePayload.ChefEnvironment, translatedNode.Environment)
	var recipesArray = []string{
		"opscode-ci::slave",
		"opscode-ci::_build_support",
		"chef-sugar::default",
		"opscode-ci::_platform_tweaks",
		"yum-epel::default",
		"opscode-ci::_migration",
		"omnibus::default",
		"omnibus::_common",
		"omnibus::_user",
		"omnibus::_omnibus_toolchain",
		"omnibus::_cacerts",
		"omnibus::_compile",
		"build-essential::default",
		"omnibus::_git",
		"omnibus::_ruby",
		"omnibus::_github",
		"omnibus::_libffi",
		"omnibus::_packaging",
		"omnibus::_selinux",
		"omnibus::_environment",
		"opscode-ci::_github",
		"opscode-ci::_ntp",
		"opscode-ci::_package_signing",
		"opscode-ci::_rubygems",
		"opscode-ci::_users",
		"opscode-ci::_sudo",
		"sudo::default",
		"opscode-ci::_java",
		"aws::default",
		"opscode-ci::_omnibus",
	}
	assert.Equal(t, recipesArray, translatedNode.Recipes)
	assert.Equal(t, recipesArray, run.RecipeNames())
	var tagsArray = []string{
		"slave",
		"supermarket",
		"builder",
	}
	assert.Equal(t, tagsArray, translatedNode.ChefTags)
	assert.Equal(t, tagsArray, run.TagNames())
	var rolesArray = []string{
		"test",
		"test1",
	}
	assert.Equal(t, rolesArray, translatedNode.Roles)
	assert.Equal(t, rolesArray, run.RoleNames())
	assert.Equal(t, translatedNode.RunList, run.RunList)
	assert.Equal(t, "12.6.0", translatedNode.ChefVersion)
	assert.Equal(t, int64(12342607), translatedNode.UptimeSeconds)
	platform, ok := run.NodePayload.Automatic["platform"].(string)
	assert.True(t, ok)
	platformVersion, ok := run.NodePayload.Automatic["platform_version"].(string)
	assert.True(t, ok)
	platformWithVersion := platform + " " + platformVersion
	assert.Equal(t, platformWithVersion, translatedNode.Platform)
	assert.Equal(t, run.NodePayload.Automatic["platform_family"], translatedNode.PlatformFamily)
	assert.Equal(t, run.NodePayload.Automatic["platform_version"], translatedNode.PlatformVersion)
	assert.Equal(t, run.NodePayload.Automatic["fqdn"], translatedNode.Fqdn)
	assert.Equal(t, run.NodePayload.Automatic["ipaddress"], translatedNode.Ipaddress)
	assert.Equal(t, "6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482", translatedNode.PolicyRevision)
	var resourceNamesArray = []string{
		"/tmp/test.txt",
		"test_resource_path",
		"ls",
		"/tmp/test.txt",
		"ls -l",
		"/tmp/test.txt",
		"/tmp/always-updated.txt",
		"/failed/file/resource",
		"/tmp/do-not-write.txt",
	}
	assert.Equal(t, resourceNamesArray, translatedNode.ResourceNames)
	assert.Equal(t, resourceNamesArray, run.ResourceNames())
	assert.Equal(t, run.ExpandedRunList, translatedNode.ExpandedRunList)
	assert.Equal(t, "i-9ab3dd6d", translatedNode.Ec2.InstanceId)
	assert.Equal(t, "i-9ab3dd6d", translatedNode.CloudID)
	assert.Equal(t, "m3.medium", translatedNode.Ec2.InstanceType)
	assert.Equal(t, "us-west-2a", translatedNode.Ec2.PlacementAvailabilityZone)
	// We need this to be nil if the attribute is not set
	assert.Nil(t, translatedNode.Ec2.PublicIpv4)
	assert.Equal(t, run.RunList, translatedNode.RunList)
	assert.Equal(t, run.Tags, translatedNode.Tags)
	assert.Contains(t, translatedNode.Cookbooks, "apt")
	assert.Contains(t, translatedNode.Cookbooks, "aws")
	assert.Equal(t, run.TotalResourceCount, translatedNode.NodeInfo.TotalResourceCount)
}

func TestCountNumberOfValuesMapSimple(t *testing.T) {
	type Map1 map[string]interface{}

	jsonMap := Map1{"foo": Map1{"first": 1}, "boo": Map1{"second": 2}}
	count := subject.CountNumberOfValuesMap(jsonMap)

	assert.Equal(t, 2, count)
}

func TestCountNumberOfValuesMapEmpty(t *testing.T) {
	type Map1 map[string]interface{}

	jsonMap := Map1{}
	count := subject.CountNumberOfValuesMap(jsonMap)

	assert.Equal(t, 0, count)
}

func TestCountNumberOfValuesMapLarge(t *testing.T) {
	jsonMap := map[string]interface{}{
		"9":   "1",
		"foo": map[string]interface{}{"1": "2", "11": "3"},
		"boo": map[string]interface{}{"2": "4"},
		"tim": []interface{}{
			map[string]interface{}{"3": "5"},
			map[string]interface{}{"6": "6"},
			map[string]interface{}{"7": map[string]interface{}{"8": "7", "10": "8"}}}}
	count := subject.CountNumberOfValuesMap(jsonMap)

	assert.Equal(t, 8, count)
}

func TestCCRToRun(t *testing.T) {
	run, _ := ReadCCRJsonDefault()
	translatedRun, err := run.ToNodeRun()
	assert.Nil(t, err)
	assert.Equal(t, run.EntityUUID, translatedRun.EntityUuid)
	assert.Equal(t, run.NodePayload.ChefEnvironment, translatedRun.Environment)
	var recipesArray = []string{
		"opscode-ci::slave",
		"opscode-ci::_build_support",
		"chef-sugar::default",
		"opscode-ci::_platform_tweaks",
		"yum-epel::default",
		"opscode-ci::_migration",
		"omnibus::default",
		"omnibus::_common",
		"omnibus::_user",
		"omnibus::_omnibus_toolchain",
		"omnibus::_cacerts",
		"omnibus::_compile",
		"build-essential::default",
		"omnibus::_git",
		"omnibus::_ruby",
		"omnibus::_github",
		"omnibus::_libffi",
		"omnibus::_packaging",
		"omnibus::_selinux",
		"omnibus::_environment",
		"opscode-ci::_github",
		"opscode-ci::_ntp",
		"opscode-ci::_package_signing",
		"opscode-ci::_rubygems",
		"opscode-ci::_users",
		"opscode-ci::_sudo",
		"sudo::default",
		"opscode-ci::_java",
		"aws::default",
		"opscode-ci::_omnibus",
	}
	assert.Equal(t, recipesArray, translatedRun.Recipes)
	assert.Equal(t, recipesArray, run.RecipeNames())
	var tagsArray = []string{
		"slave",
		"supermarket",
		"builder",
	}
	assert.Equal(t, tagsArray, translatedRun.ChefTags)
	assert.Equal(t, tagsArray, run.TagNames())
	var rolesArray = []string{
		"test",
		"test1",
	}
	assert.Equal(t, rolesArray, translatedRun.Roles)
	assert.Equal(t, rolesArray, run.RoleNames())
	assert.Equal(t, translatedRun.RunList, run.RunList)
	assert.Equal(t, "12.6.0", translatedRun.ChefVersion)
	assert.Equal(t, int64(12342607), translatedRun.UptimeSeconds)
	platform, ok := run.NodePayload.Automatic["platform"].(string)
	assert.True(t, ok)
	platformVersion, ok := run.NodePayload.Automatic["platform_version"].(string)
	assert.True(t, ok)
	platformWithVersion := platform + " " + platformVersion
	assert.Equal(t, platformWithVersion, translatedRun.Platform)
	assert.Equal(t, run.NodePayload.Automatic["platform_family"], translatedRun.PlatformFamily)
	assert.Equal(t, run.NodePayload.Automatic["platform_version"], translatedRun.PlatformVersion)
	assert.Equal(t, run.NodePayload.Automatic["fqdn"], translatedRun.Fqdn)
	assert.Equal(t, run.NodePayload.Automatic["ipaddress"], translatedRun.Ipaddress)
	assert.Equal(t, "6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482", translatedRun.PolicyRevision)
	var resourceNamesArray = []string{
		"/tmp/test.txt",
		"test_resource_path",
		"ls",
		"/tmp/test.txt",
		"ls -l",
		"/tmp/test.txt",
		"/tmp/always-updated.txt",
		"/failed/file/resource",
		"/tmp/do-not-write.txt",
	}
	assert.Equal(t, resourceNamesArray, translatedRun.ResourceNames)
	assert.Equal(t, resourceNamesArray, run.ResourceNames())
	assert.Equal(t, run.ExpandedRunList, translatedRun.ExpandedRunList)
	assert.Equal(t, run.RunList, translatedRun.RunList)
	assert.Equal(t, run.Tags, translatedRun.Tags)
	assert.Equal(t, subject.ChefError{}, translatedRun.Error)
	assert.Equal(t, run.TotalResourceCount, translatedRun.NodeInfo.TotalResourceCount)
	assert.Equal(t, run.UpdatedResourceCount, translatedRun.UpdatedResourceCount)
	assert.Equal(t, run.StartTime, translatedRun.StartTime)
	assert.Equal(t, run.EndTime, translatedRun.EndTime)
	assert.Equal(t, run.RunID, translatedRun.RunID)
	assert.Contains(t, translatedRun.Cookbooks, "apt")
	assert.Contains(t, translatedRun.Cookbooks, "aws")
	var vCookbooks = []subject.VersionedCookbook{
		{
			Name:    "apt",
			Version: "2.9.2",
		},
		{
			Name:    "aws",
			Version: "2.3.0",
		},
	}
	assert.Contains(t, translatedRun.VersionedCookbooks, vCookbooks[0])
	assert.Contains(t, translatedRun.VersionedCookbooks, vCookbooks[1])
}

func TestCCRPlatformWithVersionNormal(t *testing.T) {
	run, _ := ReadCCRJsonDefault()

	run.NodePayload.Automatic["platform"] = "ubuntu"
	run.NodePayload.Automatic["platform_version"] = "7.1"

	translatedRun, err := run.ToNodeRun()
	assert.Nil(t, err)

	assert.Equal(t, "ubuntu 7.1", translatedRun.Platform)
}

func TestCCRPlatformWithVersionNoVersion(t *testing.T) {
	run, _ := ReadCCRJsonDefault()

	run.NodePayload.Automatic["platform"] = "ubuntu"
	run.NodePayload.Automatic["platform_version"] = nil

	translatedRun, err := run.ToNodeRun()
	assert.Nil(t, err)

	assert.Equal(t, "ubuntu", translatedRun.Platform)
}

func TestCCRPlatformWithVersionNoPlatform(t *testing.T) {
	run, _ := ReadCCRJsonDefault()

	run.NodePayload.Automatic["platform"] = nil
	run.NodePayload.Automatic["platform_version"] = "7.1"

	translatedRun, err := run.ToNodeRun()
	assert.Nil(t, err)

	assert.Equal(t, "", translatedRun.Platform)
}

func TestCCRToNodeAttribute(t *testing.T) {
	run, _ := ReadCCRJsonDefault()
	attribute, err := run.ToNodeAttribute()
	assert.Nil(t, err)
	assert.Equal(t, run.EntityUUID, attribute.EntityUUID)
	assert.Equal(t, run.NodePayload.Name, attribute.Name)
	assert.Equal(t, run.NodePayload.ChefEnvironment, attribute.ChefEnvironment)
	assert.Equal(t, run.NodePayload.RunList, attribute.RunList)
	attributeNormal, err := json.Marshal(run.NodePayload.Normal)
	assert.Nil(t, err)
	assert.Equal(t, string(attributeNormal), attribute.Normal)

	attributeDefault, err := json.Marshal(run.NodePayload.Default)
	assert.Nil(t, err)
	assert.Equal(t, string(attributeDefault), attribute.Default)

	attributeOverride, err := json.Marshal(run.NodePayload.Override)
	assert.Nil(t, err)
	assert.Equal(t, string(attributeOverride), attribute.Override)
}

func TestCCRExpandedRunListWithLoopingRoles(t *testing.T) {
	run, _ := ReadCCRJson("../examples/looping_roles_ccr.json")
	node, err := run.ToNode()
	assert.Nil(t, err)
	nodeExpandedRunList := node.ExpandedRunList
	assert.NotNil(t, nodeExpandedRunList)
	assert.Equal(t, "_default", nodeExpandedRunList.ID)
	assert.Equal(t, 1, len(nodeExpandedRunList.RunList))
	runListItem := nodeExpandedRunList.RunList[0]
	assert.Equal(t, "role", runListItem.Type)
	assert.Equal(t, "web3", runListItem.Name)

	assert.Equal(t, 4, len(runListItem.Children))
	assert.Equal(t, "recipe", runListItem.Children[0].Type)
	assert.Equal(t, "chef-client::default", runListItem.Children[0].Name)
	assert.Equal(t, nil, runListItem.Children[0].Version)
	assert.Equal(t, false, runListItem.Children[0].Skipped)

	assert.Equal(t, "role", runListItem.Children[3].Type)
	assert.Equal(t, "none", runListItem.Children[3].Name)
	assert.Equal(t, nil, runListItem.Children[3].Version)
	assert.Equal(t, false, runListItem.Children[3].Skipped)
	assert.Equal(t, 2, len(runListItem.Children[3].Children))

	assert.Equal(t, "role", runListItem.Children[3].Children[1].Type)
	assert.Equal(t, "web3", runListItem.Children[3].Children[1].Name)
	assert.Equal(t, nil, runListItem.Children[3].Children[1].Version)
	assert.Equal(t, true, runListItem.Children[3].Children[1].Skipped)
	assert.Equal(t, 0, len(runListItem.Children[3].Children[1].Children))
}

func TestCCRExpandedRunListWithTwoRoles(t *testing.T) {
	run, _ := ReadCCRJson("../examples/two_roles_ccr.json")
	node, err := run.ToNode()
	assert.Nil(t, err)
	nodeExpandedRunList := node.ExpandedRunList
	assert.NotNil(t, nodeExpandedRunList)
	assert.Equal(t, "_default", nodeExpandedRunList.ID)
	assert.Equal(t, 2, len(nodeExpandedRunList.RunList))
	runListItem0 := nodeExpandedRunList.RunList[0]
	assert.Equal(t, "role", runListItem0.Type)
	assert.Equal(t, "none", runListItem0.Name)
	assert.Equal(t, 1, len(runListItem0.Children))

	assert.Equal(t, "recipe", runListItem0.Children[0].Type)
	assert.Equal(t, "test::default", runListItem0.Children[0].Name)
	assert.Equal(t, nil, runListItem0.Children[0].Version)
	assert.Equal(t, false, runListItem0.Children[0].Skipped)

	runListItem1 := nodeExpandedRunList.RunList[1]
	assert.Equal(t, "role", runListItem1.Type)
	assert.Equal(t, "web", runListItem1.Name)
	assert.Equal(t, 3, len(runListItem1.Children))

	assert.Equal(t, "recipe", runListItem1.Children[2].Type)
	assert.Equal(t, "lamp2::default", runListItem1.Children[2].Name)
	assert.Equal(t, nil, runListItem0.Children[0].Version)
	assert.Equal(t, false, runListItem0.Children[0].Skipped)
}
