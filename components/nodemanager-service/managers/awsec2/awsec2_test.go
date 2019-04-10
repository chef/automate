package awsec2

import (
	"testing"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/nodemanager-service/api/manager"

	"github.com/aws/aws-sdk-go/service/ec2"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFormatRegionFilters(t *testing.T) {
	region := "*us-west*"
	region2 := "*us-east-1*"

	filterList := formatRegionFilters([]*string{&region, &region2})
	filterName := "endpoint"
	filter := ec2.Filter{Name: &filterName, Values: []*string{&region, &region2}}
	assert.Equal(t, []*ec2.Filter{&filter}, filterList)
}

func TestHandleFieldFilterTags(t *testing.T) {
	nodes := map[string][]*manager.ManagerNode{
		"us-west": {
			{Tags: []*common.Kv{
				{Key: "Environment", Value: "Prod"},
				{Key: "Environment", Value: "Test"},
				{Key: "Test", Value: "VJ"},
			},
			},
		},
	}
	uniqueMap, err := handleFieldFilterTags(nodes, "tags")
	require.NoError(t, err)
	expected := make(map[string]interface{}, 0)
	expected["Environment"] = nil
	expected["Test"] = nil
	assert.Equal(t, expected, uniqueMap)

	uniqueMap, err = handleFieldFilterTags(nodes, "tags:Environment")
	require.NoError(t, err)
	expected = make(map[string]interface{}, 0)
	expected["Prod"] = nil
	expected["Test"] = nil
	assert.Equal(t, expected, uniqueMap)
}

func TestHandleRegionFilters(t *testing.T) {
	region := "us-west-1"
	region2 := "us-east*"

	formatRegion := "*us-west-1*"
	formatRegion2 := "*us-east*"

	search, excluded := handleRegionFilters([]*string{&region, &region2}, []*string{})
	assert.Equal(t, []*string{&formatRegion, &formatRegion2}, search)
	assert.Equal(t, []*string{}, excluded)

	region2excl := "us-east"
	search, excluded = handleRegionFilters([]*string{}, []*string{&region, &region2})
	assert.Equal(t, []*string{&region, &region2excl}, excluded)
	assert.Equal(t, []*string{}, search)
}

func TestHandleExcludedNodes(t *testing.T) {
	excludedNodes := []*manager.ManagerNode{
		{Id: "456", Region: "us-west"},
		{Id: "298", Region: "us-east"},
	}
	nodes := map[string][]*manager.ManagerNode{
		"us-west": {
			{Id: "123"},
			{Id: "789"},
			{Id: "456"},
		},
		"us-east": {
			{Id: "234"},
			{Id: "543"},
			{Id: "453"},
			{Id: "298"},
		},
	}
	newNodes := handleExcludedNodes(nodes, excludedNodes)
	assert.Equal(t, map[string][]*manager.ManagerNode{
		"us-west": {
			{Id: "123"},
			{Id: "789"},
		},
		"us-east": {
			{Id: "234"},
			{Id: "543"},
			{Id: "453"},
		},
	}, newNodes)
}
func TestRemoveMatchingNodeManager(t *testing.T) {
	arr := []*manager.ManagerNode{
		{Id: "123"},
		{Id: "789"},
		{Id: "456"},
		{Id: "298"},
	}
	newArr := removeMatchingNodeMngr(arr, &manager.ManagerNode{Id: "456"})
	assert.Equal(t, []*manager.ManagerNode{
		{Id: "123"},
		{Id: "789"},
		{Id: "298"},
	}, newArr)
}

func TestNodesFiltersToEc2Filters(t *testing.T) {
	key := "tag:Name"
	val := "vj"
	instanceState := "instance-state-name"
	running := "running"

	ec2FiltersExpected := []*ec2.Filter{
		{Name: &key, Values: []*string{&val}},
		{Name: &instanceState, Values: []*string{&running}},
	}
	excludedEc2FiltersExpected := []*ec2.Filter{
		{Name: &instanceState, Values: []*string{&running}},
	}
	region := "us-west"
	region2 := "us-east"
	regionsExpected := []*string{&region, &region2}
	excludedRegionsExpected := []*string{}

	filters := []*common.Filter{
		{Key: "region", Values: []string{"us-west", "us-east"}},
		{Key: "Name", Values: []string{"vj"}},
	}

	ec2Filters, excludedEc2Filters, regions, excludedRegions := nodesFiltersToEc2Filters(filters)
	assert.Equal(t, regionsExpected, regions)
	assert.Equal(t, excludedRegionsExpected, excludedRegions)
	assert.Equal(t, ec2FiltersExpected, ec2Filters)
	assert.Equal(t, excludedEc2FiltersExpected, excludedEc2Filters)
}

func TestParseInstanceInfo(t *testing.T) {
	var inst ec2.Instance
	env := "Environment"
	envVal := "Prod"
	inst.Tags = []*ec2.Tag{{Key: &env, Value: &envVal}}
	Id := "12345"
	inst.InstanceId = &Id
	dns := "some-val"
	inst.PublicDnsName = &dns
	node := parseInstanceInfo(&inst, "us-west-1", map[string]string{})
	assert.Equal(t, &manager.ManagerNode{
		Name: "some-val",
		Id:   Id,
		Host: dns,
		Tags: []*common.Kv{
			{Key: "Environment", Value: "Prod"},
		},
		Region: "us-west-1",
	}, node)
}

func TestKvArrayToMap(t *testing.T) {
	one := "123"
	two := "456"
	filter := common.Filter{Key: "Test", Values: []string{one, two}}
	mapFilters := kvArrayToMap(&filter)
	zaMap := make(map[string][]*string, 0)
	zaMap["Test"] = []*string{&one, &two}
	assert.Equal(t, zaMap, mapFilters)
}
