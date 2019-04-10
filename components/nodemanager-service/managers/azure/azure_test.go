package azure

import (
	"testing"

	"github.com/Azure/azure-sdk-for-go/services/compute/mgmt/2017-12-01/compute"

	"github.com/Azure/azure-sdk-for-go/services/resources/mgmt/2017-05-10/resources"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/nodemanager-service/api/manager"

	"github.com/stretchr/testify/assert"
)

func TestRemoveExcludedSubsFromList(t *testing.T) {
	var subsList []*manager.ManagerNode
	excSubs := []string{"1234", "5678", "7523"}
	subs := []*manager.ManagerNode{
		{Id: "4334"},
		{Id: "5678"},
		{Id: "9242"},
		{Id: "4388"},
		{Id: "1234"},
		{Id: "8943"},
	}

	subsList = removeExcludedSubsFromList(subs, excSubs)
	assert.ElementsMatch(t, []*manager.ManagerNode{
		{Id: "4334"},
		{Id: "9242"},
		{Id: "4388"},
		{Id: "8943"},
	}, subsList)
}

func TestHandleSubscriptionFilters(t *testing.T) {
	filters := []*common.Filter{
		{Key: "subscription_id", Values: []string{"789"}},
	}
	subs, excludedSubs := handleSubscriptionFilters(filters)
	assert.Equal(t, []*manager.ManagerNode{
		{Id: "789"},
	}, subs)
	assert.Equal(t, 0, len(excludedSubs))

	filterExclude := []*common.Filter{
		{Key: "subscription_id", Values: []string{"123"}, Exclude: true},
	}
	subs, excludedSubs = handleSubscriptionFilters(filterExclude)
	assert.Equal(t, 0, len(subs))
	assert.Equal(t, []string{"123"}, excludedSubs)
}

func TestHandleVMTags(t *testing.T) {
	val1 := "vj"
	val2 := "Prod"
	val3 := "Luigi"
	tags := map[string]*string{"test": &val1, "Environment": &val2, "Mario": &val3}
	instanceTags := handleVMTags(tags)
	assert.ElementsMatch(t, []*common.Kv{
		{Key: "test", Value: "vj"},
		{Key: "Environment", Value: "Prod"},
		{Key: "Mario", Value: "Luigi"},
	}, instanceTags)
}

func TestBuildFilterString(t *testing.T) {
	filter := common.Filter{Key: "region", Values: []string{"uswest", "useast"}}
	filters := []*common.Filter{&filter}
	filterStrings, excTagsFilterStrings := buildFilterStrings(filters)
	assert.Equal(t, "location eq 'uswest'", filterStrings["region"][0])
	assert.Equal(t, "location eq 'useast'", filterStrings["region"][1])
	assert.Equal(t, 0, len(excTagsFilterStrings))

	filter = common.Filter{Key: "region", Values: []string{"uswest", "useast"}, Exclude: true}
	filters = []*common.Filter{&filter}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, "location ne 'uswest'", filterStrings["region"][0])
	assert.Equal(t, "location ne 'useast'", filterStrings["region"][1])
	assert.Equal(t, 0, len(excTagsFilterStrings))

	filter = common.Filter{Key: "name", Values: []string{"vj", "compliance"}}
	filters = []*common.Filter{&filter}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, "substringof(name, 'vj')", filterStrings["name"][0])
	assert.Equal(t, "substringof(name, 'compliance')", filterStrings["name"][1])
	assert.Equal(t, 0, len(excTagsFilterStrings))

	filter = common.Filter{Key: "name", Values: []string{"vj", "compliance"}, Exclude: true}
	filters = []*common.Filter{&filter}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, "name ne 'vj'", filterStrings["name"][0])
	assert.Equal(t, "name ne 'compliance'", filterStrings["name"][1])
	assert.Equal(t, 0, len(excTagsFilterStrings))

	filter = common.Filter{Key: "subscription_id", Values: []string{"1234"}}
	filters = []*common.Filter{&filter}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, 0, len(filterStrings))
	assert.Equal(t, 0, len(excTagsFilterStrings))

	filter = common.Filter{Key: "Environment", Values: []string{"Prod"}}
	filters = []*common.Filter{&filter}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, "tagname eq 'Environment' and tagvalue eq 'Prod'", filterStrings["Environment"][0])
	assert.Equal(t, 0, len(excTagsFilterStrings))

	filter = common.Filter{Key: "Environment", Values: []string{"Prod"}}
	filter2 := common.Filter{Key: "Test", Values: []string{"VJ"}}
	filters = []*common.Filter{&filter, &filter2}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, "tagname eq 'Environment' and tagvalue eq 'Prod'", filterStrings["Environment"][0])
	assert.Equal(t, "tagname eq 'Test' and tagvalue eq 'VJ'", filterStrings["Test"][0])
	assert.Equal(t, 0, len(excTagsFilterStrings))

	envFilterString := "tagname eq 'Environment' and tagvalue eq 'Prod'"
	filter = common.Filter{Key: "Environment", Values: []string{"Prod", "Test"}}
	filters = []*common.Filter{&filter}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, envFilterString, filterStrings["Environment"][0])
	assert.Equal(t, "tagname eq 'Environment' and tagvalue eq 'Test'", filterStrings["Environment"][1])
	assert.Equal(t, 0, len(excTagsFilterStrings))

	filter1 := common.Filter{Key: "Environment", Values: []string{"Prod", "Test"}, Exclude: true}
	filter2 = common.Filter{Key: "Some-Tag", Values: []string{"bananarama"}}
	filters = []*common.Filter{&filter1, &filter2}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, "tagname eq 'Environment' and tagvalue ne 'Prod'", excTagsFilterStrings[0])
	assert.Equal(t, "tagname eq 'Environment' and tagvalue ne 'Test'", excTagsFilterStrings[1])
	assert.Equal(t, "tagname eq 'Some-Tag' and tagvalue eq 'bananarama'", filterStrings["Some-Tag"][0])

	filter1 = common.Filter{Key: "Environment", Values: []string{"Prod", "Test"}, Exclude: true}
	filter2 = common.Filter{Key: "Some-Tag", Values: []string{"bananarama", "another one"}}
	filter3 := common.Filter{Key: "Different-Tag", Values: []string{"test-name", "other-name"}, Exclude: true}
	filters = []*common.Filter{&filter1, &filter2, &filter3}
	filterStrings, excTagsFilterStrings = buildFilterStrings(filters)
	assert.Equal(t, "tagname eq 'Environment' and tagvalue ne 'Prod'", excTagsFilterStrings[0])
	assert.Equal(t, "tagname eq 'Environment' and tagvalue ne 'Test'", excTagsFilterStrings[1])
	assert.Equal(t, "tagname eq 'Some-Tag' and tagvalue eq 'bananarama'", filterStrings["Some-Tag"][0])
	assert.Equal(t, "tagname eq 'Some-Tag' and tagvalue eq 'another one'", filterStrings["Some-Tag"][1])
	assert.Equal(t, "tagname eq 'Different-Tag' and tagvalue ne 'test-name'", excTagsFilterStrings[2])
	assert.Equal(t, "tagname eq 'Different-Tag' and tagvalue ne 'other-name'", excTagsFilterStrings[3])
}

func TestHandleResources(t *testing.T) {
	Id := "/subscription/123456/resources/resource-name"
	Id2 := "/subscription/123456/resources/resource-name-2"
	Id3 := "/subscription/123456/resources/resource-name-3"
	filter := common.Filter{Key: "FilterKey", Values: []string{"FilterVal"}}
	filters := []*common.Filter{&filter}
	res := make(map[string][]resources.GenericResource)
	res["region"] = []resources.GenericResource{
		{ID: &Id},
		{ID: &Id2},
		{ID: &Id3},
	}
	resourceGroupNames, err := handleResources(res, filters)
	if err != nil {
		assert.FailNow(t, err.Error())
	}
	assert.Equal(t, []string{"resource-name", "resource-name-2", "resource-name-3"}, resourceGroupNames)
}

func TestHandleExcludedResources(t *testing.T) {
	Id := "/subscription/123456/resources/resource-name"
	Id2 := "/subscription/123456/resources/resource-name-2"
	Id3 := "/subscription/123456/resources/resource-name-3"
	excludedResources := []resources.GenericResource{
		{ID: &Id},
		{ID: &Id2},
	}
	resources := []resources.GenericResource{
		{ID: &Id},
		{ID: &Id2},
		{ID: &Id3},
	}
	filteredResources := handleExcludedResources(resources, excludedResources)
	assert.Equal(t, 1, len(filteredResources))
	assert.Equal(t, "/subscription/123456/resources/resource-name-3", *filteredResources[0].ID)
}

func TestHandleStateResponse(t *testing.T) {
	code := "PowerState/something"
	display := "VM RUNNING"
	statuses := []compute.InstanceViewStatus{
		{Code: &code, DisplayStatus: &display},
	}
	state := handleStateResponse(&statuses)
	assert.Equal(t, "RUNNING", state)
}
