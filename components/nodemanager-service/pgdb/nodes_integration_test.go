package pgdb_test

import (
	"context"
	"testing"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/components/nodemanager-service/pgdb/dbtest"

	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/suite"
)

type NodesIntegrationSuite dbtest.Suite

func (suite *NodesIntegrationSuite) SetupSuite() {
	suite.Database = dbtest.Setup()
}

func (suite *NodesIntegrationSuite) SetupTest() {
	err := dbtest.TruncateTables(suite.Database)
	suite.Require().NoError(err)
}

func TestRunNodesIntegrationSuite(t *testing.T) {
	if dbtest.Run() {
		suite.Run(t, new(NodesIntegrationSuite))
	}
}

func (suite *NodesIntegrationSuite) TestGetNodesCanFilterByManagerIDAndReturnsAllManagerIDs() {
	mgr1 := manager.NodeManager{Name: "mgr1", Type: "aws-ec2"}
	mgrID1, err := suite.Database.AddNodeManager(&mgr1, "11111111")
	suite.Require().NoError(err)

	// TODO: Is there another easy way to add a second node manager besides changing the type?
	mgr2 := manager.NodeManager{Name: "mgr2", Type: "aws-api"}
	mgrID2, err := suite.Database.AddNodeManager(&mgr2, "22222222")
	suite.Require().NoError(err)

	// Host set here is Name after roundtripping to the DB with AddManagerNodesToDB/GetNodes
	node1 := manager.ManagerNode{Id: "i-1111111", Region: "us-west-2", Host: "Node1"}
	node2 := manager.ManagerNode{Id: "i-2222222", Region: "us-west-2", Host: "Node2"}

	instances := []*manager.ManagerNode{&node1, &node2}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrID1, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(2, len(nodeIds))

	instances = []*manager.ManagerNode{&node1}
	nodeIds = suite.Database.AddManagerNodesToDB(instances, mgrID2, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	filter := &common.Filter{
		Key:    "manager_id",
		Values: []string{mgrID1},
	}
	// Get the nodes in the DB that belong to the first manager, ordered by their name.
	newNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	suite.Require().NoError(err)

	suite.Equal(2, len(newNodes))
	suite.Equal(&pgdb.TotalCount{Total: 2, Unreachable: 0, Reachable: 0, Unknown: 2}, count)

	suite.Equal("Node1", newNodes[0].Name)
	suite.ElementsMatch([]string{mgrID1, mgrID2}, newNodes[0].ManagerIds)

	suite.Equal("Node2", newNodes[1].Name)
	suite.Equal([]string{mgrID1}, newNodes[1].ManagerIds)
}

func (suite *NodesIntegrationSuite) TestGetNodesCanFilterByEmptyManagerIDList() {
	mgr1 := manager.NodeManager{Name: "mgr1", Type: "aws-ec2"}
	mgrID1, err := suite.Database.AddNodeManager(&mgr1, "11111111")
	suite.Require().NoError(err)

	// Host set here is Name after roundtripping to the DB with AddManagerNodesToDB/GetNodes
	node1 := manager.ManagerNode{Id: "i-1111111", Region: "us-west-2", Host: "Node1"}
	instances := []*manager.ManagerNode{&node1}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrID1, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	node2Id, err := suite.Database.AddNode(&nodes.Node{Name: "Node2", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:    "manager_id",
		Values: []string{},
	}
	// Get the nodes in the DB that belong to the first manager, ordered by their name.
	newNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	suite.Require().NoError(err)

	suite.Equal(1, len(newNodes))
	suite.Equal(&pgdb.TotalCount{Total: 1, Unreachable: 0, Reachable: 0, Unknown: 2}, count)

	suite.Equal(node2Id, newNodes[0].Id)
	suite.Equal("Node2", newNodes[0].Name)
	suite.Equal([]string{}, newNodes[0].ManagerIds)
}

func (suite *NodesIntegrationSuite) TestGetNodesCanFilterByNameWithWildcard() {
	node1Id, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	node2Id, err := suite.Database.AddNode(&nodes.Node{Name: "Tostada Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Nacho Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:    "name",
		Values: []string{"Taco"},
	}
	filter2 := &common.Filter{
		Key:    "name",
		Values: []string{"Tostada"},
	}
	fetchedNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter, filter2})
	suite.Require().NoError(err)

	suite.Equal(2, len(fetchedNodes))
	suite.Equal(&pgdb.TotalCount{Total: 2, Unreachable: 0, Reachable: 0, Unknown: 3}, count)

	suite.Equal(node1Id, fetchedNodes[0].Id)
	suite.Equal("Taco Node", fetchedNodes[0].Name)
	suite.Equal(node2Id, fetchedNodes[1].Id)
	suite.Equal("Tostada Node", fetchedNodes[1].Name)
}

func (suite *NodesIntegrationSuite) TestGetNodesCanExcludeByNameWithWildcard() {
	_, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Tostada Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	node3Id, err := suite.Database.AddNode(&nodes.Node{Name: "Nacho Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:     "name",
		Values:  []string{"Taco"},
		Exclude: true,
	}
	filter2 := &common.Filter{
		Key:     "name",
		Values:  []string{"Tostada"},
		Exclude: true,
	}
	fetchedNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter, filter2})
	suite.Require().NoError(err)

	suite.Equal(1, len(fetchedNodes))
	suite.Equal(&pgdb.TotalCount{Total: 1, Unreachable: 0, Reachable: 0, Unknown: 3}, count)

	suite.Equal(node3Id, fetchedNodes[0].Id)
	suite.Equal("Nacho Node", fetchedNodes[0].Name)
}

func (suite *NodesIntegrationSuite) TestGetNodesReturnsErrorWithConflictingIncludeAndExcludeFilters() {
	filter := &common.Filter{
		Key:     "name",
		Values:  []string{"Taco"},
		Exclude: true,
	}
	filter2 := &common.Filter{
		Key:    "name",
		Values: []string{"Tostada"},
	}
	_, _, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter, filter2})

	message := "GetNodes error building where filter: buildWhereHavingFilter error: Filters are not allowed to be inclusive and exclusive on the same field."
	suite.EqualError(err, message)
}

func (suite *NodesIntegrationSuite) TestGetNodesCanFilterByTags() {
	node1Id, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Manager: "automate", Tags: []*common.Kv{{Key: "tacos", Value: "yes"}}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Nacho Node", Manager: "automate", Tags: []*common.Kv{{Key: "nachos", Value: "yes"}}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:    "tacos",
		Values: []string{"yes"},
	}
	fetchedNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	suite.Require().NoError(err)

	suite.Equal(1, len(fetchedNodes))
	suite.Equal(&pgdb.TotalCount{Total: 1, Unreachable: 0, Reachable: 0, Unknown: 2}, count)

	suite.Equal(node1Id, fetchedNodes[0].Id)
	suite.Equal("Taco Node", fetchedNodes[0].Name)
}

func (suite *NodesIntegrationSuite) TestGetNodesCanFilterByMultipleTags() {
	_, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Manager: "automate",
		Tags: []*common.Kv{{Key: "tacos", Value: "yes"}}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Nacho Node", Manager: "automate",
		Tags: []*common.Kv{{Key: "nachos", Value: "yes"}}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "No Nacho Node", Manager: "automate",
		Tags: []*common.Kv{{Key: "nachos", Value: "no"}, {Key: "tacos", Value: "yes"}}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	// Testing the OR of filters with the same key
	filter1 := &common.Filter{
		Key:    "nachos",
		Values: []string{"no", "yes"},
	}
	fetchedNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter1})
	suite.Require().NoError(err)
	suite.Require().Equal(2, len(fetchedNodes))
	suite.Equal(&pgdb.TotalCount{Total: 2, Unreachable: 0, Reachable: 0, Unknown: 3}, count)
	suite.Equal("Nacho Node", fetchedNodes[0].GetName())
	suite.Equal("No Nacho Node", fetchedNodes[1].GetName())

	// Testing the AND of filters with different keys
	filter1 = &common.Filter{
		Key:    "nachos",
		Values: []string{"no"},
	}
	filter2 := &common.Filter{
		Key:    "tacos",
		Values: []string{"yes"},
	}
	fetchedNodes, count, err = suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter1, filter2})
	suite.Require().NoError(err)
	suite.Require().Equal(1, len(fetchedNodes))
	suite.Equal(&pgdb.TotalCount{Total: 1, Unreachable: 0, Reachable: 0, Unknown: 3}, count)
	suite.Equal("No Nacho Node", fetchedNodes[0].GetName())
}

func (suite *NodesIntegrationSuite) TestGetNodesCanFilterByProjects() {
	node1Id, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Projects: []string{"Favorite Food", "Taco Bell Menu"}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Spaghetti Node", Projects: []string{"Best Pastas", "Favorite Food"}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:    "project",
		Values: []string{"Taco Bell Menu"},
	}
	fetchedNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	suite.Require().NoError(err)
	suite.Require().Equal(1, len(fetchedNodes))
	suite.Equal(&pgdb.TotalCount{Total: 1, Unreachable: 0, Reachable: 0, Unknown: 2}, count)

	suite.Equal(node1Id, fetchedNodes[0].Id)
	suite.Equal("Taco Node", fetchedNodes[0].Name)
}

func (suite *NodesIntegrationSuite) TestGetNodesCanFilterByNoProjects() {
	node1Id, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node"})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Spaghetti Node", Projects: []string{"Best Pastas", "Favorite Food"}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:    "project",
		Values: []string{},
	}
	fetchedNodes, count, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	suite.Require().NoError(err)
	suite.Require().Equal(1, len(fetchedNodes))
	suite.Equal(&pgdb.TotalCount{Total: 1, Unreachable: 0, Reachable: 0, Unknown: 2}, count)

	suite.Equal(node1Id, fetchedNodes[0].Id)
	suite.Equal("Taco Node", fetchedNodes[0].Name)
}

func (suite *NodesIntegrationSuite) TestDeleteNodesWithQuery() {
	_, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Tostada Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	node3Id, err := suite.Database.AddNode(&nodes.Node{Name: "Nacho Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:    "name",
		Values: []string{"Taco"},
	}
	filter2 := &common.Filter{
		Key:    "name",
		Values: []string{"Tostada*"},
	}
	names, err := suite.Database.DeleteNodesWithQuery([]*common.Filter{filter, filter2})
	suite.Require().NoError(err)

	suite.ElementsMatch([]string{"Taco Node", "Tostada Node"}, names)

	fetchedNodes, _, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{})

	suite.Require().Equal(1, len(fetchedNodes))

	suite.Equal(node3Id, fetchedNodes[0].Id)
	suite.Equal("Nacho Node", fetchedNodes[0].Name)
}

func (suite *NodesIntegrationSuite) TestAddNodeEscapesTags() {
	tags := []*common.Kv{
		{Key: `'; drop table users;`, Value: `'; drop table orders;`},
	}
	_, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Manager: "automate", Tags: tags, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	fetchedNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	suite.Require().NoError(err)

	suite.Require().Equal(1, len(fetchedNodes))
	suite.Require().Equal(1, len(fetchedNodes[0].Tags))

	tag := fetchedNodes[0].Tags[0]
	suite.Equal(`'; drop table users;`, tag.Key)
	suite.Equal(`'; drop table orders;`, tag.Value)
}

func (suite *NodesIntegrationSuite) TestGetNodesReturnsCorrectCountsWhenRequestingOnlyManagedNodes() {
	_, err := suite.Database.AddNode(&nodes.Node{Name: "Manually Managed Node", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Cloud Managed Node", Manager: "aws-ec2", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	_, err = suite.Database.AddNode(&nodes.Node{Name: "Ingested Node", Manager: "", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	suite.Require().NoError(err)

	filter := &common.Filter{
		Key:     "manager_type",
		Values:  []string{""},
		Exclude: true,
	}
	nodes, total, err := suite.Database.GetNodes("name", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	suite.Require().NoError(err)
	suite.Equal(&pgdb.TotalCount{Total: 2, Unreachable: 0, Reachable: 0, Unknown: 2}, total)
	suite.Equal("Cloud Managed Node", nodes[0].Name)
	suite.Equal("Manually Managed Node", nodes[1].Name)
}

func (suite *NodesIntegrationSuite) TestProjectsAreRoundtrippedThroughNodeLifecycle() {
	ctx := context.Background()

	// Create the node we're going to test with.
	testNodeID, err := suite.Database.AddNode(&nodes.Node{Name: "Taco Node", Projects: []string{"Favorite Food", "Taco Bell Menu"}})
	suite.Require().NotEmpty(testNodeID)
	suite.Require().NoError(err)

	testNode, err := suite.Database.GetNode(ctx, testNodeID)
	suite.Require().NoError(err)
	suite.Equal([]string{"Favorite Food", "Taco Bell Menu"}, testNode.Projects)

	// Create another node as the "control" that shouldn't change.
	controlNodeID, err := suite.Database.AddNode(&nodes.Node{Name: "Pozole Node", Projects: []string{"Mexican Restaurant Menu", "Best Soups"}})
	suite.Require().NotEmpty(controlNodeID)
	suite.Require().NoError(err)

	controlNode, err := suite.Database.GetNode(ctx, controlNodeID)
	suite.Require().NoError(err)
	suite.Equal([]string{"Best Soups", "Mexican Restaurant Menu"}, controlNode.Projects)

	// Update the projects on the original node.
	err = suite.Database.UpdateNode(&nodes.Node{Id: testNodeID, Name: "Updated Taco Node", Projects: []string{"Favorite Food", "Mexican Restaurant Menu"}})
	suite.Require().NoError(err)

	// The projects on the original node should be updated.
	testNode, err = suite.Database.GetNode(ctx, testNodeID)
	suite.Require().NoError(err)
	suite.Equal([]string{"Favorite Food", "Mexican Restaurant Menu"}, testNode.Projects)

	// The projects on the control node should be the same
	controlNode, err = suite.Database.GetNode(ctx, controlNodeID)
	suite.Require().NoError(err)
	suite.Equal([]string{"Best Soups", "Mexican Restaurant Menu"}, controlNode.Projects)
}

func (suite *NodesIntegrationSuite) TestBulkAddNodesAndTagsUpdate() {
	ctx := context.Background()

	node1 := &nodes.Node{Name: "Bulky One",
		Tags: []*common.Kv{{Key: "bleep", Value: "bloop"}}}

	testNodeIDs, err := suite.Database.BulkAddNodes([]*nodes.Node{node1})
	suite.Equal(0, len(testNodeIDs))
	suite.EqualError(err, "BulkAddNodes unable find TargetConfig hosts")

	// Setting up node1 with Host
	node1.TargetConfig = &nodes.TargetConfig{Host: "127.0.0.1"}
	testNodeIDs, err = suite.Database.BulkAddNodes([]*nodes.Node{node1})
	suite.Equal(0, len(testNodeIDs))
	suite.EqualError(err, "BulkAddNodes does not support TargetConfig.Host, use Hosts instead")

	// Setting up node1 with Hosts
	node1.TargetConfig = &nodes.TargetConfig{Hosts: []string{"127.0.0.1"}}

	// Setting up node2 with Hosts
	node2 := &nodes.Node{Name: "Bulky Two",
		TargetConfig: &nodes.TargetConfig{Hosts: []string{"127.0.0.2"}},
		Tags:         []*common.Kv{{Key: "bada", Value: "bing"}, {Key: "bleep", Value: "bloop"}}}

	// Add the nodes we're going to test with
	testNodeIDs, err = suite.Database.BulkAddNodes([]*nodes.Node{node1, node2})
	suite.Require().NoError(err)
	suite.Require().Equal(2, len(testNodeIDs))
	suite.Require().NotEmpty(testNodeIDs[0])
	suite.Require().NoError(err)

	testNode, err := suite.Database.GetNode(ctx, testNodeIDs[0])
	suite.Require().NoError(err)
	suite.Equal([]*common.Kv{{Key: "bleep", Value: "bloop"}}, testNode.Tags)

	testNode, err = suite.Database.GetNode(ctx, testNodeIDs[1])
	suite.Require().NoError(err)
	suite.Equal([]*common.Kv{{Key: "bada", Value: "bing"}, {Key: "bleep", Value: "bloop"}}, testNode.Tags)

	// Update the tags on node one
	err = suite.Database.UpdateNode(&nodes.Node{Id: testNodeIDs[0], Name: "Bulky One - Updated",
		Tags: []*common.Kv{{Key: "bleep", Value: "new"}}})
	suite.Require().NoError(err)

	// Name and Tags on one should be updated.
	testNode, err = suite.Database.GetNode(ctx, testNodeIDs[0])
	suite.Require().NoError(err)
	suite.Equal("Bulky One - Updated", testNode.Name)
	suite.Equal([]*common.Kv{{Key: "bleep", Value: "new"}}, testNode.Tags)

	// Name and Tags on second node should be the same.
	testNode, err = suite.Database.GetNode(ctx, testNodeIDs[1])
	suite.Require().NoError(err)
	suite.Equal("Bulky Two", testNode.Name)
	suite.Equal([]*common.Kv{{Key: "bada", Value: "bing"}, {Key: "bleep", Value: "bloop"}}, testNode.Tags)
}

var nowTime = ptypes.TimestampNow()
var baseNodeData = manager.NodeMetadata{
	Uuid:            "1223-4254-2424-1322",
	Name:            "my really cool client run node",
	PlatformName:    "debian",
	PlatformRelease: "8.6",
	LastContact:     nowTime,
	SourceId:        "",
	SourceRegion:    "",
	SourceAccountId: "",
	JobUuid:         "12343-232324-1231242",
}

func (suite *NodesIntegrationSuite) TestFilterByLastCheckInRange() {
	node := baseNodeData
	node.RunData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_PASSED,
	}
	err := suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	timeRangeMax := time.Now().Add(time.Hour * 24).Format(time.RFC3339)
	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_run_timerange", Values: []string{"2019-03-05T00:00:00Z", timeRangeMax}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
	suite.Equal(nowTime, listNodes[0].GetRunData().GetEndTime())
}

func (suite *NodesIntegrationSuite) TestFilterByLastScanTimeRange() {
	node := baseNodeData
	node.ScanData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_PASSED,
	}
	err := suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	timeRangeMax := time.Now().Add(time.Hour * 24).Format(time.RFC3339)
	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_scan_timerange", Values: []string{"2019-03-05T00:00:00Z", timeRangeMax}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
	suite.Equal(nowTime, listNodes[0].GetScanData().GetEndTime())
}

func (suite *NodesIntegrationSuite) TestFilterByRunDataStatus() {
	node := baseNodeData
	node.RunData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_FAILED,
	}
	err := suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_run_status", Values: []string{"FAILED"}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
	suite.Equal(nodes.LastContactData_FAILED, listNodes[0].GetRunData().GetStatus())
}

func (suite *NodesIntegrationSuite) TestFilterByScanDataStatus() {
	node := baseNodeData
	node.ScanData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_FAILED,
	}
	err := suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_scan_status", Values: []string{"FAILED"}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
	suite.Equal(nodes.LastContactData_FAILED, listNodes[0].GetScanData().GetStatus())
}

func (suite *NodesIntegrationSuite) TestFilterByRunDataPenultStatus() {
	node := baseNodeData
	node.RunData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_FAILED,
	}
	err := suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	node.RunData.Status = nodes.LastContactData_PASSED
	err = suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_run_penultimate_status", Values: []string{"FAILED"}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
	suite.Equal(nodes.LastContactData_FAILED, listNodes[0].GetRunData().GetPenultimateStatus())
	suite.Equal(nodes.LastContactData_PASSED, listNodes[0].GetRunData().GetStatus())

	listNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_run_penultimate_status", Values: []string{"FAILED"}},
		{Key: "last_run_status", Values: []string{"PASSED"}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))

}

func (suite *NodesIntegrationSuite) TestFilterByScanDataPenultStatus() {
	node := baseNodeData
	node.ScanData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_FAILED,
	}
	err := suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	node.ScanData.Status = nodes.LastContactData_SKIPPED
	err = suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_scan_penultimate_status", Values: []string{"FAILED"}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
	suite.Equal(nodes.LastContactData_FAILED, listNodes[0].GetScanData().GetPenultimateStatus())
	suite.Equal(nodes.LastContactData_SKIPPED, listNodes[0].GetScanData().GetStatus())

	listNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_scan_penultimate_status", Values: []string{"FAILED"}},
		{Key: "last_scan_status", Values: []string{"SKIPPED"}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
}

func (suite *NodesIntegrationSuite) TestFilterByScanDataAndRunDataStatus() {
	node := baseNodeData
	node.ScanData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_FAILED,
	}
	err := suite.Database.ProcessIncomingNode(&node)
	suite.Require().NoError(err)

	node2 := baseNodeData
	node2.RunData = &nodes.LastContactData{
		Id:      "0803-97654-2098-1322",
		EndTime: nowTime,
		Status:  nodes.LastContactData_PASSED,
	}
	err = suite.Database.ProcessIncomingNode(&node2)
	suite.Require().NoError(err)

	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{
		{Key: "last_scan_status", Values: []string{"FAILED"}},
		{Key: "last_run_status", Values: []string{"PASSED"}},
	})
	suite.Require().NoError(err)

	suite.Equal(1, len(listNodes))
	suite.Equal(nodes.LastContactData_FAILED, listNodes[0].GetScanData().GetStatus())
	suite.Equal(nodes.LastContactData_PASSED, listNodes[0].GetRunData().GetStatus())
}
