package pgdb_test

import (
	"context"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/components/nodemanager-service/pgdb/dbtest"
	"github.com/golang/protobuf/ptypes"

	"github.com/stretchr/testify/suite"
)

type NodeManagersAndNodesDBSuite dbtest.Suite

var ctx context.Context

func (suite *NodeManagersAndNodesDBSuite) SetupSuite() {
	suite.Database = dbtest.Setup()
}

func (suite *NodeManagersAndNodesDBSuite) SetupTest() {
	err := dbtest.TruncateTables(suite.Database)
	suite.Require().NoError(err)
}

func TestRunSuiteNM(t *testing.T) {
	if dbtest.Run() {
		suite.Run(t, new(NodeManagersAndNodesDBSuite))
	}
}

func (suite *NodeManagersAndNodesDBSuite) TestUpdateOrInsertInstanceSourceStateInDbChangesState() {
	mgr := manager.NodeManager{Name: "test", Type: "aws-ec2"}
	mgrID, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}
	// instance state: stopped
	stoppedInstanceState := pgdb.InstanceState{ID: "i-079356", State: "stopped", Region: "eu-west-1"}
	_, err = suite.Database.UpdateOrInsertInstanceSourceStateInDb(stoppedInstanceState, mgrID, "12345678", "aws-ec2")
	if err != nil {
		suite.FailNow(err.Error())
	}
	filter := &common.Filter{
		Key:    "state",
		Values: []string{"STOPPED"},
	}
	stoppedNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(0, len(stoppedNodes))

	// instance state: running
	runningInstanceState := pgdb.InstanceState{ID: "i-079356", State: "running", Region: "eu-west-1"}
	_, err = suite.Database.UpdateOrInsertInstanceSourceStateInDb(runningInstanceState, mgrID, "12345678", "aws-ec2")
	if err != nil {
		suite.FailNow(err.Error())
	}
	filter = &common.Filter{
		Key:    "state",
		Values: []string{"RUNNING"},
	}
	runningNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(runningNodes))

	// instance state: terminated
	terminatedInstanceState := pgdb.InstanceState{ID: "i-079356", State: "terminated", Region: "eu-west-1"}
	_, err = suite.Database.UpdateOrInsertInstanceSourceStateInDb(terminatedInstanceState, mgrID, "12345678", "aws-ec2")
	if err != nil {
		suite.FailNow(err.Error())
	}
	filter = &common.Filter{
		Key:    "state",
		Values: []string{"TERMINATED"},
	}
	terminatedNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(terminatedNodes))

	// now that instance state has been set to terminated, it cannot be changed
	_, err = suite.Database.UpdateOrInsertInstanceSourceStateInDb(runningInstanceState, mgrID, "12345678", "aws-ec2")
	if err != nil {
		suite.FailNow(err.Error())
	}
	terminatedNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(terminatedNodes))

	// test that node that was added is readable/in a good state
	_, err = suite.Database.GetNode(ctx, terminatedNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	_, err = suite.Database.DeleteNode(terminatedNodes[0].Id)

	// try to send in a new node with state of terminated, no nodes added
	terminatedInstanceState = pgdb.InstanceState{ID: "i-9346723", State: "terminated", Region: "eu-west-1"}
	_, err = suite.Database.UpdateOrInsertInstanceSourceStateInDb(terminatedInstanceState, mgrID, "12345678", "aws-ec2")
	if err != nil {
		suite.FailNow(err.Error())
	}
	filter = &common.Filter{
		Key:    "state",
		Values: []string{"TERMINATED"},
	}
	terminatedNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(0, len(terminatedNodes))
}

func (suite *NodeManagersAndNodesDBSuite) TestUpdateOrInsertInstanceSourceStateInDbIngestingNewNodeIncludesNameAndMakesMgrConnection() {
	mgr := manager.NodeManager{Name: "test", Type: "azure-vm"}
	mgrID, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}
	instanceState := pgdb.InstanceState{ID: "i-079356", Name: "my-new-node", State: "running", Region: "eu-west-1"}
	_, err = suite.Database.UpdateOrInsertInstanceSourceStateInDb(instanceState, mgrID, "12345678", "azure-vm")
	if err != nil {
		suite.FailNow(err.Error())
	}
	filter := &common.Filter{
		Key:    "manager_id",
		Values: []string{mgrID},
	}
	mgrNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Require().Equal(1, len(mgrNodes))

	node := mgrNodes[0]
	suite.Equal("my-new-node", node.Name)
	_, err = suite.Database.DeleteNode(node.Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
}
func (suite *NodeManagersAndNodesDBSuite) TestGetAwsEc2ManagersCollectsAllMgrs() {
	// TODO @afiune Mock the secrets-service
	secretId := "12345678901234567890123456789012"
	mgr := manager.NodeManager{Name: "tester", Type: "aws-ec2", CredentialId: secretId}
	_, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	// TODO @afiune Mock the secrets-service
	secretId2 := "NEW-KEY-ID1234567890123456789012"
	if err != nil {
		suite.FailNow(err.Error())
	}
	mgr2 := manager.NodeManager{Name: "tester-1", Type: "aws-ec2", CredentialId: secretId2}
	_, err = suite.Database.AddNodeManager(&mgr2, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	mgrIds, err := suite.Database.GetAllManagersByType("aws-ec2")

	if err != nil {
		suite.FailNow(err.Error())
	}
	// expect two
	suite.Equal(2, len(mgrIds))
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerSubscriptionsToDBAddsTheSubsWithCorrectInfo() {
	// TODO @afiune Mock the secrets-service
	secretId := "12345678901234567890123456789012"
	mgr := manager.NodeManager{Name: "tester", Type: "azure-api", CredentialId: secretId}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	// we add two subs, expect two nodes
	subs := []*manager.ManagerNode{{Id: "123252"}, {Id: "3432523"}}
	nodeIds := suite.Database.AddManagerSubscriptionsToDB(subs, mgrId, "1224313", secretId)
	suite.Equal(2, len(nodeIds))

	// expect node to have backend of azure and correct subscription id
	ids, err := suite.Database.GetNodeSecretIds(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(ids))
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerNodeToDBAddsANodeWithCorrectInfo() {
	// TODO @afiune Mock the secrets-service
	secretId := "12345678901234567890123456789012"
	mgr := manager.NodeManager{Name: "tester", Type: "azure-api", CredentialId: secretId}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	nodeIds, err := suite.Database.AddManagerNodeToDB(mgrId, "242403433", secretId, "account alias", "us-east-1")
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(nodeIds))

	// expect node to have backend of aws and correct region
	ids, err := suite.Database.GetNodeSecretIds(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(ids))
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerNodesToDBAddsTheInstancesWithCorrectInfo() {
	// TODO @afiune Mock the secrets-service
	secretId := "12345678901234567890123456789012"
	winrmSecretId := "12345678901234567890123456789012"

	credByTag := &manager.CredentialsByTags{
		TagKey:        "my tag key",
		TagValue:      "my tag val",
		CredentialIds: []string{winrmSecretId},
	}
	mgr := manager.NodeManager{
		Name:                "tester",
		Type:                "aws-ec2",
		CredentialId:        secretId,
		InstanceCredentials: []*manager.CredentialsByTags{credByTag},
	}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	// we add two instances, expect two nodes
	// one node will have tags that match credentials, one will not
	tag := common.Kv{Key: "my tag key", Value: "my tag val"}
	mgrNode := manager.ManagerNode{Id: "i-934332", Platform: "windows", Region: "us-west-2", Tags: []*common.Kv{&tag}}
	mgrNode2 := manager.ManagerNode{Id: "i-6536332", Region: "us-west-2", Tags: []*common.Kv{}}
	instances := []*manager.ManagerNode{&mgrNode, &mgrNode2}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrId, "242403433", []*manager.CredentialsByTags{credByTag}, "aws-ec2")
	suite.Equal(2, len(nodeIds))

	// expect node1 to have 1 cred associated
	ids, err := suite.Database.GetNodeSecretIds(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(ids))

	// expect node2 to have no creds associated
	ids, err = suite.Database.GetNodeSecretIds(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(0, len(ids))
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerNodesToDBDoesNotAddNodeWhenAlreadyPresent() {
	secretId := "12345678901234567890123456789012"

	mgr := manager.NodeManager{
		Name:                "tester",
		Type:                "aws-ec2",
		CredentialId:        secretId,
		InstanceCredentials: []*manager.CredentialsByTags{},
	}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "999999999999")
	if err != nil {
		suite.FailNow(err.Error())
	}

	fetchedNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 1000, []*common.Filter{})
	suite.Require().NoError(err)

	time.Sleep(5)

	// we add a "starter" node with cloud info through processnode call
	err = suite.Database.ProcessIncomingNode(&manager.NodeMetadata{
		Uuid:            "1234-7823402-2438942",
		Name:            "my-test",
		PlatformName:    "windows",
		SourceId:        "i-93433000",
		SourceRegion:    "us-west-2",
		SourceAccountId: "999999999999",
		LastContact:     ptypes.TimestampNow(),
		RunData: &nodes.LastContactData{
			Id:      "1003-9254-2004-1322",
			EndTime: ptypes.TimestampNow(),
			Status:  nodes.LastContactData_PASSED,
		},
	})
	suite.NoError(err)

	fetchedNodes2, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 1000, []*common.Filter{})
	suite.Require().NoError(err)

	suite.Equal((len(fetchedNodes) + 1), len(fetchedNodes2))

	// we send a node with same cloud info through - no new node should be created
	node := manager.ManagerNode{Id: "i-93433000", Platform: "windows", Region: "us-west-2"}
	nodeIds := suite.Database.AddManagerNodesToDB([]*manager.ManagerNode{&node}, mgrId, "999999999999", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	fetchedNodes3, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 1000, []*common.Filter{})
	suite.Require().NoError(err)

	suite.Equal(len(fetchedNodes2), len(fetchedNodes3))
}

func (suite *NodeManagersAndNodesDBSuite) TestDeleteNodeManagerResetsManagerFieldOfNodes() {
	mgr1 := manager.NodeManager{Name: "test", Type: "aws-ec2"}
	mgrId1, err := suite.Database.AddNodeManager(&mgr1, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}
	// TODO: Is there another easy way to add a second node manager besides changing the type?
	mgr2 := manager.NodeManager{Name: "mgr2", Type: "aws-api"}
	mgrId2, err := suite.Database.AddNodeManager(&mgr2, "22222222")
	if err != nil {
		suite.FailNow(err.Error())
	}

	node1 := manager.ManagerNode{Id: "i-1111111", Region: "us-west-2"}
	node2 := manager.ManagerNode{Id: "i-2222222", Region: "us-west-2"}

	instances := []*manager.ManagerNode{&node1}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrId1, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	instances = []*manager.ManagerNode{&node1, &node2}
	nodeIds = suite.Database.AddManagerNodesToDB(instances, mgrId2, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(2, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.ElementsMatch([]string{mgrId1, mgrId2}, n.ManagerIds)

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}

	suite.ElementsMatch([]string{mgrId2}, n.ManagerIds)

	err = suite.Database.DeleteNodeManager(mgrId2)
	if err != nil {
		suite.FailNow(err.Error())
	}

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("aws-ec2", n.Manager)

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("automate", n.Manager)
}

func (suite *NodeManagersAndNodesDBSuite) TestDeleteNodeManagerWithNodesDeletesNodes() {
	mgr1 := manager.NodeManager{Name: "mgr1", Type: "aws-ec2"}
	mgrId1, err := suite.Database.AddNodeManager(&mgr1, "11111111")
	if err != nil {
		suite.FailNow(err.Error())
	}
	// TODO: Is there another easy way to add a second node manager besides changing the type?
	mgr2 := manager.NodeManager{Name: "mgr2", Type: "aws-api"}
	mgrId2, err := suite.Database.AddNodeManager(&mgr2, "22222222")
	if err != nil {
		suite.FailNow(err.Error())
	}

	node1 := manager.ManagerNode{Id: "i-1111111", Region: "us-west-2"}
	node2 := manager.ManagerNode{Id: "i-2222222", Region: "us-west-2"}

	instances := []*manager.ManagerNode{&node1}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrId1, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	instances = []*manager.ManagerNode{&node1, &node2}
	nodeIds = suite.Database.AddManagerNodesToDB(instances, mgrId2, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(2, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.ElementsMatch([]string{mgrId1, mgrId2}, n.ManagerIds)

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.ElementsMatch([]string{mgrId2}, n.ManagerIds)

	deletedIds, err := suite.Database.DeleteNodeManagerWithNodes(mgrId2)
	if err != nil {
		suite.FailNow(err.Error())
	}

	suite.ElementsMatch([]string{nodeIds[1]}, deletedIds)

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.ElementsMatch([]string{mgrId1}, n.ManagerIds)

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	suite.Error(err)
}

func (suite *NodeManagersAndNodesDBSuite) TestDeleteNodeManagerWithNodeStateUpdateUpdatesNodes() {
	mgr1 := manager.NodeManager{Name: "mgr1", Type: "aws-ec2"}
	mgrId1, err := suite.Database.AddNodeManager(&mgr1, "11111111")
	if err != nil {
		suite.FailNow(err.Error())
	}
	// TODO: Is there another easy way to add a second node manager besides changing the type?
	mgr2 := manager.NodeManager{Name: "mgr2", Type: "aws-api"}
	mgrId2, err := suite.Database.AddNodeManager(&mgr2, "22222222")
	if err != nil {
		suite.FailNow(err.Error())
	}

	node1 := manager.ManagerNode{Id: "i-1111111", Region: "us-west-2"}
	node2 := manager.ManagerNode{Id: "i-2222222", Region: "us-west-2"}

	instances := []*manager.ManagerNode{&node1}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrId1, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	instances = []*manager.ManagerNode{&node1, &node2}
	nodeIds = suite.Database.AddManagerNodesToDB(instances, mgrId2, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(2, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.ElementsMatch([]string{mgrId1, mgrId2}, n.ManagerIds)

	stateBefore := n.State

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}

	suite.ElementsMatch([]string{mgrId2}, n.ManagerIds)

	err = suite.Database.DeleteNodeManagerWithNodeStateUpdate(mgrId2, "terminated")
	if err != nil {
		suite.FailNow(err.Error())
	}

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}

	suite.ElementsMatch([]string{mgrId1}, n.ManagerIds)
	suite.Equal(stateBefore, n.State)

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.ElementsMatch([]string{}, n.ManagerIds)
	suite.Equal("terminated", n.State)
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerNodesToDBResetsManagerFieldOfNodes() {
	mgr := manager.NodeManager{Name: "test", Type: "aws-ec2"}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	node := manager.ManagerNode{Id: "i-934332", Region: "us-west-2"}
	instances := []*manager.ManagerNode{&node}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrId, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("aws-ec2", n.Manager)

	err = suite.Database.DeleteNodeManager(mgrId)
	if err != nil {
		suite.FailNow(err.Error())
	}

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("automate", n.Manager)

	mgrId, err = suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	nodeIds = suite.Database.AddManagerNodesToDB(instances, mgrId, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("aws-ec2", n.Manager)
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerSubscriptionsToDBResetsManagerFieldOfNodes() {
	// TODO @afiune Mock the secrets-service
	secretId := "12345678901234567890123456789012"
	mgr := manager.NodeManager{Name: "tester", Type: "azure-api", CredentialId: secretId}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	subs := []*manager.ManagerNode{{Id: "123252"}}
	nodeIds := suite.Database.AddManagerSubscriptionsToDB(subs, mgrId, "1224313", secretId)
	suite.Equal(1, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("azure-api", n.Manager)

	err = suite.Database.DeleteNodeManager(mgrId)
	if err != nil {
		suite.FailNow(err.Error())
	}

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("automate", n.Manager)

	mgrId, err = suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	nodeIds = suite.Database.AddManagerSubscriptionsToDB(subs, mgrId, "1224313", secretId)

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("azure-api", n.Manager)
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerNodeToDBResetsManagerFieldOfNodes() {
	// TODO @afiune Mock the secrets-service
	secretId := "12345678901234567890123456789012"
	mgr := manager.NodeManager{Name: "tester", Type: "azure-api", CredentialId: secretId}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	nodeIds, err := suite.Database.AddManagerNodeToDB(mgrId, "242403433", secretId, "account alias", "us-west-1")
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("aws-api", n.Manager)

	err = suite.Database.DeleteNodeManager(mgrId)
	if err != nil {
		suite.FailNow(err.Error())
	}

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("automate", n.Manager)

	mgrId, err = suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	nodeIds, err = suite.Database.AddManagerNodeToDB(mgrId, "242403433", secretId, "account alias", "us-east-2")
	if err != nil {
		suite.FailNow(err.Error())
	}

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("aws-account-account alias", n.Name)
	suite.Equal("aws-api", n.Manager)
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerNodeToDBName() {
	secretId := "12345678901234567890123456789012"
	mgr := manager.NodeManager{Name: "tester", Type: "azure-api", CredentialId: secretId}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	nodeIds, err := suite.Database.AddManagerNodeToDB(mgrId, "242403433", secretId, "", "us-east-1")
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}

	suite.Equal("aws-account-242403433", n.Name)

	nodeIds, err = suite.Database.AddManagerNodeToDB(mgrId, "242403433", secretId, "a cool alias", "us-west-2")
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(nodeIds))

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}

	suite.Equal("aws-account-a cool alias", n.Name)
	suite.Equal("us-west-2", n.TargetConfig.Region)

}
func (suite *NodeManagersAndNodesDBSuite) TestRemoveStaleNodeAssociationsUpdatesOrphanNodes() {
	mgr := manager.NodeManager{Name: "tester", Type: "aws-ec2"}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	mgrNode := manager.ManagerNode{Id: "i-934332", Region: "us-west-2"}
	mgrNode2 := manager.ManagerNode{Id: "i-6536332", Region: "us-west-2"}
	instances := []*manager.ManagerNode{&mgrNode, &mgrNode2}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrId, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(2, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal([]string{mgrId}, n.ManagerIds)
	suite.Equal("aws-ec2", n.Manager)

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal([]string{mgrId}, n.ManagerIds)
	suite.Equal("aws-ec2", n.Manager)

	err = suite.Database.RemoveStaleNodeAssociations(mgrId, nodeIds[0:1])
	if err != nil {
		suite.FailNow(err.Error())
	}

	n, err = suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal([]string{mgrId}, n.ManagerIds)
	suite.Equal("aws-ec2", n.Manager)

	n, err = suite.Database.GetNode(ctx, nodeIds[1])
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal([]string{}, n.ManagerIds)
	suite.Equal("automate", n.Manager)
}

func (suite *NodeManagersAndNodesDBSuite) TestAddManagerNodesToDBDoesNotDuplicateTags() {
	mgr := manager.NodeManager{Name: "tester", Type: "aws-ec2"}
	mgrId, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	tags := []*common.Kv{{Key: "tacos", Value: "yes"}, {Key: "nachos", Value: "very yes"}}
	mgrNode := manager.ManagerNode{Id: "i-934332", Region: "us-west-2", Tags: tags}
	instances := []*manager.ManagerNode{&mgrNode}
	nodeIds := suite.Database.AddManagerNodesToDB(instances, mgrId, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))
	// Add the node again to simulate a manager being updated.
	nodeIds = suite.Database.AddManagerNodesToDB(instances, mgrId, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	suite.Equal(1, len(nodeIds))

	n, err := suite.Database.GetNode(ctx, nodeIds[0])
	if err != nil {
		suite.FailNow(err.Error())
	}
	expected := []*common.Kv{{Key: "tacos", Value: "yes"}, {Key: "nachos", Value: "very yes"}}
	suite.ElementsMatch(expected, n.Tags)
}
func (suite *NodeManagersAndNodesDBSuite) TestUpdateOrInsertInstanceSourceStateInDbIngestingNewNodeUsesIDIfNoName() {
	mgr := manager.NodeManager{Name: "test", Type: "azure-vm"}
	mgrID, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}
	instanceState := pgdb.InstanceState{ID: "i-079356", State: "running", Region: "eu-west-1"}
	_, err = suite.Database.UpdateOrInsertInstanceSourceStateInDb(instanceState, mgrID, "12345678", "azure-vm")
	if err != nil {
		suite.FailNow(err.Error())
	}
	filter := &common.Filter{
		Key:    "manager_id",
		Values: []string{mgrID},
	}
	mgrNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Require().Equal(1, len(mgrNodes))

	node := mgrNodes[0]
	suite.Equal("i-079356", node.Name)
	_, err = suite.Database.DeleteNode(node.Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
}
