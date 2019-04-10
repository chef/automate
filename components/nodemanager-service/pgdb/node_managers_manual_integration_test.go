package pgdb_test

import (
	"time"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
)

func (suite *NodeManagersAndNodesDBSuite) TestGetManualNodesDueGetsTheRightNodes() {
	// add three nodes
	nId, err := suite.Database.AddNode(&nodes.Node{Name: "node one", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	if err != nil {
		suite.FailNow(err.Error())
	}
	nId2, err := suite.Database.AddNode(&nodes.Node{Name: "node two", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	if err != nil {
		suite.FailNow(err.Error())
	}
	nId3, err := suite.Database.AddNode(&nodes.Node{Name: "node three", Manager: "automate", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	if err != nil {
		suite.FailNow(err.Error())
	}
	nId4, err := suite.Database.AddNode(&nodes.Node{Name: "node three", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	if err != nil {
		suite.FailNow(err.Error())
	}

	// should get three nodes due
	nodeIds, err := suite.Database.GetManualNodesDue(time.Now().Add(time.Minute * -10))
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(3, len(nodeIds))

	// change the node state to terminated for one node
	err = suite.Database.ChangeNodeState(&manager.NodeState{
		Id:    nodeIds[0],
		State: manager.NodeState_TERMINATED,
	})
	if err != nil {
		suite.FailNow(err.Error())
	}

	// should get two nodes due
	nodeIds, err = suite.Database.GetManualNodesDue(time.Now().Add(time.Minute * -10))
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(2, len(nodeIds))

	_, err = suite.Database.DeleteNode(nId)
	_, err = suite.Database.DeleteNode(nId2)
	_, err = suite.Database.DeleteNode(nId3)
	_, err = suite.Database.DeleteNode(nId4)
}

func (suite *NodeManagersAndNodesDBSuite) TestChangeNodeStateChangesTheNodeState() {
	nId, err := suite.Database.AddNode(&nodes.Node{Name: "node one", Tags: []*common.Kv{}, TargetConfig: &nodes.TargetConfig{}})
	if err != nil {
		suite.FailNow(err.Error())
	}

	// should have no nodes with state running, stopped, or terminated
	filter := &common.Filter{
		Key:    "state",
		Values: []string{"TERMINATED", "RUNNING", "STOPPED"},
	}
	stateNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(0, len(stateNodes))

	// change the node state to stopped
	err = suite.Database.ChangeNodeState(&manager.NodeState{
		Id:    nId,
		State: manager.NodeState_STOPPED,
	})
	if err != nil {
		suite.FailNow(err.Error())
	}

	// should have one nodes with state stopped
	filter = &common.Filter{
		Key:    "state",
		Values: []string{"STOPPED"},
	}
	stoppedNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(stoppedNodes))

	// change the node state to terminated
	err = suite.Database.ChangeNodeState(&manager.NodeState{
		Id:    nId,
		State: manager.NodeState_TERMINATED,
	})
	if err != nil {
		suite.FailNow(err.Error())
	}

	// should have one node with state terminated
	filter = &common.Filter{
		Key:    "state",
		Values: []string{"TERMINATED"},
	}
	termNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(termNodes))

	// try to change the node state to running; can't b/c it was terminated
	err = suite.Database.ChangeNodeState(&manager.NodeState{
		Id:    nId,
		State: manager.NodeState_RUNNING,
	})
	if err != nil {
		suite.FailNow(err.Error())
	}
	termNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(termNodes))

	_, err = suite.Database.DeleteNode(nId)
}
