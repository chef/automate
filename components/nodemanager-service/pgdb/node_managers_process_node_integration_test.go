package pgdb_test

import (
	"time"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/golang/protobuf/ptypes"
)

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeWithNoUUID() {
	// test that a new node with no uuid, yes source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "",
		Name:            "123.798.324.32",
		PlatformName:    "ubuntu",
		PlatformRelease: "16.04",
		JobUuid:         "12345-389244-2433",
		LastContact:     nowTime,
		SourceId:        "i-078973",
		SourceRegion:    "eu-west-1",
		SourceAccountId: "8799247840",
	}
	err := suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err := suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("123.798.324.32", readNode.Name)
	suite.Equal("ubuntu", readNode.Platform)
	suite.InDelta(nowTime.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	// now send the same node info through again, expect the node to be updated,
	// and have the updated last_contact time
	nowTime3 := ptypes.TimestampNow()
	node.LastContact = nowTime3
	err = suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err = suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("123.798.324.32", readNode.Name)
	suite.Equal("ubuntu", readNode.Platform)
	// the code for read node rounds the last contact:
	// https://github.com/chef/automate/blob/master/components/compliance-service/dao/pgdb/nodes.go#L253
	// so here we test with the same logic
	suite.InDelta(nowTime3.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	filter := &common.Filter{
		Key:    "state",
		Values: []string{"RUNNING"},
	}
	runningNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(runningNodes))
	_, err = suite.Database.DeleteNode(listNodes[0].Id)
}

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeThatExistsAndHasSourceID() {
	// test that a new node with no uuid, yes source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4254-2424-1322",
		Name:            "123.798.324.32",
		PlatformName:    "ubuntu",
		PlatformRelease: "16.04",
		JobUuid:         "12345-389244-2433",
		LastContact:     nowTime,
	}
	err := suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err := suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("123.798.324.32", readNode.Name)
	suite.Equal("ubuntu", readNode.Platform)
	suite.InDelta(nowTime.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	// now send the same node info through again, with a source id
	node.SourceId = "i-078973"
	err = suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err = suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("1223-4254-2424-1322", readNode.Id)
	// suite.Equal("i-078973", readNode.GetTargetConfig().SourceId)
	suite.Equal("ubuntu", readNode.Platform)

	filter := &common.Filter{
		Key:    "state",
		Values: []string{"RUNNING"},
	}
	runningNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(runningNodes))
	_, err = suite.Database.DeleteNode(listNodes[0].Id)
}

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeWithUUID() {
	// test that a new node with uuid, no source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4254-2424-1322",
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		JobUuid:         "12345-389244-2433",
		LastContact:     nowTime,
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
	}
	err := suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err := suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("my really cool node", readNode.Name)
	suite.Equal("debian", readNode.Platform)
	suite.InDelta(nowTime.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	// now send the same node info through again, expect the node to be updated,
	// and have the updated last_contact time
	nowTime2 := ptypes.TimestampNow()
	node.LastContact = nowTime2
	err = suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err = suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("my really cool node", readNode.Name)
	suite.Equal("debian", readNode.Platform)
	suite.InDelta(nowTime2.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	filter := &common.Filter{
		Key:    "state",
		Values: []string{"RUNNING"},
	}
	runningNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(runningNodes))
	_, err = suite.Database.DeleteNode(listNodes[0].Id)
}

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeOldReportTime() {
	// sending in a node with a last contact of more than 10 min ago
	// should result in a node with a state of empty string (no nodes with state of running expected)
	tenMinAgo := time.Now().UTC().Add(time.Minute * -20)
	timestamp, err := ptypes.TimestampProto(tenMinAgo)
	if err != nil {
		suite.FailNow(err.Error())
	}
	node := &manager.NodeMetadata{
		Uuid:            "1223-4254-2424-1322",
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		JobUuid:         "12345-389244-2433",
		LastContact:     timestamp,
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
	}
	err = suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err := suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("my really cool node", readNode.Name)
	suite.Equal("debian", readNode.Platform)
	suite.InDelta(timestamp.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	filter := &common.Filter{
		Key:    "state",
		Values: []string{"RUNNING"},
	}
	runningNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(0, len(runningNodes))
	_, err = suite.Database.DeleteNode(listNodes[0].Id)
}

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeTerminatedNode() {
	// send in a node to get started
	node := &nodes.Node{
		Name:            "my really cool node",
		Platform:        "debian",
		PlatformVersion: "8.6",
		TargetConfig:    &nodes.TargetConfig{},
	}
	id, err := suite.Database.AddNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}

	// change the node state to terminated
	err = suite.Database.ChangeNodeState(&manager.NodeState{
		Id:    id,
		State: manager.NodeState_TERMINATED,
	})
	if err != nil {
		suite.FailNow(err.Error())
	}

	// should have one node with state terminated
	filter := &common.Filter{
		Key:    "state",
		Values: []string{"TERMINATED"},
	}
	terminatedNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(terminatedNodes))

	// send in node with same uuid as terminated one
	mgrNode := &manager.NodeMetadata{
		Uuid:            id,
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		JobUuid:         "12345-389244-2433",
		LastContact:     ptypes.TimestampNow(),
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
	}

	// this call won't update anything about the node because the node has a state of terminated
	// TODO (@vj): should a call like this return an error? or just not update anything, as it's doing now?
	// think about it and leave a comment here explaning decision and make change some day in near future
	err = suite.Database.ProcessIncomingNode(mgrNode)
	terminatedNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{filter})
	if err != nil {
		suite.FailNow(err.Error())
	}
	// we should still have one terminated node
	suite.Equal(1, len(terminatedNodes))
	_, err = suite.Database.DeleteNode(terminatedNodes[0].Id)
}

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeWithTags() {
	// test that a new node with uuid, no source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4254-2424-1322",
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		JobUuid:         "12345-389244-2433",
		LastContact:     nowTime,
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
		Tags: []*common.Kv{
			{Key: "my test", Value: "my val"},
			{Key: "environment", Value: "dev"},
		},
	}
	err := suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err := suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err := suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal("my really cool node", readNode.Name)
	suite.Equal([]*common.Kv{
		{Key: "my test", Value: "my val"},
		{Key: "environment", Value: "dev"},
	}, readNode.Tags)

	// send node in again with more tags
	node.Tags = []*common.Kv{
		{Key: "birds", Value: "are fun"},
		{Key: "archer", Value: "detective"},
	}
	err = suite.Database.ProcessIncomingNode(node)
	if err != nil {
		suite.FailNow(err.Error())
	}
	listNodes, _, err = suite.Database.GetNodes("", nodes.Query_ASC, 1, 100, []*common.Filter{})
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(1, len(listNodes))

	readNode, err = suite.Database.GetNode(ctx, listNodes[0].Id)
	if err != nil {
		suite.FailNow(err.Error())
	}

	suite.Equal([]*common.Kv{
		{Key: "my test", Value: "my val"},
		{Key: "environment", Value: "dev"},
		{Key: "birds", Value: "are fun"},
		{Key: "archer", Value: "detective"},
	}, readNode.Tags)
	_, err = suite.Database.DeleteNode(listNodes[0].Id)
}
