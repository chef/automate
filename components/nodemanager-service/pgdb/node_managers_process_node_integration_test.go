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
		LastContact:     nowTime,
		SourceId:        "i-078973",
		SourceRegion:    "eu-west-1",
		SourceAccountId: "8799247840",
		ScanData: &nodes.LastContactData{
			Id: "12345-9999-002323",
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
		ScanData: &nodes.LastContactData{
			Id:      "12345-9999-002323",
			EndTime: nowTime,
			Status:  nodes.LastContactData_PASSED,
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
		LastContact:     nowTime,
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
		ScanData: &nodes.LastContactData{
			Id: "12345-9999-002323",
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
		LastContact:     timestamp,
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
		ScanData: &nodes.LastContactData{
			Id: "12345-9999-002323",
		},
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
		LastContact:     ptypes.TimestampNow(),
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
		ScanData: &nodes.LastContactData{
			Id: "12345-9999-002323",
		},
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

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeWithUUIDAndScanData() {
	// test that a new node with uuid, no source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4254-2424-1322",
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		LastContact:     nowTime,
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
		JobUuid:         "12343-232324-1231242",
		ScanData: &nodes.LastContactData{
			Id:      "1003-9254-2004-1322",
			EndTime: nowTime,
			Status:  nodes.LastContactData_PASSED,
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
	suite.Equal("debian", readNode.Platform)
	suite.InDelta(nowTime.GetSeconds(), readNode.LastContact.GetSeconds(), 1)
	suite.Equal(nodes.LastContactData_PASSED, readNode.GetScanData().Status)

	// now send the same node info through again, expect the node to be updated,
	// and have the updated last_contact time, and updated statuses
	nowTime2 := ptypes.TimestampNow()
	node.LastContact = nowTime2
	node.ScanData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime2,
		Status:  nodes.LastContactData_FAILED,
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
	suite.Equal("my really cool node", readNode.Name)
	suite.Equal("debian", readNode.Platform)
	suite.InDelta(nowTime2.GetSeconds(), readNode.LastContact.GetSeconds(), 1)
	suite.NotEqual(nil, readNode.GetScanData())
	suite.Equal(nodes.LastContactData_FAILED, readNode.GetScanData().GetStatus())
	suite.Equal(nodes.LastContactData_PASSED, readNode.GetScanData().GetPenultimateStatus())

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

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeWithDiffSourceInfoScanDataAndSourceInfo() {
	// test that a new node with uuid, no source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4254-2424-1322",
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		LastContact:     nowTime,
		SourceId:        "i-09837523",
		SourceRegion:    "us-east-1",
		SourceAccountId: "2398433048610242",
		JobUuid:         "12343-232324-1231242",
		ScanData: &nodes.LastContactData{
			Id:      "1003-9254-2004-1322",
			EndTime: nowTime,
			Status:  nodes.LastContactData_PASSED,
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
	suite.InDelta(nowTime.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	// now send the same node info through again, with a diff uuid but same source info
	nowTime2 := ptypes.TimestampNow()
	node.Uuid = "122433-9038543-41433"
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
	suite.InDelta(nowTime2.GetSeconds(), readNode.LastContact.GetSeconds(), 1)

	_, err = suite.Database.DeleteNode(listNodes[0].Id)
}

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeUUIDWithTags() {
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
		RunData: &nodes.LastContactData{
			Id:      "1003-9254-2004-1322",
			EndTime: nowTime,
			Status:  nodes.LastContactData_PASSED,
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

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeSourceInfoWithTags() {
	// test that a new node with uuid, no source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4784-2424-1389",
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		JobUuid:         "12345-389244-2433",
		LastContact:     nowTime,
		SourceId:        "i-078973",
		SourceRegion:    "eu-west-1",
		SourceAccountId: "8799247840",
		Tags: []*common.Kv{
			{Key: "my test", Value: "my val"},
			{Key: "environment", Value: "dev"},
		},
		ScanData: &nodes.LastContactData{
			Id:      "1003-9254-2004-1322",
			EndTime: nowTime,
			Status:  nodes.LastContactData_PASSED,
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

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeRejectsNodeWithNoRunOrScanData() {
	// test that a new node with uuid, no source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4784-2424-1389",
		Name:            "my really cool node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		JobUuid:         "12345-389244-2433",
		LastContact:     nowTime,
		SourceId:        "i-078973",
		SourceRegion:    "eu-west-1",
		SourceAccountId: "8799247840",
		Tags: []*common.Kv{
			{Key: "my test", Value: "my val"},
			{Key: "environment", Value: "dev"},
		},
	}
	err := suite.Database.ProcessIncomingNode(node)
	suite.Equal("ProcessIncomingNode unable to parse node last contact data: invalid request: scan_data or run_data must be provided", err.Error())
}

func (suite *NodeManagersAndNodesDBSuite) TestProcessIncomingNodeWithUUIDAndRunData() {
	// test that a new node with uuid, no source_id makes it into the db
	// with the right info and is readable
	nowTime := ptypes.TimestampNow()
	node := &manager.NodeMetadata{
		Uuid:            "1223-4254-2424-1322",
		Name:            "my really client run node",
		PlatformName:    "debian",
		PlatformRelease: "8.6",
		LastContact:     nowTime,
		SourceId:        "",
		SourceRegion:    "",
		SourceAccountId: "",
		JobUuid:         "12343-232324-1231242",
		RunData: &nodes.LastContactData{
			Id:      "1003-9254-2004-1322",
			EndTime: nowTime,
			Status:  nodes.LastContactData_PASSED,
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
	suite.Equal("my really client run node", readNode.Name)
	suite.Equal("debian", readNode.Platform)
	suite.InDelta(nowTime.GetSeconds(), readNode.LastContact.GetSeconds(), 1)
	suite.Equal(nodes.LastContactData_PASSED, readNode.GetRunData().Status)

	// now send the same node info through again, expect the node to be updated,
	// and have the updated last_contact time, and updated statuses
	nowTime2 := ptypes.TimestampNow()
	node.LastContact = nowTime2
	node.RunData = &nodes.LastContactData{
		Id:      "1003-9254-2004-1322",
		EndTime: nowTime2,
		Status:  nodes.LastContactData_FAILED,
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
	suite.Equal("my really client run node", readNode.Name)
	suite.Equal("debian", readNode.Platform)
	suite.InDelta(nowTime2.GetSeconds(), readNode.LastContact.GetSeconds(), 1)
	suite.NotEqual(nil, readNode.GetRunData())
	suite.Equal(nodes.LastContactData_FAILED, readNode.GetRunData().Status)
	suite.Equal(nodes.LastContactData_PASSED, readNode.GetRunData().PenultimateStatus)

	_, err = suite.Database.DeleteNode(listNodes[0].Id)
}
