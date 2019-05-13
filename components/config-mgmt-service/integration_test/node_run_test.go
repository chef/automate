//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"google.golang.org/grpc/codes"

	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestNodeRunEmptyRequestReturnsError(t *testing.T) {
	ctx := context.Background()
	req := request.NodeRun{}

	res, err := cfgmgmt.GetNodeRun(ctx, &req)
	grpctest.AssertCode(t, codes.InvalidArgument, err)
	assert.Nil(t, res)
}

func TestNodeRunWithANodeNotFoundReturnsError(t *testing.T) {
	ctx := context.Background()
	req := request.NodeRun{RunId: "FAKE"}

	res, err := cfgmgmt.GetNodeRun(ctx, &req)
	grpctest.AssertCode(t, codes.NotFound, err)
	assert.Nil(t, res)
}

func TestNodeRunWithRuns(t *testing.T) {
	var (
		nodeID = newUUID()
		run    = newIngestRun(nodeID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Minute).Format(time.RFC3339))
		req  = request.NodeRun{NodeId: run.EntityUuid, RunId: run.RunID}
		ctx  = context.Background()
		node = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
		}
	)

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns([]iBackend.Run{run})
	defer suite.DeleteAllDocuments()

	t.Run(fmt.Sprintf("with request '%v' should return the node run", req),
		func(t *testing.T) {
			res, err := cfgmgmt.GetNodeRun(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, ingestRunToMessage(run), res)
		})
}

func TestNodeRunWithRunsNonmatchingNode(t *testing.T) {
	var (
		nodeID = newUUID()
		run    = newIngestRun(nodeID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Minute).Format(time.RFC3339))
		req  = request.NodeRun{NodeId: run.EntityUuid, RunId: run.RunID}
		ctx  = context.Background()
		node = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: "not-matching-run",
			},
		}
	)

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns([]iBackend.Run{run})
	defer suite.DeleteAllDocuments()

	_, err := cfgmgmt.GetNodeRun(ctx, &req)
	assert.Error(t, err)
}

func TestNodeRunProjectFilter(t *testing.T) {
	var (
		nodeID = newUUID()
		run    = newIngestRun(nodeID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Minute).Format(time.RFC3339))
		req                = request.NodeRun{NodeId: run.EntityUuid, RunId: run.RunID}
		successfulResponse = ingestRunToMessage(run)
	)

	cases := []struct {
		description   string
		ctx           context.Context
		nodeProjects  []string
		expectedError bool
	}{
		{
			description:   "Node project matching request projects",
			ctx:           contextWithProjects([]string{"project9"}),
			nodeProjects:  []string{"project9"},
			expectedError: false,
		},
		{
			description:   "Node project not matching request projects",
			ctx:           contextWithProjects([]string{"project3"}),
			nodeProjects:  []string{"project9"},
			expectedError: true,
		},
		{
			description:   "Node one project; request all projects allowed",
			ctx:           contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			nodeProjects:  []string{"project9"},
			expectedError: false,
		},
		{
			description:   "Node has no projects; request all projects allowed",
			ctx:           contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			nodeProjects:  []string{},
			expectedError: false,
		},
		{
			description:   "Node has no projects; request unassigned projects allowed",
			ctx:           contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			nodeProjects:  []string{},
			expectedError: false,
		},
		{
			description:   "Node has a project; request only unassigned projects",
			ctx:           contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			nodeProjects:  []string{"project9"},
			expectedError: true,
		},
		{
			description:   "Node has a projects; request unassigned and matching project allowed",
			ctx:           contextWithProjects([]string{authzConstants.UnassignedProjectID, "project9"}),
			nodeProjects:  []string{"project9"},
			expectedError: false,
		},
		{
			description:   "Node has no projects; request a project not allowed",
			ctx:           contextWithProjects([]string{}),
			nodeProjects:  []string{},
			expectedError: false,
		},
		{
			description:   "Node with one project matching one of several requested projects allowed",
			ctx:           contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			nodeProjects:  []string{"project9"},
			expectedError: false,
		},
		{
			description:   "Node with one project not matching any of several requested projects allowed",
			ctx:           contextWithProjects([]string{"project3", "project4", "project7", "project6"}),
			nodeProjects:  []string{"project9"},
			expectedError: true,
		},
		{
			description:   "Node with several projects where one matches a single requested project allowed",
			ctx:           contextWithProjects([]string{"project3"}),
			nodeProjects:  []string{"project3", "project4", "project7", "project6"},
			expectedError: false,
		},
		{
			description:   "Node with several projects where one matches one of several requested project allowed",
			ctx:           contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			nodeProjects:  []string{"project3", "project4", "project7", "project6"},
			expectedError: false,
		},
		{
			description:   "Node with several projects where none matches several requested project allowed",
			ctx:           contextWithProjects([]string{"project14", "project10", "project12", "project13"}),
			nodeProjects:  []string{"project3", "project4", "project7", "project6"},
			expectedError: true,
		},
		{
			description:   "Node with several projects where two matches two of several requested project allowed",
			ctx:           contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			nodeProjects:  []string{"project3", "project10", "project7", "project6"},
			expectedError: false,
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			node := iBackend.Node{
				Exists:   true,
				Projects: test.nodeProjects,
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: nodeID,
				},
			}
			suite.IngestNodes([]iBackend.Node{node})
			suite.IngestRuns([]iBackend.Run{run})
			defer suite.DeleteAllDocuments()

			res, err := cfgmgmt.GetNodeRun(test.ctx, &req)
			if test.expectedError {
				assert.Error(t, err)
				return
			}
			assert.Nil(t, err)
			assert.Equal(t, successfulResponse, res)
		})
	}
}

func TestNodeRunWithCookbookRunlist(t *testing.T) {
	var (
		nodeID = newUUID()
		run    = newIngestRun(nodeID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Minute).Format(time.RFC3339))
		req  = request.NodeRun{NodeId: run.EntityUuid, RunId: run.RunID}
		ctx  = context.Background()
		node = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
		}
	)
	expandedRunListRunList := make([]iBackend.ExpandedRunListRunList, 1)
	expandedRunListRunList[0] = iBackend.ExpandedRunListRunList{
		Type:    "recipe",
		Name:    "chef-client::default",
		Skipped: false,
	}
	run.ExpandedRunList.ID = "_default"
	run.ExpandedRunList.RunList = expandedRunListRunList

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns([]iBackend.Run{run})
	defer suite.DeleteAllDocuments()

	t.Run(fmt.Sprintf("with request '%v' should return the node run", req),
		func(t *testing.T) {
			res, err := cfgmgmt.GetNodeRun(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, ingestRunToMessage(run), res)
		})
}

func TestNodeRunWithOneRoleRunlist(t *testing.T) {
	var (
		nodeID = newUUID()
		run    = newIngestRun(nodeID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Minute).Format(time.RFC3339))
		req  = request.NodeRun{NodeId: run.EntityUuid, RunId: run.RunID}
		ctx  = context.Background()
		node = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
		}
	)
	expandedRunListRunList := make([]iBackend.ExpandedRunListRunList, 1)
	expandedRunListRunList[0] = iBackend.ExpandedRunListRunList{
		Type:    "role",
		Name:    "web",
		Skipped: false,
		Children: []iBackend.ExpandedRunListRunList{
			{
				Type:    "recipe",
				Name:    "chef-client::default",
				Skipped: false,
			},
			{
				Type:    "recipe",
				Name:    "chef-client::delete_validation",
				Skipped: false,
			},
			{
				Type:    "recipe",
				Name:    "lamp2::default",
				Skipped: false,
			},
		},
	}
	run.ExpandedRunList.ID = "_default"
	run.ExpandedRunList.RunList = expandedRunListRunList

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns([]iBackend.Run{run})
	defer suite.DeleteAllDocuments()

	t.Run(fmt.Sprintf("with request '%v' should return the node run", req),
		func(t *testing.T) {
			res, err := cfgmgmt.GetNodeRun(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, ingestRunToMessage(run), res)
		})
}

func TestNodeRunWithTwoRolesInRunlist(t *testing.T) {
	var (
		nodeID = newUUID()
		run    = newIngestRun(nodeID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Minute).Format(time.RFC3339))
		req  = request.NodeRun{NodeId: run.EntityUuid, RunId: run.RunID}
		ctx  = context.Background()
		node = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
		}
	)
	expandedRunListRunList := make([]iBackend.ExpandedRunListRunList, 2)
	expandedRunListRunList[0] = iBackend.ExpandedRunListRunList{
		Type:    "role",
		Name:    "none",
		Skipped: false,
		Children: []iBackend.ExpandedRunListRunList{
			{
				Type:    "recipe",
				Name:    "test::default",
				Skipped: false,
			},
		},
	}
	expandedRunListRunList[1] = iBackend.ExpandedRunListRunList{
		Type:    "role",
		Name:    "web",
		Skipped: false,
		Children: []iBackend.ExpandedRunListRunList{
			{
				Type:    "recipe",
				Name:    "chef-client::default",
				Skipped: false,
			},
			{
				Type:    "recipe",
				Name:    "chef-client::delete_validation",
				Skipped: false,
			},
			{
				Type:    "recipe",
				Name:    "lamp2::default",
				Skipped: false,
			},
		},
	}
	run.ExpandedRunList.ID = "_default"
	run.ExpandedRunList.RunList = expandedRunListRunList

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns([]iBackend.Run{run})
	defer suite.DeleteAllDocuments()

	t.Run(fmt.Sprintf("with request '%v' should return the node run", req),
		func(t *testing.T) {
			res, err := cfgmgmt.GetNodeRun(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, ingestRunToMessage(run), res)
		})
}

func TestNodeRunWithLoopRolesInRunlist(t *testing.T) {
	var (
		nodeID = newUUID()
		run    = newIngestRun(nodeID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Minute).Format(time.RFC3339))
		req  = request.NodeRun{NodeId: run.EntityUuid, RunId: run.RunID}
		ctx  = context.Background()
		node = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
		}
	)
	expandedRunListRunList := make([]iBackend.ExpandedRunListRunList, 2)
	expandedRunListRunList[0] = iBackend.ExpandedRunListRunList{
		Type:    "role",
		Name:    "none",
		Skipped: false,
		Children: []iBackend.ExpandedRunListRunList{
			{
				Type:    "recipe",
				Name:    "test::default",
				Skipped: false,
			},
		},
	}
	expandedRunListRunList[1] = iBackend.ExpandedRunListRunList{
		Type:    "role",
		Name:    "web3",
		Skipped: false,
		Children: []iBackend.ExpandedRunListRunList{
			{
				Type:    "recipe",
				Name:    "chef-client::default",
				Skipped: false,
			},
			{
				Type:    "recipe",
				Name:    "chef-client::delete_validation",
				Skipped: false,
			},
			{
				Type:    "recipe",
				Name:    "lamp2::default",
				Skipped: false,
			},
			{
				Type:    "role",
				Name:    "none",
				Skipped: false,
				Children: []iBackend.ExpandedRunListRunList{
					{
						Type:    "recipe",
						Name:    "test::default",
						Skipped: false,
					},
					{
						Type:    "role",
						Name:    "web3",
						Skipped: true,
					},
				},
			},
		},
	}
	run.ExpandedRunList.ID = "_default"
	run.ExpandedRunList.RunList = expandedRunListRunList

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns([]iBackend.Run{run})
	defer suite.DeleteAllDocuments()

	t.Run(fmt.Sprintf("with request '%v' should return the node run", req),
		func(t *testing.T) {
			res, err := cfgmgmt.GetNodeRun(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, ingestRunToMessage(run), res)
		})
}

func ingestRunToMessage(run iBackend.Run) proto.Message {
	startTime, _ := ptypes.TimestampProto(run.StartTime)
	endTime, _ := ptypes.TimestampProto(run.EndTime)
	return &response.Run{
		Id:                   run.RunID,
		NodeId:               run.EntityUuid,
		NodeName:             run.NodeName,
		Fqdn:                 run.Fqdn,
		StartTime:            startTime,
		EndTime:              endTime,
		UptimeSeconds:        int32(run.UptimeSeconds),
		Organization:         run.OrganizationName,
		Environment:          run.Environment,
		Platform:             run.Platform,
		PlatformFamily:       run.PlatformFamily,
		PlatformVersion:      run.PlatformVersion,
		Status:               run.Status,
		SourceFqdn:           run.SourceFqdn,
		PolicyName:           run.PolicyName,
		PolicyGroup:          run.PolicyGroup,
		PolicyRevision:       run.PolicyRevision,
		TotalResourceCount:   int32(run.TotalResourceCount),
		UpdatedResourceCount: int32(run.UpdatedResourceCount),
		ExpandedRunList: &response.ExpandedRunList{ // For now we initialize them but would be nice to cast them
			Id:      run.ExpandedRunList.ID,
			RunList: toResponseExpandedRunList(run.ExpandedRunList.RunList),
		},
		Error:        &response.ChefError{},     // For now we initialize them but would be nice to cast them
		Resources:    []*response.Resource{},    // For now we initialize them but would be nice to cast them
		Deprecations: []*response.Deprecation{}, // For now we initialize them but would be nice to cast them
		// (@afiune) I have no idea what is this 'source'
		//Source:      run.Source,
	}
}

func toResponseExpandedRunList(runList []iBackend.ExpandedRunListRunList) []*response.RunList {
	responseRunList := make([]*response.RunList, len(runList))
	for index, runList := range runList {
		responseRunList[index] = toResponseRunList(runList)
	}

	return responseRunList
}

func toResponseRunList(runListItem iBackend.ExpandedRunListRunList) *response.RunList {
	children := make([]*response.RunList, len(runListItem.Children))
	for index, runListChild := range runListItem.Children {
		children[index] = toResponseRunList(runListChild)
	}
	version, ok := runListItem.Version.(string)

	if !ok {
		version = ""
	}

	return &response.RunList{
		Type:     runListItem.Type,
		Name:     runListItem.Name,
		Version:  version,
		Skipped:  runListItem.Skipped,
		Children: children,
	}
}
