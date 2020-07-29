package publisher

import (
	"context"
	"testing"
	"time"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
)

func TestGatherInfoForNode(t *testing.T) {
	nowTime := time.Now().UTC()
	timestampNow, err := ptypes.TimestampProto(nowTime)
	assert.NoError(t, err)

	run := message.ChefRun{
		Platform: "redhat",
		Node: backend.Node{
			NodeInfo: backend.NodeInfo{
				EntityUuid:       "8dcca219-a730-3985-907b-e6b22f9f848d",
				NodeName:         "chef-load-44",
				PlatformFamily:   "rhel",
				PlatformVersion:  "8.9",
				ChefTags:         []string{"application", "database"},
				Status:           "success",
				OrganizationName: "test-org",
				SourceFqdn:       "chef-server-2",
				Roles:            []string{"my-cool-role"},
				Environment:      "test-env",
			},
			Checkin:        nowTime,
			LatestRunID:    "123353254545425",
			Projects:       []string{"tomato", "cucumber"},
			CloudID:        "i-0aee75f0b4b0d9f22",
			CloudAccountID: "123456789",
			CloudRegion:    "us-west-2",
		},
	}

	nodeMetadata, err := gatherInfoForNode(run)
	assert.NoError(t, err)
	assert.Equal(t, &manager.NodeMetadata{
		Uuid:            "8dcca219-a730-3985-907b-e6b22f9f848d",
		Name:            "chef-load-44",
		PlatformName:    "redhat",
		PlatformRelease: "8.9",
		Tags: []*common.Kv{
			{Key: "chef-tag", Value: "application"},
			{Key: "chef-tag", Value: "database"},
			{Key: "environment", Value: "test-env"},
		},
		LastContact:     timestampNow,
		SourceId:        "i-0aee75f0b4b0d9f22",
		SourceRegion:    "us-west-2",
		SourceAccountId: "123456789",
		RunData: &nodes.LastContactData{
			Id:      "123353254545425",
			EndTime: timestampNow,
			Status:  nodes.LastContactData_PASSED,
		},
		Projects: []string{"tomato", "cucumber"},
		ProjectsData: []*nodes.ProjectsData{
			{Key: "environment", Values: []string{"test-env"}},
			{Key: "roles", Values: []string{"my-cool-role"}},
			{Key: "organization_name", Values: []string{"test-org"}},
			{Key: "chef_tags", Values: []string{"application", "database"}},
			{Key: "chef_server", Values: []string{"chef-server-2"}},
		},
		ManagerType: "chef",
	}, nodeMetadata)
}

func TestBundlerSingleMessage(t *testing.T) {
	inbox := make(chan message.ChefRun, 100)
	processNodeCount := 0
	nodeMgrClient := manager.NewMockNodeManagerServiceClient(gomock.NewController(t))
	nodeMgrClient.EXPECT().ProcessNode(gomock.Any(), gomock.Any()).DoAndReturn(
		func(ctx interface{}, in interface{}) (*manager.ProcessNodeResponse, error) {
			processNodeCount++
			return &manager.ProcessNodeResponse{}, nil
		})
	errc := make(chan error)

	inbox <- message.NewChefRun(context.Background(), &chef.Run{}, errc)
	close(inbox)
	out := nodeManagerPublisher(inbox, nodeMgrClient, 1)

	<-out

	assert.Equal(t, 1, processNodeCount)
}
