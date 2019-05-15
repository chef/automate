package publisher

import (
	"context"
	"testing"
	"time"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
)

func TestGatherInfoForNode(t *testing.T) {
	nowTime := time.Now().UTC()
	timestampNow, err := ptypes.TimestampProto(nowTime)
	assert.NoError(t, err)

	backendNode := backend.Node{
		NodeInfo: backend.NodeInfo{
			EntityUuid:      "8dcca219-a730-3985-907b-e6b22f9f848d",
			NodeName:        "chef-load-44",
			Platform:        "ubuntu",
			PlatformVersion: "16.04",
			ChefTags:        []string{"application", "database"},
			Status:          "success",
		},
		Checkin: nowTime,
		Ec2: backend.Ec2{
			InstanceId:                "i-0aee75f0b4b0d9f22",
			PlacementAvailabilityZone: "us-west-2a",
		},
		LatestRunID: "123353254545425",
	}

	nodeMetadata, err := gatherInfoForNode(backendNode)
	assert.NoError(t, err)
	assert.Equal(t, &manager.NodeMetadata{
		Uuid:            "8dcca219-a730-3985-907b-e6b22f9f848d",
		Name:            "chef-load-44",
		PlatformName:    "ubuntu",
		PlatformRelease: "16.04",
		Tags: []*common.Kv{
			{Key: "chef-tag", Value: "application"},
			{Key: "chef-tag", Value: "database"},
		},
		LastContact:  timestampNow,
		SourceId:     "i-0aee75f0b4b0d9f22",
		SourceRegion: "us-west-2a",
		RunData: &nodes.LastContactData{
			Id:      "123353254545425",
			EndTime: timestampNow,
			Status:  nodes.LastContactData_PASSED,
		},
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
	out := nodeManagerPublisher(inbox, nodeMgrClient)

	<-out

	assert.Equal(t, 1, processNodeCount)
}
