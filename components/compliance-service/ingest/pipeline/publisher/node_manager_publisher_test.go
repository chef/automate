package publisher

import (
	"context"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/compliance/common"
	inspec "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"

	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
)

func TestGatherInfoForNode(t *testing.T) {
	nowTimeString := "2018-10-25T10:18:41Z"
	time, err := time.Parse(time.RFC3339, nowTimeString)
	assert.NoError(t, err)

	timestampNow, err := ptypes.TimestampProto(time)
	assert.NoError(t, err)

	nodeReport := message.Compliance{
		Report: compliance.Report{
			NodeUuid:          "8dcca219-a730-3985-907b-e6b22f9f848d",
			NodeName:          "chef-load-44",
			Platform:          &inspec.Platform{Name: "ubuntu", Release: "16.04"},
			ChefTags:          []string{"application", "database"},
			OrganizationName:  "test-org",
			SourceFqdn:        "chef-server-2",
			Roles:             []string{"my-cool-role"},
			EndTime:           nowTimeString,
			SourceId:          "i-0aee75f0b4b0d9f22",
			SourceRegion:      "us-west-2a",
			ReportUuid:        "123353254545425",
			AutomateManagerId: "12345",
			Environment:       "test-env",
		},
		InspecReport: &relaxting.ESInSpecReport{
			Projects: []string{"tomato", "cucumber"},
			Status:   "passed",
		},
	}

	nodeMetadata, err := gatherInfoForNode(nodeReport)
	assert.NoError(t, err)
	assert.Equal(t, &manager.NodeMetadata{
		Uuid:            "8dcca219-a730-3985-907b-e6b22f9f848d",
		Name:            "chef-load-44",
		PlatformName:    "ubuntu",
		PlatformRelease: "16.04",
		LastContact:     timestampNow,
		SourceId:        "i-0aee75f0b4b0d9f22",
		SourceRegion:    "us-west-2a",
		ScanData: &nodes.LastContactData{
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
		ManagerId: "12345",
		Tags: []*common.Kv{
			{Key: "chef-tag", Value: "application"},
			{Key: "chef-tag", Value: "database"},
			{Key: "environment", Value: "test-env"},
		},
		ManagerType: "chef",
	}, nodeMetadata)
}

func TestGatherInfoForNodeCollectsProjectsDataIfScanJob(t *testing.T) {
	nowTimeString := "2018-10-25T10:18:41Z"
	time, err := time.Parse(time.RFC3339, nowTimeString)
	assert.NoError(t, err)

	timestampNow, err := ptypes.TimestampProto(time)
	assert.NoError(t, err)

	nodeReport := message.Compliance{
		Report: compliance.Report{
			NodeUuid:     "8dcca219-a730-3985-907b-e6b22f9f848d",
			NodeName:     "my-node",
			Platform:     &inspec.Platform{Name: "ubuntu", Release: "16.04"},
			EndTime:      nowTimeString,
			SourceId:     "i-0aee75f0b4b0d9f22",
			SourceRegion: "us-west-2a",
			ReportUuid:   "123353254545425",
			JobUuid:      "12335892329454",
			Environment:  "test-env",
		},
		InspecReport: &relaxting.ESInSpecReport{
			Status: "passed",
		},
	}

	nodeMetadata, err := gatherInfoForNode(nodeReport)
	assert.NoError(t, err)
	assert.Equal(t, &manager.NodeMetadata{
		Uuid:            "8dcca219-a730-3985-907b-e6b22f9f848d",
		Name:            "my-node",
		PlatformName:    "ubuntu",
		PlatformRelease: "16.04",
		LastContact:     timestampNow,
		SourceId:        "i-0aee75f0b4b0d9f22",
		SourceRegion:    "us-west-2a",
		ScanData: &nodes.LastContactData{
			Id:      "123353254545425",
			EndTime: timestampNow,
			Status:  nodes.LastContactData_PASSED,
		},
		JobUuid: "12335892329454",
		ProjectsData: []*nodes.ProjectsData{
			{Key: "environment", Values: []string{"test-env"}},
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "test-env"},
		},
		ManagerType: "",
	}, nodeMetadata)
}

func TestBundlerSingleMessage(t *testing.T) {
	inbox := make(chan message.Compliance, 100)
	processNodeCount := 0
	nodeMgrClient := manager.NewMockNodeManagerServiceClient(gomock.NewController(t))
	nodeMgrClient.EXPECT().ProcessNode(gomock.Any(), gomock.Any()).DoAndReturn(
		func(ctx interface{}, in interface{}) (*manager.ProcessNodeResponse, error) {
			processNodeCount++
			return &manager.ProcessNodeResponse{}, nil
		})
	done := make(chan error)
	ctx := context.Background()

	inbox <- message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: "", Roles: []string{}},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}
	close(inbox)
	out := nodeManagerPublisher(inbox, nodeMgrClient)

	<-out

	assert.Equal(t, 1, processNodeCount)
}
