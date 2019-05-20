package publisher

import (
	"context"
	"testing"
	"time"

	inspec "github.com/chef/automate/components/compliance-service/ingest/events/inspec"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
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
			NodeUuid:         "8dcca219-a730-3985-907b-e6b22f9f848d",
			NodeName:         "chef-load-44",
			Platform:         &inspec.Platform{Name: "ubuntu", Release: "16.04"},
			ChefTags:         []string{"application", "database"},
			OrganizationName: "test-org",
			SourceFqdn:       "chef-server-2",
			Roles:            []string{"my-cool-role"},
			EndTime:          nowTimeString,
			SourceId:         "i-0aee75f0b4b0d9f22",
			SourceRegion:     "us-west-2a",
			ReportUuid:       "123353254545425",
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
		ProjectsData: map[string]*manager.ProjectsValues{
			"chef_server":       &manager.ProjectsValues{Values: []string{"chef-server-2"}},
			"organization_name": &manager.ProjectsValues{Values: []string{"test-org"}},
			"roles":             &manager.ProjectsValues{Values: []string{"my-cool-role"}},
			"chef_tags":         &manager.ProjectsValues{Values: []string{"application", "database"}},
		},
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
