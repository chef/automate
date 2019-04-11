package v2_test

import (
	"context"
	"testing"
	"time"

	automate_event "github.com/chef/automate/api/interservice/event"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	project_update_tags "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc"
)

func TestProjectUpdateManagerOneUpdateRunningAtATime(t *testing.T) {
	mockEventServiceClient := &MockEventServiceClient{}
	manager := v2.NewProjectUpdateManager(mockEventServiceClient)

	assert.Equal(t, v2.NotRunningState, manager.State())

	originalNumberOfPublishedEvents := mockEventServiceClient.PublishedEvents

	err := manager.Start()
	assert.NoError(t, err)

	assert.Equal(t, originalNumberOfPublishedEvents+1, mockEventServiceClient.PublishedEvents)
	assert.Equal(t, automate_event_type.ProjectRulesUpdate,
		mockEventServiceClient.LastestPublishedEvent.Type.Name)
	assert.Equal(t, v2.RunningState, manager.State())

	// Starting a second update without finishing the first one should return an error.
	err = manager.Start()
	assert.Error(t, err)

	// No extra events were published
	assert.Equal(t, originalNumberOfPublishedEvents+1, mockEventServiceClient.PublishedEvents)

	assert.Equal(t, v2.RunningState, manager.State())
}

func TestProjectUpdateManagerFinishesAfterCompletStatusMessages(t *testing.T) {
	mockEventServiceClient := &MockEventServiceClient{}
	manager := v2.NewProjectUpdateManager(mockEventServiceClient)
	assert.Equal(t, v2.NotRunningState, manager.State())

	err := manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, v2.RunningState, manager.State())

	eventData := mockEventServiceClient.LastestPublishedEvent.Data

	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := createStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompeleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusMessage(infraStatusEvent)

	complianceStatusEvent := createStatusEventMsg(
		projectUpdateIDTag, // projectUpdateID not matching current
		0.0,                // EstimatedTimeCompeleteInSec
		1.0,                // percentageComplete
		true,               // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusMessage(complianceStatusEvent)

	assert.Equal(t, v2.NotRunningState, manager.State())
}

func TestProjectUpdateManagerNotFinishAfterOldCompletStatusMessages(t *testing.T) {
	mockEventServiceClient := &MockEventServiceClient{}
	manager := v2.NewProjectUpdateManager(mockEventServiceClient)
	assert.Equal(t, v2.NotRunningState, manager.State())

	err := manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, v2.RunningState, manager.State())

	eventData := mockEventServiceClient.LastestPublishedEvent.Data
	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := createStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompeleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	err = manager.ProcessStatusMessage(infraStatusEvent)
	assert.NoError(t, err)

	complianceStatusEvent := createStatusEventMsg(
		"Not-active-project-update-id", // projectUpdateID not matching current
		0.0,                            // EstimatedTimeCompeleteInSec
		1.0,                            // percentageComplete
		true,                           // completed
		event_ids.ComplianceInspecReportProducerID)

	err = manager.ProcessStatusMessage(complianceStatusEvent)
	assert.NoError(t, err)

	assert.Equal(t, v2.RunningState, manager.State())
}

// the compliance status is estimated to be longer so its percentage will be used.
func TestProjectUpdateManagerPercentageComplete(t *testing.T) {
	mockEventServiceClient := &MockEventServiceClient{}
	manager := v2.NewProjectUpdateManager(mockEventServiceClient)
	assert.Equal(t, v2.NotRunningState, manager.State())

	err := manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, v2.RunningState, manager.State())

	eventData := mockEventServiceClient.LastestPublishedEvent.Data

	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := createStatusEventMsg(projectUpdateIDTag,
		1554844823.0, // This update is estimated to be longer than infra
		0.8,          // percentageComplete
		false,        // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusMessage(infraStatusEvent)

	complianceStatusEvent := createStatusEventMsg(projectUpdateIDTag,
		1554845823.0, // This update is estimated to be longer than infra
		0.4,          // percentageComplete
		false,        // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusMessage(complianceStatusEvent)

	assert.InDelta(t, 0.4, manager.PercentageComplete(), 0.001)
	assert.Equal(t, time.Unix(1554845823, 0), manager.EstimatedTimeCompelete())
}

func TestProjectUpdateManagerPercentageCompleteAllComplete(t *testing.T) {
	mockEventServiceClient := &MockEventServiceClient{}
	manager := v2.NewProjectUpdateManager(mockEventServiceClient)
	assert.Equal(t, v2.NotRunningState, manager.State())

	err := manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, v2.RunningState, manager.State())

	eventData := mockEventServiceClient.LastestPublishedEvent.Data

	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := createStatusEventMsg(projectUpdateIDTag,
		1554844823.0, // This update is estimated to be longer than infra
		0.8,          // percentageComplete
		true,         // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusMessage(infraStatusEvent)

	complianceStatusEvent := createStatusEventMsg(projectUpdateIDTag,
		1554845823.0, // This update is estimated to be longer than infra
		0.4,          // percentageComplete
		true,         // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusMessage(complianceStatusEvent)

	assert.InDelta(t, 1.0, manager.PercentageComplete(), 0.001)
	assert.Equal(t, time.Time{}, manager.EstimatedTimeCompelete())
}

func TestProjectUpdateManagerFailureMessagesOldUpdate(t *testing.T) {
	mockEventServiceClient := &MockEventServiceClient{}
	manager := v2.NewProjectUpdateManager(mockEventServiceClient)
	assert.Equal(t, v2.NotRunningState, manager.State())

	err := manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, v2.RunningState, manager.State())

	eventData := mockEventServiceClient.LastestPublishedEvent.Data
	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := createStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompeleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	err = manager.ProcessStatusMessage(infraStatusEvent)
	assert.NoError(t, err)

	complianceStatusEvent := createFailureEventMsg(
		"Not-active-project-update-id", // projectUpdateID not matching current
		event_ids.ComplianceInspecReportProducerID)

	// Get an error when processing the message.
	err = manager.ProcessFailMessage(complianceStatusEvent)
	assert.NoError(t, err)
}

func createFailureEventMsg(projectUpdateIDTag string, producer string) *automate_event.EventMsg {
	return &automate_event.EventMsg{
		EventID:   "event-id-2",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateFailed},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: producer,
		},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: projectUpdateIDTag,
					},
				},
			},
		},
	}
}

func createStatusEventMsg(projectUpdateIDTag string, estimatedTimeCompeleteInSec float64,
	percentageComplete float64, completed bool, producer string) *automate_event.EventMsg {
	return &automate_event.EventMsg{
		EventID:   "event-id-2",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateStatus},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: producer,
		},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				"Completed": &_struct.Value{
					Kind: &_struct.Value_BoolValue{
						BoolValue: completed,
					},
				},
				"PercentageComplete": &_struct.Value{
					Kind: &_struct.Value_NumberValue{
						NumberValue: percentageComplete,
					},
				},
				"EstimatedTimeCompeleteInSec": &_struct.Value{
					Kind: &_struct.Value_NumberValue{
						NumberValue: estimatedTimeCompeleteInSec,
					},
				},
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: projectUpdateIDTag,
					},
				},
			},
		},
	}
}

type MockEventServiceClient struct {
	PublishedEvents       int
	LastestPublishedEvent *automate_event.EventMsg
}

func (m *MockEventServiceClient) Publish(ctx context.Context,
	in *automate_event.PublishRequest,
	opts ...grpc.CallOption) (*automate_event.PublishResponse, error) {
	m.PublishedEvents++
	m.LastestPublishedEvent = in.Msg
	return &automate_event.PublishResponse{}, nil
}

func (m *MockEventServiceClient) Subscribe(ctx context.Context,
	in *automate_event.SubscribeRequest,
	opts ...grpc.CallOption) (*automate_event.SubscribeResponse, error) {
	return &automate_event.SubscribeResponse{}, nil
}

func (m *MockEventServiceClient) Start(ctx context.Context,
	in *automate_event.StartRequest,
	opts ...grpc.CallOption) (*automate_event.StartResponse, error) {
	return &automate_event.StartResponse{}, nil
}

func (m *MockEventServiceClient) Stop(ctx context.Context,
	in *automate_event.StopRequest,
	opts ...grpc.CallOption) (*automate_event.StopResponse, error) {
	return &automate_event.StopResponse{}, nil
}
