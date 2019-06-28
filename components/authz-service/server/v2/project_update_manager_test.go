package v2_test

import (
	"os"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/authz-service/config"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	"github.com/chef/automate/components/authz-service/testhelpers"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	project_update_tags "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
)

func TestProjectUpdateManagerOneUpdateRunningAtATime(t *testing.T) {
	numberOfPublishedEvents := 0
	var lastestPublishedEvent *automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			numberOfPublishedEvents++
			lastestPublishedEvent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)

	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)

	assert.Equal(t, config.NotRunningState, manager.State())

	originalNumberOfPublishedEvents := numberOfPublishedEvents

	err = manager.Start()
	assert.NoError(t, err)

	assert.Equal(t, originalNumberOfPublishedEvents+1, numberOfPublishedEvents)
	assert.Equal(t, automate_event_type.ProjectRulesUpdate, lastestPublishedEvent.Type.Name)
	assert.Equal(t, config.RunningState, manager.State())

	// Starting a second update without finishing the first one should return an error.
	err = manager.Start()
	assert.Error(t, err)

	// No extra events were published
	assert.Equal(t, originalNumberOfPublishedEvents+1, numberOfPublishedEvents)

	assert.Equal(t, config.RunningState, manager.State())
}

func TestProjectUpdateManagerFinishesAfterCompletStatusMessages(t *testing.T) {
	var eventsSent []*automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			eventsSent = append(eventsSent, in.Msg)
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)

	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)
	assert.Equal(t, config.NotRunningState, manager.State())

	err = manager.Start()
	assert.NoError(t, err)

	testhelpers.WaitForWithTimeout(t, func() bool {
		return config.RunningState == manager.State()
	}, time.Second*3, "State did not switch to Running")

	waitFor(func() bool {
		return len(eventsSent) > 0
	})

	eventData := eventsSent[0].Data

	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusEvent(infraStatusEvent)

	complianceStatusEvent := testhelpers.CreateStatusEventMsg(
		projectUpdateIDTag, // projectUpdateID not matching current
		0.0,                // EstimatedTimeCompleteInSec
		1.0,                // percentageComplete
		true,               // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusEvent(complianceStatusEvent)

	testhelpers.WaitForWithTimeout(t, func() bool {
		return config.NotRunningState == manager.State()
	}, time.Second*3, "State did not switch to NotRunning")
}

func TestProjectUpdateManagerSendCancelEvent(t *testing.T) {
	var eventsSent []*automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			eventsSent = append(eventsSent, in.Msg)
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)
	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)
	assert.Equal(t, config.NotRunningState, manager.State())

	err = manager.Start()
	assert.NoError(t, err)
	testhelpers.WaitForWithTimeout(t, func() bool {
		return config.RunningState == manager.State()
	}, time.Second*3, "State did not switch to Running")

	waitFor(func() bool {
		return len(eventsSent) > 0
	})

	eventData := eventsSent[0].Data

	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusEvent(infraStatusEvent)

	manager.Cancel()
	testhelpers.WaitForWithTimeout(t, func() bool {
		for _, event := range eventsSent {
			if event.Type.Name == automate_event_type.ProjectRulesCancelUpdate {
				return true
			}
		}
		return config.RunningState == manager.State()
	}, time.Second*3, "Cancel event not sent")

	time.Sleep(time.Millisecond * 100)
	// State does not change from running cancel
	assert.Equal(t, config.RunningState, manager.State())

	complianceStatusEvent := testhelpers.CreateStatusEventMsg(
		projectUpdateIDTag, // projectUpdateID not matching current
		0.0,                // EstimatedTimeCompleteInSec
		1.0,                // percentageComplete
		true,               // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusEvent(complianceStatusEvent)

	testhelpers.WaitForWithTimeout(t, func() bool {
		return config.NotRunningState == manager.State()
	}, time.Second*3, "State did not switch to NotRunning")
}

func TestProjectUpdateManagerNoCancelEventSent(t *testing.T) {
	var eventsSent []*automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			eventsSent = append(eventsSent, in.Msg)
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)
	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)
	assert.Equal(t, config.NotRunningState, manager.State())

	err = manager.Start()
	assert.NoError(t, err)
	testhelpers.WaitForWithTimeout(t, func() bool {
		return config.RunningState == manager.State()
	}, time.Second*3, "State did not switch to Running")

	waitFor(func() bool {
		return len(eventsSent) > 0
	})

	eventData := eventsSent[0].Data
	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusEvent(infraStatusEvent)

	complianceStatusEvent := testhelpers.CreateStatusEventMsg(
		projectUpdateIDTag, // projectUpdateID not matching current
		0.0,                // EstimatedTimeCompleteInSec
		1.0,                // percentageComplete
		true,               // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusEvent(complianceStatusEvent)

	testhelpers.WaitForWithTimeout(t, func() bool {
		return config.NotRunningState == manager.State()
	}, time.Second*3, "State did not switch to NotRunning")

	manager.Cancel()
	time.Sleep(time.Millisecond * 100)
	// Check that no cancel event is sent.
	for _, event := range eventsSent {
		assert.NotEqual(t, automate_event_type.ProjectRulesCancelUpdate, event.Type.Name)
	}
}

func TestProjectUpdateManagerNotFinishAfterOldCompletStatusMessages(t *testing.T) {
	var lastestPublishedEvent *automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastestPublishedEvent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)
	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)
	assert.Equal(t, config.NotRunningState, manager.State())

	err = manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, config.RunningState, manager.State())

	eventData := lastestPublishedEvent.Data
	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	err = manager.ProcessStatusEvent(infraStatusEvent)
	assert.NoError(t, err)

	complianceStatusEvent := testhelpers.CreateStatusEventMsg(
		"Not-active-project-update-id", // projectUpdateID not matching current
		0.0,  // EstimatedTimeCompleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.ComplianceInspecReportProducerID)

	err = manager.ProcessStatusEvent(complianceStatusEvent)
	assert.NoError(t, err)

	assert.Equal(t, config.RunningState, manager.State())
}

// the compliance status is estimated to be longer so its percentage will be used.
func TestProjectUpdateManagerPercentageComplete(t *testing.T) {
	var lastestPublishedEvent *automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastestPublishedEvent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)
	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)
	assert.Equal(t, config.NotRunningState, manager.State())

	err = manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, config.RunningState, manager.State())

	eventData := lastestPublishedEvent.Data

	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		1554844823.0, // This update is estimated to be longer than infra
		0.8,          // percentageComplete
		false,        // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusEvent(infraStatusEvent)

	complianceStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		1554845823.0, // This update is estimated to be longer than infra
		0.4,          // percentageComplete
		false,        // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusEvent(complianceStatusEvent)

	assert.InDelta(t, 0.4, manager.PercentageComplete(), 0.001)
	assert.Equal(t, time.Unix(1554845823, 0), manager.EstimatedTimeComplete())
}

func TestProjectUpdateManagerPercentageCompleteAllComplete(t *testing.T) {
	var lastestPublishedEvent *automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastestPublishedEvent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)
	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)
	assert.Equal(t, config.NotRunningState, manager.State())

	err = manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, config.RunningState, manager.State())

	eventData := lastestPublishedEvent.Data

	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		1554844823.0, // This update is estimated to be longer than infra
		0.8,          // percentageComplete
		true,         // completed
		event_ids.InfraClientRunsProducerID)

	manager.ProcessStatusEvent(infraStatusEvent)

	complianceStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		1554845823.0, // This update is estimated to be longer than infra
		0.4,          // percentageComplete
		true,         // completed
		event_ids.ComplianceInspecReportProducerID)

	manager.ProcessStatusEvent(complianceStatusEvent)

	assert.InDelta(t, 1.0, manager.PercentageComplete(), 0.001)
	assert.Equal(t, time.Time{}, manager.EstimatedTimeComplete())
}

func TestProjectUpdateManagerFailureMessagesOldUpdate(t *testing.T) {
	var lastestPublishedEvent *automate_event.EventMsg
	mockEventServiceClient := automate_event.NewMockEventServiceClient(gomock.NewController(t))
	mockEventServiceClient.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(cxt interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastestPublishedEvent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	configFile := "/tmp/.authz-delete-me"
	err := os.Remove(configFile)
	configMgr, err := config.NewManager(configFile)
	require.NoError(t, err)
	defer os.Remove(configFile)
	manager := v2.NewProjectUpdateManager(mockEventServiceClient, configMgr)
	assert.Equal(t, config.NotRunningState, manager.State())

	err = manager.Start()
	assert.NoError(t, err)
	assert.Equal(t, config.RunningState, manager.State())

	eventData := lastestPublishedEvent.Data
	projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

	infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
		0.0,  // EstimatedTimeCompleteInSec
		1.0,  // percentageComplete
		true, // completed
		event_ids.InfraClientRunsProducerID)

	err = manager.ProcessStatusEvent(infraStatusEvent)
	assert.NoError(t, err)

	complianceStatusEvent := createFailureEventMsg(
		"Not-active-project-update-id", // projectUpdateID not matching current
		event_ids.ComplianceInspecReportProducerID)

	// Get an error when processing the message.
	err = manager.ProcessFailEvent(complianceStatusEvent)
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
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: projectUpdateIDTag,
					},
				},
			},
		},
	}
}

func waitFor(f func() bool) {
	period := time.Millisecond * 10

	for {
		if f() {
			break
		}

		time.Sleep(period)
	}
}
