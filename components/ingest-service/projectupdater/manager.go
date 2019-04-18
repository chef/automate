package projectupdater

import (
	"context"
	"fmt"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	"github.com/chef/automate/components/ingest-service/backend"
	project_update_tags "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const (
	runningState                         = "running"
	notRunningState                      = "not_running"
	sleepTimeBetweenStatusChecksMilliSec = 1000
	maxNumberOfConsecutiveFails          = 10
)

// Manager - project update manager
type Manager struct {
	state                 string
	projectUpdateID       string
	esJobID               string
	percentageComplete    float32
	estimatedEndTimeInSec int64
	client                backend.Client
	authzProjectsClient   iam_v2.ProjectsClient
	eventServiceClient    automate_event.EventServiceClient
}

// TODO
// * Store running job IDs so if service restart it can pick up where it left off

// NewManager - create a new project update manager
func NewManager(client backend.Client, authzProjectsClient iam_v2.ProjectsClient,
	eventServiceClient automate_event.EventServiceClient) Manager {
	return Manager{
		state:               notRunningState,
		client:              client,
		authzProjectsClient: authzProjectsClient,
		eventServiceClient:  eventServiceClient,
	}
}

func (manager *Manager) Cancel(projectUpdateID string) {
	switch manager.state {
	case notRunningState:
		// do nothing job is not running
	case runningState:
		if manager.projectUpdateID == projectUpdateID {
			logrus.Debugf("Cancelling project tag update for ID %q elasticsearch task ID %q",
				manager.projectUpdateID, manager.esJobID)
			manager.client.JobCancel(context.Background(), manager.esJobID)
		} else {
			// do nothing because the requested project update job is not running
		}
	default:
		// error state not found
		manager.sendFailedEvent(fmt.Sprintf(
			"Internal error state %q eventID %q", manager.state, manager.projectUpdateID),
			projectUpdateID)
	}
}

// Start - start a project update
func (manager *Manager) Start(projectUpdateID string) {
	switch manager.state {
	case notRunningState:
		// Start elasticsearch update job async and return the job ID
		esJobID, err := manager.startProjectTagUpdater()
		if err != nil {
			logrus.Errorf("Failed to start Elasticsearch Project rule update job projectUpdateID: %q", projectUpdateID)
			manager.sendFailedEvent(fmt.Sprintf(
				"Failed to start Elasticsearch Project rule update job projectUpdateID: %q", projectUpdateID),
				projectUpdateID)
			return
		}
		manager.esJobID = esJobID // Store the job ID and event ID
		manager.projectUpdateID = projectUpdateID
		manager.state = runningState
		go manager.waitingForJobToComplete()
	case runningState:
		if manager.projectUpdateID == projectUpdateID {
			//	Do nothing. The job has ready started
		} else {
			manager.sendFailedEvent(fmt.Sprintf(
				"Can not start another project update %q is running", manager.projectUpdateID),
				projectUpdateID)
		}
	default:
		// error state not found
		manager.sendFailedEvent(fmt.Sprintf(
			"Internal error state %q eventID %q", manager.state, manager.projectUpdateID),
			projectUpdateID)
	}
}

// PercentageComplete - percentage of the job complete
func (manager *Manager) PercentageComplete() float32 {
	switch manager.state {
	case notRunningState:
	case runningState:
		return manager.percentageComplete
	default:
	}

	return 1.0
}

// EstimatedTimeCompelete - the estimated date and time of compeletion.
func (manager *Manager) EstimatedTimeCompelete() time.Time {
	switch manager.state {
	case notRunningState:
	case runningState:
		return time.Unix(int64(manager.estimatedEndTimeInSec), 0)
	default:
	}

	return time.Time{}
}

// State - The current state of the manager
func (manager *Manager) State() string {
	return manager.state
}

// This is grabbing the latest project rules from the authz-service and kicking off the update
// process in elasticsearch.
func (manager *Manager) startProjectTagUpdater() (string, error) {
	logrus.Debug("starting project updater")
	ctx := context.Background()

	projectCollectionRulesResp, err := manager.authzProjectsClient.ListProjectRules(ctx,
		&iam_v2.ListProjectRulesReq{})
	if err != nil {
		return "", errors.Wrap(err, "Failed to get authz project rules")
	}

	esJobID, err := manager.client.UpdateNodeProjectTags(ctx, projectCollectionRulesResp.ProjectRules)
	if err != nil {
		return "", errors.Wrap(err, "Failed to start Elasticsearch Node project tags update")
	}

	logrus.Debugf("Started Project rule update with job ID: %q", esJobID)

	return esJobID, nil
}

func (manager *Manager) waitingForJobToComplete() {
	var (
		isJobComplete            = false
		numberOfConsecutiveFails = 0
	)

	// initial status
	manager.sendStatusEvent(backend.JobStatus{})

	jobStatus, err := manager.client.JobStatus(context.Background(), manager.esJobID)
	if err != nil {
		logrus.Errorf("Failed to check the running job: %v", err)
		numberOfConsecutiveFails++
	} else {
		manager.sendStatusEvent(jobStatus)
		isJobComplete = jobStatus.Completed
	}

	for !isJobComplete {
		time.Sleep(time.Millisecond * sleepTimeBetweenStatusChecksMilliSec)
		jobStatus, err = manager.client.JobStatus(context.Background(), manager.esJobID)
		if err != nil {
			logrus.Errorf("Failed to check the running job: %v", err)
			numberOfConsecutiveFails++
			if numberOfConsecutiveFails > maxNumberOfConsecutiveFails {
				logrus.Errorf("Failed to check Elasticsearch job %q %d times",
					manager.esJobID, numberOfConsecutiveFails)
				manager.sendFailedEvent(fmt.Sprintf("Failed to check Elasticsearch job %q %d times",
					manager.esJobID, numberOfConsecutiveFails), manager.projectUpdateID)
				return
			}
		} else {
			manager.estimatedEndTimeInSec = jobStatus.EstimatedEndTimeInSec
			manager.percentageComplete = jobStatus.PercentageComplete
			manager.sendStatusEvent(jobStatus)
			numberOfConsecutiveFails = 0
			isJobComplete = jobStatus.Completed
		}
	}

	logrus.Debugf("Finished Project rule update with Elasticsearch job ID: %q and projectUpdate ID %q",
		manager.esJobID, manager.projectUpdateID)

	manager.state = notRunningState
}

// publish a project update failed event
func (manager *Manager) sendFailedEvent(msg string, projectUpdateID string) {
	event := &automate_event.EventMsg{
		EventID:   createEventUUID(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateFailed},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: event_ids.InfraClientRunsProducerID,
		},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: projectUpdateID,
					},
				},
				"message": &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: msg,
					},
				},
			},
		},
	}

	pubReq := &automate_event.PublishRequest{Msg: event}
	_, err := manager.eventServiceClient.Publish(context.Background(), pubReq)
	if err != nil {
		logrus.Warnf("Publishing Failed event %v", err)
	}
}

// publish a project update status event
func (manager *Manager) sendStatusEvent(jobStatus backend.JobStatus) {
	event := &automate_event.EventMsg{
		EventID:   createEventUUID(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateStatus},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: event_ids.InfraClientRunsProducerID,
		},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				"Completed": &_struct.Value{
					Kind: &_struct.Value_BoolValue{
						BoolValue: jobStatus.Completed,
					},
				},
				"PercentageComplete": &_struct.Value{
					Kind: &_struct.Value_NumberValue{
						NumberValue: float64(jobStatus.PercentageComplete),
					},
				},
				"EstimatedTimeCompeleteInSec": &_struct.Value{
					Kind: &_struct.Value_NumberValue{
						NumberValue: float64(jobStatus.EstimatedEndTimeInSec),
					},
				},
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: manager.projectUpdateID,
					},
				},
			},
		},
	}

	pubReq := &automate_event.PublishRequest{Msg: event}
	_, err := manager.eventServiceClient.Publish(context.Background(), pubReq)
	if err != nil {
		logrus.Warnf("Publishing Status event %v", err)
	}
}

func createEventUUID() string {
	uuid, err := uuid.NewV4()
	if err != nil {
		logrus.Errorf("Failed to create UUID %v", err)
		// Using a default UUID if the creation of the UUID fails
		return "39bffcd3-4325-4d18-bff5-5bd4410949ba"
	}

	return uuid.String()
}
