package projectupdater

import (
	"context"
	"fmt"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/config"
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

type stage struct {
	state           string
	projectUpdateID string
	esJobID         string
}

// Manager - project update manager
type Manager struct {
	// stage should only be updated by using the 'send' function
	stage                 stage
	percentageComplete    float32
	estimatedEndTimeInSec int64
	client                backend.Client
	authzProjectsClient   iam_v2.ProjectsClient
	eventServiceClient    automate_event.EventServiceClient
	configManager         *config.Manager
	updateQueue           chan<- func(stage) stage
}

// NewManager - create a new project update manager
func NewManager(client backend.Client, authzProjectsClient iam_v2.ProjectsClient,
	eventServiceClient automate_event.EventServiceClient, configManager *config.Manager) *Manager {

	updateQueue := make(chan func(stage) stage, 100)
	manager := &Manager{
		stage: stage{
			state: notRunningState,
		},
		client:              client,
		authzProjectsClient: authzProjectsClient,
		eventServiceClient:  eventServiceClient,
		configManager:       configManager,
		updateQueue:         updateQueue,
	}

	// Single goroutine that updates the stage data
	go func() {
		for update := range updateQueue {
			stage := update(manager.stage)

			manager.stage = stage
		}
	}()

	manager.resumePreviousState()
	return manager
}

// Cancel - stop any running job
func (manager *Manager) Cancel(projectUpdateID string) {
	updateFunc := func(stage stage) stage {
		switch stage.state {
		case notRunningState:
			// do nothing job is not running
		case runningState:
			if stage.projectUpdateID == projectUpdateID {
				logrus.Debugf("Cancelling project tag update for ID %q elasticsearch task ID %q",
					stage.projectUpdateID, stage.esJobID)
				manager.client.JobCancel(context.Background(), stage.esJobID)
			} else {
				// do nothing because the requested project update job is not running
			}
		default:
			// error state not found
			manager.sendFailedEvent(fmt.Sprintf(
				"Internal error state %q eventID %q", stage.state, stage.projectUpdateID))
		}
		return stage
	}

	manager.send(updateFunc)
}

// Start - start a project update
func (manager *Manager) Start(projectUpdateID string) {
	updateFunc := func(stage stage) stage {
		switch stage.state {
		case notRunningState:
			// TODO store and run through past projectUpdateIDs to check for a match
			if stage.projectUpdateID == projectUpdateID {
				// Update has already completed with this project update ID
				// Send a complete status
				status := backend.JobStatus{
					Completed:          true,
					PercentageComplete: 1.0,
				}
				manager.updateStatus(status, projectUpdateID)
			} else {
				// Start elasticsearch update job async and return the job ID
				esJobID, err := manager.startProjectTagUpdater()
				if err != nil {
					logrus.Errorf("Failed to start Elasticsearch Project rule update job projectUpdateID: %q",
						projectUpdateID)
					manager.sendFailedEvent(fmt.Sprintf(
						"Failed to start Elasticsearch Project rule update job projectUpdateID: %q", projectUpdateID))
				} else {
					stage.esJobID = esJobID // Store the job ID and event ID
					stage.projectUpdateID = projectUpdateID
					stage.state = runningState
					manager.saveState(stage)
					go manager.waitingForJobToComplete()
				}
			}
		case runningState:
			if stage.projectUpdateID == projectUpdateID {
				//	Do nothing. The job has ready started
			} else {
				manager.sendFailedEvent(fmt.Sprintf(
					"Can not start another project update %q is running", stage.projectUpdateID))
			}
		default:
			// error state not found
			manager.sendFailedEvent(fmt.Sprintf(
				"Internal error state %q eventID %q", stage.state, stage.projectUpdateID))
		}
		return stage
	}
	manager.send(updateFunc)
}

// PercentageComplete - percentage of the job complete
func (manager *Manager) PercentageComplete() float32 {
	switch manager.stage.state {
	case notRunningState:
	case runningState:
		return manager.percentageComplete
	default:
	}

	return 1.0
}

// EstimatedTimeCompelete - the estimated date and time of compeletion.
func (manager *Manager) EstimatedTimeCompelete() time.Time {
	switch manager.stage.state {
	case notRunningState:
	case runningState:
		return time.Unix(int64(manager.estimatedEndTimeInSec), 0)
	default:
	}

	return time.Time{}
}

// State - The current state of the manager
func (manager *Manager) State() string {
	return manager.stage.state
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

	logrus.Info("waitingForJobToComplete")

	// initial status
	manager.updateStatus(backend.JobStatus{}, manager.stage.projectUpdateID)

	jobStatus, err := manager.client.JobStatus(context.Background(), manager.stage.esJobID)
	if err != nil {
		logrus.Errorf("Failed to check the running job: %v", err)
		numberOfConsecutiveFails++
	} else {
		manager.updateStatus(jobStatus, manager.stage.projectUpdateID)
		isJobComplete = jobStatus.Completed
	}

	for !isJobComplete {
		time.Sleep(time.Millisecond * sleepTimeBetweenStatusChecksMilliSec)

		jobStatus, err = manager.client.JobStatus(context.Background(), manager.stage.esJobID)
		if err != nil {
			logrus.Errorf("Failed to check the running job: %v", err)
			numberOfConsecutiveFails++
			if numberOfConsecutiveFails > maxNumberOfConsecutiveFails {
				logrus.Errorf("Failed to check Elasticsearch job %q %d times",
					manager.stage.esJobID, numberOfConsecutiveFails)
				manager.failedJob(err.Error())
				return
			}
		} else {
			manager.updateStatus(jobStatus, manager.stage.projectUpdateID)
			numberOfConsecutiveFails = 0
			isJobComplete = jobStatus.Completed
		}
	}

	logrus.Debugf("Finished Project rule update with Elasticsearch job ID: %q and projectUpdate ID %q",
		manager.stage.esJobID, manager.stage.projectUpdateID)

	manager.completeJob()
}

func (manager *Manager) resumePreviousState() {
	projectUpdateConfig := manager.configManager.GetProjectUpdateConfig()

	manager.stage.projectUpdateID = projectUpdateConfig.ProjectUpdateID
	manager.stage.esJobID = projectUpdateConfig.EsJobID

	if projectUpdateConfig.State == runningState {
		manager.stage.state = runningState
		logrus.Infof("Setting smanager.state: %s manager.projectUpdateID: %s manager.esJobID: %s",
			manager.stage.state, manager.stage.projectUpdateID, manager.stage.esJobID)
		go manager.waitingForJobToComplete()
	}
}

func (manager *Manager) saveState(stage stage) {
	projectUpdateConfig := config.ProjectUpdateConfig{
		State:           stage.state,
		ProjectUpdateID: stage.projectUpdateID,
		EsJobID:         stage.esJobID,
	}

	err := manager.configManager.UpdateProjectUpdateConfig(projectUpdateConfig)
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"error": err,
		}).Error("Update Project Update Config")
	}
}

func (manager *Manager) failedJob(errMsg string) {
	updateFunc := func(stage stage) stage {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		manager.sendFailedEvent(fmt.Sprintf("Failed to check Elasticsearch job %q %d times; error message %q",
			manager.stage.esJobID, maxNumberOfConsecutiveFails, errMsg))

		stage.state = notRunningState
		manager.saveState(stage)

		return stage
	}
	manager.send(updateFunc)
}

func (manager *Manager) completeJob() {
	updateFunc := func(stage stage) stage {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		stage.state = notRunningState
		manager.saveState(stage)

		return stage
	}
	manager.send(updateFunc)
}

// publish a project update failed event
func (manager *Manager) sendFailedEvent(msg string) {
	logrus.Infof("Sending sendFailedEvent msg: %s", msg)
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
						StringValue: manager.stage.projectUpdateID,
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
func (manager *Manager) updateStatus(jobStatus backend.JobStatus, projectUpdateID string) {
	manager.estimatedEndTimeInSec = jobStatus.EstimatedEndTimeInSec
	manager.percentageComplete = jobStatus.PercentageComplete

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
						StringValue: projectUpdateID,
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

func (manager *Manager) send(updateFunc func(stage) stage) {
	manager.updateQueue <- updateFunc
}
