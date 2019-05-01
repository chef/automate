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
	// stage should only be updated by using the 'updateStage' function
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
			state: config.NotRunningState,
		},
		client:              client,
		authzProjectsClient: authzProjectsClient,
		eventServiceClient:  eventServiceClient,
		configManager:       configManager,
		updateQueue:         updateQueue,
	}

	// Single goroutine that updates the stage data
	go manager.stageUpdater(updateQueue)

	manager.resumePreviousState()
	return manager
}

// Cancel - stop any running job
// Action
func (manager *Manager) Cancel(projectUpdateID string) {
	manager.updateStage(func(stage stage) stage {
		switch manager.stage.state {
		case config.NotRunningState:
			// do nothing job is not running
		case config.RunningState:
			if manager.stage.projectUpdateID != projectUpdateID {
				// do nothing because the requested project update job is not running
				return stage
			}
			logrus.Debugf("Cancelling project tag update for ID %q elasticsearch task ID %q",
				manager.stage.projectUpdateID, manager.stage.esJobID)
			manager.client.JobCancel(context.Background(), manager.stage.esJobID)
		default:
			// error state not found
			manager.sendFailedEvent(fmt.Sprintf(
				"Internal error state %q eventID %q", manager.stage.state, projectUpdateID),
				projectUpdateID)
		}

		return stage
	})
}

// Start - start a project update
// Action
func (manager *Manager) Start(projectUpdateID string) {
	manager.updateStage(func(stage stage) stage {
		switch stage.state {
		case config.NotRunningState:
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
						"Failed to start Elasticsearch Project rule update job projectUpdateID: %q", projectUpdateID),
						projectUpdateID)
				} else {
					stage.esJobID = esJobID
					stage.projectUpdateID = projectUpdateID
					stage.state = config.RunningState
					go manager.waitingForJobToComplete()
				}
			}
		case config.RunningState:
			if stage.projectUpdateID == projectUpdateID {
				//	Do nothing. The job has ready started
			} else {
				manager.sendFailedEvent(fmt.Sprintf(
					"Can not start another project update %q is running", stage.projectUpdateID),
					projectUpdateID)
			}
		default:
			// error state not found
			manager.sendFailedEvent(fmt.Sprintf(
				"Internal error state %q eventID %q", stage.state, projectUpdateID),
				projectUpdateID)
		}
		return stage
	})
}

// PercentageComplete - percentage of the job complete
func (manager *Manager) PercentageComplete() float32 {
	switch manager.stage.state {
	case config.NotRunningState:
	case config.RunningState:
		return manager.percentageComplete
	default:
	}

	return 1.0
}

// EstimatedTimeCompelete - the estimated date and time of compeletion.
func (manager *Manager) EstimatedTimeCompelete() time.Time {
	switch manager.stage.state {
	case config.NotRunningState:
	case config.RunningState:
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

	if projectUpdateConfig.State == config.RunningState {
		manager.stage.state = config.RunningState
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
	manager.updateStage(func(stage stage) stage {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		manager.sendFailedEvent(fmt.Sprintf("Failed to check Elasticsearch job %q %d times; error message %q",
			manager.stage.esJobID, maxNumberOfConsecutiveFails, errMsg), stage.projectUpdateID)

		stage.state = config.NotRunningState

		return stage
	})
}

func (manager *Manager) completeJob() {
	manager.updateStage(func(stage stage) stage {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		stage.state = config.NotRunningState

		return stage
	})
}

// publish a project update failed event
func (manager *Manager) sendFailedEvent(msg string, projectUpdateID string) {
	logrus.Infof("Sending Failed Event message: %s", msg)
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

func (manager *Manager) stageUpdater(updateQueue <-chan func(stage) stage) {
	for update := range updateQueue {
		stage := update(manager.stage)

		if !stage.equal(manager.stage) {
			manager.saveState(stage)
			manager.stage = stage
		}
	}
}

func (manager *Manager) updateStage(updateFunc func(stage) stage) {
	manager.updateQueue <- updateFunc
}

func (stage1 stage) equal(stage2 stage) bool {
	if stage1.state != stage2.state {
		return false
	}

	if stage1.projectUpdateID != stage2.projectUpdateID {
		return false
	}

	if stage1.esJobID != stage2.esJobID {
		return false
	}

	return true
}
