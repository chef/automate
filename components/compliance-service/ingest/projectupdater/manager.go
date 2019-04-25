package projectupdater

import (
	"context"
	"fmt"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	project_update_tags "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
	_struct "github.com/golang/protobuf/ptypes/struct"
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
	esJobIDs        []string
}

// Manager - project update manager
type Manager struct {
	stage                 stage
	percentageComplete    float32
	estimatedEndTimeInSec int64
	client                *ingestic.ESClient
	authzProjectsClient   iam_v2.ProjectsClient
	eventServiceClient    automate_event.EventServiceClient
	configManager         *config.ConfigManager
	updateQueue           chan<- func(stage) stage
}

// NewManager - create a new project update manager
func NewManager(client *ingestic.ESClient, authzProjectsClient iam_v2.ProjectsClient,
	eventServiceClient automate_event.EventServiceClient, configManager *config.ConfigManager) *Manager {
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
	go manager.stageUpdater(updateQueue)

	manager.resumePreviousState()
	return manager
}

// Cancel - stop any running job
// Action
func (manager *Manager) Cancel(projectUpdateID string) {
	manager.updateStage(func(stage stage) stage {
		switch stage.state {
		case notRunningState:
			// do nothing job is not running
		case runningState:
			if manager.stage.projectUpdateID == projectUpdateID {
				logrus.Debugf("Cancelling project tag update for ID %q elasticsearch task ID %v",
					manager.stage.projectUpdateID, manager.stage.esJobIDs)

				for _, esJobID := range manager.stage.esJobIDs {
					err := manager.client.JobCancel(context.Background(), esJobID)
					if err != nil {
						logrus.Errorf("Failed to cancel Elasticsearch task. "+
							" Elasticsearch Task ID %q; projectUpdateID: %q", esJobID, projectUpdateID)
					}
				}
			} else {
				// do nothing because the requested project update job is not running
			}
		default:
			// error state not found
			manager.sendFailedEvent(fmt.Sprintf(
				"Internal error state %q eventID %q", stage.state, stage.projectUpdateID))
		}
		return stage
	})
}

// Start - start a project tags update
// Action
func (manager *Manager) Start(projectUpdateID string) {
	manager.updateStage(func(stage stage) stage {
		switch stage.state {
		case notRunningState:
			// TODO store and run through past projectUpdateIDs to check for a match
			if manager.stage.projectUpdateID == projectUpdateID {
				// Update has already completed with this project update ID
				// Send a complete status
				status := ingestic.JobStatus{
					Completed:          true,
					PercentageComplete: 1.0,
				}
				manager.updateStatus(status, projectUpdateID)
			} else {
				// Start elasticsearch update job async and return the job ID
				esJobIDs, err := manager.startProjectTagUpdater()
				if err != nil {
					logrus.Errorf("Failed to start Elasticsearch Project rule update job projectUpdateID: %q",
						projectUpdateID)
					manager.sendFailedEvent(fmt.Sprintf(
						"Failed to start Elasticsearch Project rule update job projectUpdateID: %q", projectUpdateID))
				} else {
					stage.esJobIDs = esJobIDs
					stage.projectUpdateID = projectUpdateID
					stage.state = runningState
					go manager.waitingForJobToComplete()
				}
			}
		case runningState:
			if manager.stage.projectUpdateID == projectUpdateID {
				//  Do nothing. The job has ready started
			} else {
				manager.sendFailedEvent(fmt.Sprintf(
					"Can not start another project update %q is running", manager.stage.projectUpdateID))
			}
		default:
			// error state not found
			manager.sendFailedEvent(fmt.Sprintf(
				"Internal error state %q eventID %q", stage.state, stage.projectUpdateID))
		}
		return stage
	})
}

// PercentageComplete - percentage of the job complete
// Read
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
// Read
func (manager *Manager) EstimatedTimeCompelete() time.Time {
	switch manager.stage.state {
	case notRunningState:
	case runningState:
		return time.Unix(manager.estimatedEndTimeInSec, 0)
	default:
	}

	return time.Time{}
}

// State - The current state of the manager
// Read
func (manager *Manager) State() string {
	return manager.stage.state
}

// This is grabbing the latest project rules from the authz-service and kicking off the update
// process in elasticsearch.
func (manager *Manager) startProjectTagUpdater() ([]string, error) {
	logrus.Debug("starting project updater")
	ctx := context.Background()

	projectCollectionRulesResp, err := manager.authzProjectsClient.ListProjectRules(ctx,
		&iam_v2.ListProjectRulesReq{})
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to get authz project rules")
	}

	esReportJobID, err := manager.client.UpdateReportProjectsTags(ctx,
		projectCollectionRulesResp.ProjectRules)
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to start Elasticsearch Node project tags update")
	}

	esSummaryJobID, err := manager.client.UpdateSummaryProjectsTags(ctx,
		projectCollectionRulesResp.ProjectRules)
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to start Elasticsearch Node project tags update")
	}

	logrus.Debugf("Started Project rule updates with report job ID: %q and summary job ID %q",
		esReportJobID, esSummaryJobID)

	return []string{esReportJobID, esSummaryJobID}, nil
}

func (manager *Manager) waitingForJobToComplete() {
	numberOfConsecutiveFails := 0
	mergedJobStatus := ingestic.JobStatus{
		Completed: false,
	}

	// initial status
	manager.updateStatus(ingestic.JobStatus{}, manager.stage.projectUpdateID)

	jobStatuses, err := manager.collectJobStatus()
	if err != nil {
		logrus.Errorf("Failed to check the running job: %v", err)
		numberOfConsecutiveFails++
	} else {
		mergedJobStatus = MergeJobStatus(jobStatuses)
		manager.updateStatus(mergedJobStatus, manager.stage.projectUpdateID)
	}

	for !mergedJobStatus.Completed {
		time.Sleep(time.Millisecond * sleepTimeBetweenStatusChecksMilliSec)
		jobStatuses, err = manager.collectJobStatus()
		if err != nil {
			logrus.Errorf("Failed to check the running job: %v", err)
			numberOfConsecutiveFails++
			if numberOfConsecutiveFails > maxNumberOfConsecutiveFails {
				manager.failedJob(err.Error())
				return
			}
		} else {
			mergedJobStatus = MergeJobStatus(jobStatuses)
			manager.updateStatus(mergedJobStatus, manager.stage.projectUpdateID)
			numberOfConsecutiveFails = 0
		}
	}

	logrus.Debugf("Finished Project rule update with Elasticsearch job IDs: %v and projectUpdate ID %q",
		manager.stage.esJobIDs, manager.stage.projectUpdateID)

	manager.completeJob()
}

func (manager *Manager) failedJob(mgs string) {
	manager.updateStage(func(stage stage) stage {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		manager.sendFailedEvent(fmt.Sprintf("Failed to check Elasticsearch job %q %d times; error message %q",
			manager.stage.esJobIDs, maxNumberOfConsecutiveFails, mgs))

		stage.state = notRunningState

		return stage
	})
}

func (manager *Manager) completeJob() {
	manager.updateStage(func(stage stage) stage {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		stage.state = notRunningState

		return stage
	})
}

func (manager *Manager) collectJobStatus() ([]ingestic.JobStatus, error) {
	jobStatuses := make([]ingestic.JobStatus, len(manager.stage.esJobIDs))
	for index, esJobID := range manager.stage.esJobIDs {
		jobStatus, err := manager.client.JobStatus(context.Background(), esJobID)
		if err != nil {
			return jobStatuses, err
		}

		jobStatuses[index] = jobStatus
	}

	return jobStatuses, nil
}

// MergeJobStatus - combine multiple jobStatus objects into one
func MergeJobStatus(jobStatuses []ingestic.JobStatus) ingestic.JobStatus {
	combinedJobStatus := ingestic.JobStatus{
		Completed:          true,
		PercentageComplete: 1.0,
	}
	for _, jobStatus := range jobStatuses {
		if !jobStatus.Completed {
			combinedJobStatus.Completed = false
			if jobStatus.EstimatedEndTimeInSec > combinedJobStatus.EstimatedEndTimeInSec {
				combinedJobStatus.EstimatedEndTimeInSec = jobStatus.EstimatedEndTimeInSec
				combinedJobStatus.PercentageComplete = jobStatus.PercentageComplete
			}
		}
	}

	return combinedJobStatus
}

// publish a project update failed event
func (manager *Manager) sendFailedEvent(msg string) {
	event := &automate_event.EventMsg{
		EventID:   createEventUUID(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateFailed},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: event_ids.ComplianceInspecReportProducerID,
		},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: manager.stage.projectUpdateID,
					},
				},
				"message": {
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
		logrus.Warnf("Publishing status event %v", err)
	}
}

// publish a project update status event
func (manager *Manager) updateStatus(jobStatus ingestic.JobStatus, projectUpdateID string) {
	manager.estimatedEndTimeInSec = jobStatus.EstimatedEndTimeInSec
	manager.percentageComplete = jobStatus.PercentageComplete

	event := &automate_event.EventMsg{
		EventID:   createEventUUID(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateStatus},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: event_ids.ComplianceInspecReportProducerID,
		},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				"Completed": {
					Kind: &_struct.Value_BoolValue{
						BoolValue: jobStatus.Completed,
					},
				},
				"PercentageComplete": {
					Kind: &_struct.Value_NumberValue{
						NumberValue: float64(jobStatus.PercentageComplete),
					},
				},
				"EstimatedTimeCompeleteInSec": {
					Kind: &_struct.Value_NumberValue{
						NumberValue: float64(jobStatus.EstimatedEndTimeInSec),
					},
				},
				project_update_tags.ProjectUpdateIDTag: {
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
		logrus.Warnf("Publishing Failed event %v", err)
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

func (manager *Manager) resumePreviousState() {
	projectUpdateConfig := manager.configManager.GetProjectUpdateConfig()

	manager.stage.projectUpdateID = projectUpdateConfig.ProjectUpdateID
	manager.stage.esJobIDs = projectUpdateConfig.EsJobIDs

	if projectUpdateConfig.State == runningState {
		manager.stage.state = runningState
		logrus.Infof("Setting manager.state: %s manager.projectUpdateID: %s manager.esJobID: %s",
			manager.stage.state, manager.stage.projectUpdateID, manager.stage.esJobIDs)
		go manager.waitingForJobToComplete()
	}
}

func (manager *Manager) saveState(stage stage) {
	projectUpdateConfig := config.ProjectUpdateConfig{
		State:           stage.state,
		ProjectUpdateID: stage.projectUpdateID,
		EsJobIDs:        stage.esJobIDs,
	}

	err := manager.configManager.UpdateProjectUpdateConfig(projectUpdateConfig)
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"error": err,
		}).Error("Update Project Update Config")
	}
}

func (manager *Manager) stageUpdater(updateQueue <-chan func(stage) stage) {
	for update := range updateQueue {
		stage := update(manager.stage.copy())

		if !stage.equal(manager.stage) {
			manager.saveState(stage)
			manager.stage = stage
		}
	}
}

func (manager *Manager) updateStage(updateFunc func(stage) stage) {
	manager.updateQueue <- updateFunc
}

func (s stage) copy() stage {
	esJobIDs := make([]string, len(s.esJobIDs))

	for index, esJobID := range s.esJobIDs {
		esJobIDs[index] = esJobID
	}
	return stage{
		state:           s.state,
		projectUpdateID: s.projectUpdateID,
		esJobIDs:        esJobIDs,
	}
}

func (s stage) equal(stage2 stage) bool {
	if s.state != stage2.state {
		return false
	}

	if s.projectUpdateID != stage2.projectUpdateID {
		return false
	}

	if len(s.esJobIDs) != len(stage2.esJobIDs) {
		return false
	}
	for index, esJobID1 := range s.esJobIDs {
		if esJobID1 != stage2.esJobIDs[index] {
			return false
		}
	}

	return true
}
