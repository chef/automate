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
	sleepTimeBetweenStatusChecksMilliSec = 1000
	maxNumberOfConsecutiveFails          = 10
)

type stage struct {
	state           string
	projectUpdateID string
	esJobIDs        []string
}

type stageUpdate struct {
	fun  func(stage) (stage, error)
	errc chan error
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
	updateQueue           chan<- stageUpdate
}

// NewManager - create a new project update manager
func NewManager(client *ingestic.ESClient, authzProjectsClient iam_v2.ProjectsClient,
	eventServiceClient automate_event.EventServiceClient, configManager *config.ConfigManager) *Manager {
	updateQueue := make(chan stageUpdate, 100)
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
	err := manager.updateStage(func(stage stage) (stage, error) {
		switch stage.state {
		case config.NotRunningState:
			// do nothing job is not running
		case config.RunningState:
			if manager.stage.projectUpdateID != projectUpdateID {
				return stage, nil
			}
			logrus.Debugf("Cancelling project tag update for ID %q elasticsearch task ID %v",
				manager.stage.projectUpdateID, manager.stage.esJobIDs)

			for _, esJobID := range manager.stage.esJobIDs {
				err := manager.client.JobCancel(context.Background(), esJobID)
				if err != nil {
					return stage, errors.Errorf("Failed to cancel Elasticsearch task. "+
						" Elasticsearch Task ID %q; projectUpdateID: %q", esJobID, projectUpdateID)
				}
			}
		default:
			// error state not found
			return stage, errors.Errorf(
				"Internal error state %q eventID %q", stage.state, projectUpdateID)
		}
		return stage, nil
	})

	if err != nil {
		logrus.Errorf("Canceling %v", err)
		manager.sendFailedEvent(err.Error(), projectUpdateID)
	}
}

// Start - start a project tags update
// Action
func (manager *Manager) Start(projectUpdateID string) {
	err := manager.updateStage(func(stage stage) (stage, error) {
		switch stage.state {
		case config.NotRunningState:
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
					return stage, errors.Errorf(
						"Failed to start Elasticsearch Project rule update job projectUpdateID: %q", projectUpdateID)
				}
				stage.esJobIDs = esJobIDs
				stage.projectUpdateID = projectUpdateID
				stage.state = config.RunningState
			}
		case config.RunningState:
			if manager.stage.projectUpdateID == projectUpdateID {
				// The update job has ready started. Send an status update
				err := manager.checkJobStatus()
				if err != nil {
					return stage, err
				}
			} else {
				// send a failure event for the new project ID requested
				return stage, errors.Errorf(
					"Can not start another project update, project update %q is running",
					stage.projectUpdateID)
			}
		default:
			// error state not found
			return stage, errors.Errorf(
				"Internal error state %q eventID %q", stage.state, projectUpdateID)
		}
		return stage, nil
	})

	if err != nil {
		logrus.Errorf("Start %v", err)
		manager.sendFailedEvent(err.Error(), projectUpdateID)
	}
}

// PercentageComplete - percentage of the job complete
// Read
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
// Read
func (manager *Manager) EstimatedTimeCompelete() time.Time {
	switch manager.stage.state {
	case config.NotRunningState:
	case config.RunningState:
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
	manager.updateStatus(ingestic.JobStatus{}, manager.stage.projectUpdateID)
	for manager.stage.state == config.RunningState {
		time.Sleep(time.Millisecond * sleepTimeBetweenStatusChecksMilliSec)
		err := manager.checkJobStatus()
		if err != nil {
			logrus.Errorf("Failed to check the running job: %v", err)
			numberOfConsecutiveFails++
			if numberOfConsecutiveFails > maxNumberOfConsecutiveFails {
				manager.failedJob(err.Error())
				break
			}
		} else {
			numberOfConsecutiveFails = 0
		}
	}
}

func (manager *Manager) checkJobStatus() error {
	jobStatuses, err := manager.collectJobStatus()
	if err != nil {
		return errors.Errorf("Failed to check the running job: %v", err)
	}

	mergedJobStatus := MergeJobStatus(jobStatuses)
	manager.updateStatus(mergedJobStatus, manager.stage.projectUpdateID)

	if mergedJobStatus.Completed {
		err := manager.completeJob()
		if err != nil {
			logrus.Errorf("Update Job Complete %v", err)
			manager.sendFailedEvent(err.Error(), manager.stage.projectUpdateID)
		}
	}

	return nil
}

func (manager *Manager) failedJob(mgs string) {
	err := manager.updateStage(func(stage stage) (stage, error) {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		manager.sendFailedEvent(fmt.Sprintf("Failed to check Elasticsearch job %q %d times; error message %q",
			manager.stage.esJobIDs, maxNumberOfConsecutiveFails, mgs), manager.stage.projectUpdateID)

		stage.state = config.NotRunningState

		return stage, nil
	})

	if err != nil {
		logrus.Errorf("Sending Failed %v", err)
		manager.sendFailedEvent(err.Error(), manager.stage.projectUpdateID)
	}
}

func (manager *Manager) completeJob() error {
	return manager.updateStage(func(stage stage) (stage, error) {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		stage.state = config.NotRunningState

		return stage, nil
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

// publish a project update failed event
func (manager *Manager) sendFailedEvent(msg string, projectUpdateID string) {
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
						StringValue: projectUpdateID,
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

	if projectUpdateConfig.State == config.RunningState {
		manager.stage.state = config.RunningState
		logrus.Infof("Setting manager.state: %s manager.projectUpdateID: %s manager.esJobID: %s",
			manager.stage.state, manager.stage.projectUpdateID, manager.stage.esJobIDs)
		go manager.waitingForJobToComplete()
	}
}

func (manager *Manager) saveState(stage stage) error {
	projectUpdateConfig := config.ProjectUpdateConfig{
		State:           stage.state,
		ProjectUpdateID: stage.projectUpdateID,
		EsJobIDs:        stage.esJobIDs,
	}

	return manager.configManager.UpdateProjectUpdateConfig(projectUpdateConfig)
}

func (manager *Manager) stageUpdater(updateQueue <-chan stageUpdate) {
	for stageUpdate := range updateQueue {
		updatedStage, err := stageUpdate.fun(manager.stage.copy())
		if err != nil {
			stageUpdate.errc <- err
			continue
		}

		oldStage := manager.stage.copy()
		if !updatedStage.equal(oldStage) {
			err = manager.saveState(updatedStage)
			if err != nil {
				logrus.Errorf("Failure updating project update state %v", err)
				stageUpdate.errc <- err
				continue
			}

			manager.stage = updatedStage
			manager.stageUpdateEvent(oldStage, updatedStage)
		}

		stageUpdate.errc <- nil
	}
}

// When the stage state is updated to running start the waitingForJobToComplete function
func (manager *Manager) stageUpdateEvent(oldStage stage, newStage stage) {
	if oldStage.state == config.NotRunningState && newStage.state == config.RunningState {
		go manager.waitingForJobToComplete()
	}
}

func (manager *Manager) updateStage(updateFunc func(stage) (stage, error)) error {
	errc := make(chan error)
	manager.updateQueue <- stageUpdate{fun: updateFunc, errc: errc}

	// Wait for the function to run
	err := <-errc
	close(errc)
	return err
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
