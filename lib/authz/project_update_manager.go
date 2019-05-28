package authz

import (
	"context"
	"fmt"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	automate_event_type "github.com/chef/automate/components/event-service/server"
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

type ProjectUpdateConfig struct {
	State           string   `toml:"state"`
	ProjectUpdateID string   `toml:"project_update_id"`
	EsJobIDs        []string `toml:"es_job_ids"`
}

type EsClient interface {
	JobCancel(context.Context, string) error
	UpdateProjectTags(context.Context, map[string]*iam_v2.ProjectRules) ([]string, error)
	JobStatus(context.Context, string) (JobStatus, error)
}

type ConfigManager interface {
	GetProjectUpdateConfig() ProjectUpdateConfig
	UpdateProjectUpdateConfig(ProjectUpdateConfig) error
}

const (
	RunningState    = "running"
	NotRunningState = "not_running"
)

// Manager - project update manager
type DomainProjectUpdateManager struct {
	// stage should only be updated by using the 'updateStage' function
	stage                 stage
	percentageComplete    float32
	estimatedEndTimeInSec int64
	esClient              EsClient
	authzProjectsClient   iam_v2.ProjectsClient
	eventServiceClient    automate_event.EventServiceClient
	configManager         ConfigManager
	updateQueue           chan<- stageUpdate
	producer              string
}

// NewDomainProjectUpdateManager - create a new domain project update manager
func NewDomainProjectUpdateManager(esClient EsClient, authzProjectsClient iam_v2.ProjectsClient,
	eventServiceClient automate_event.EventServiceClient, configManager ConfigManager,
	producer string) *DomainProjectUpdateManager {

	updateQueue := make(chan stageUpdate, 100)
	manager := &DomainProjectUpdateManager{
		stage: stage{
			state: NotRunningState,
		},
		esClient:            esClient,
		authzProjectsClient: authzProjectsClient,
		eventServiceClient:  eventServiceClient,
		configManager:       configManager,
		updateQueue:         updateQueue,
		producer:            producer,
	}

	// Single goroutine that updates the stage data
	go manager.stageUpdater(updateQueue)

	manager.resumePreviousState()
	return manager
}

// Cancel - stop any running job
// Action
func (manager *DomainProjectUpdateManager) Cancel(projectUpdateID string) {
	err := manager.updateStage(func(stage stage) (stage, error) {
		switch manager.stage.state {
		case NotRunningState:
			// do nothing job is not running
		case RunningState:
			if manager.stage.projectUpdateID != projectUpdateID {
				// do nothing because the requested project update job is not running
				return stage, nil
			}
			logrus.Debugf("Cancelling project tag update for ID %q elasticsearch task ID %v",
				manager.stage.projectUpdateID, manager.stage.esJobIDs)
			for _, esJobID := range manager.stage.esJobIDs {
				err := manager.esClient.JobCancel(context.Background(), esJobID)
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

// Start - start a project update
// Action
func (manager *DomainProjectUpdateManager) Start(projectUpdateID string) {
	err := manager.updateStage(func(stage stage) (stage, error) {
		switch stage.state {
		case NotRunningState:
			// TODO store and run through past projectUpdateIDs to check for a match
			if stage.projectUpdateID == projectUpdateID {
				// Update has already completed with this project update ID
				// Send a complete status
				status := JobStatus{
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
				stage.state = RunningState
			}
		case RunningState:
			if stage.projectUpdateID == projectUpdateID {
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
func (manager *DomainProjectUpdateManager) PercentageComplete() float32 {
	switch manager.stage.state {
	case NotRunningState:
	case RunningState:
		return manager.percentageComplete
	default:
	}

	return 1.0
}

// EstimatedTimeComplete - the estimated date and time of completion.
func (manager *DomainProjectUpdateManager) EstimatedTimeComplete() time.Time {
	switch manager.stage.state {
	case NotRunningState:
	case RunningState:
		return time.Unix(manager.estimatedEndTimeInSec, 0)
	default:
	}

	return time.Time{}
}

// State - The current state of the manager
func (manager *DomainProjectUpdateManager) State() string {
	return manager.stage.state
}

// This is grabbing the latest project rules from the authz-service and kicking off the update
// process in elasticsearch.
func (manager *DomainProjectUpdateManager) startProjectTagUpdater() ([]string, error) {
	logrus.Debug("starting project updater")
	ctx := context.Background()

	projectCollectionRulesResp, err := manager.authzProjectsClient.ListProjectRules(ctx,
		&iam_v2.ListProjectRulesReq{})
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to get authz project rules")
	}

	esJobIDs, err := manager.esClient.UpdateProjectTags(ctx, projectCollectionRulesResp.ProjectRules)
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to start Elasticsearch Node project tags update")
	}

	logrus.Debugf("Started Project rule update with job IDs: %v", esJobIDs)

	return esJobIDs, nil
}

func (manager *DomainProjectUpdateManager) waitingForJobToComplete() {
	numberOfConsecutiveFails := 0
	manager.updateStatus(JobStatus{}, manager.stage.projectUpdateID)
	for manager.stage.state == RunningState {
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

func (manager *DomainProjectUpdateManager) checkJobStatus() error {
	jobStatuses, err := manager.collectJobStatus()
	if err != nil {
		return errors.Errorf("Failed to check the running job: %v", err)
	}

	mergedJobStatus := FindSlowestJobStatus(jobStatuses)
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

func (manager *DomainProjectUpdateManager) collectJobStatus() ([]JobStatus, error) {
	jobStatuses := make([]JobStatus, len(manager.stage.esJobIDs))
	for index, esJobID := range manager.stage.esJobIDs {
		jobStatus, err := manager.esClient.JobStatus(context.Background(), esJobID)
		if err != nil {
			return jobStatuses, err
		}

		jobStatuses[index] = jobStatus
	}

	return jobStatuses, nil
}

func (manager *DomainProjectUpdateManager) failedJob(errMsg string) {
	err := manager.updateStage(func(stage stage) (stage, error) {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		manager.sendFailedEvent(fmt.Sprintf("Failed to check Elasticsearch jobs %v %d times; error message %q",
			manager.stage.esJobIDs, maxNumberOfConsecutiveFails, errMsg), stage.projectUpdateID)

		stage.state = NotRunningState

		return stage, nil
	})

	if err != nil {
		logrus.Errorf("Sending Failed %v", err)
		manager.sendFailedEvent(err.Error(), manager.stage.projectUpdateID)
	}
}

func (manager *DomainProjectUpdateManager) completeJob() error {
	return manager.updateStage(func(stage stage) (stage, error) {
		manager.percentageComplete = 1.0
		manager.estimatedEndTimeInSec = 0
		stage.state = NotRunningState

		return stage, nil
	})
}

func (manager *DomainProjectUpdateManager) resumePreviousState() {
	projectUpdateConfig := manager.configManager.GetProjectUpdateConfig()

	manager.stage.projectUpdateID = projectUpdateConfig.ProjectUpdateID
	manager.stage.esJobIDs = projectUpdateConfig.EsJobIDs

	if projectUpdateConfig.State == RunningState {
		manager.stage.state = RunningState
		go manager.waitingForJobToComplete()
	}
}

func (manager *DomainProjectUpdateManager) saveState(stage stage) error {
	projectUpdateConfig := ProjectUpdateConfig{
		State:           stage.state,
		ProjectUpdateID: stage.projectUpdateID,
		EsJobIDs:        stage.esJobIDs,
	}

	return manager.configManager.UpdateProjectUpdateConfig(projectUpdateConfig)
}

func (manager *DomainProjectUpdateManager) stageUpdater(updateQueue <-chan stageUpdate) {
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
func (manager *DomainProjectUpdateManager) stageUpdateEvent(oldStage stage, newStage stage) {
	if oldStage.state == NotRunningState && newStage.state == RunningState {
		go manager.waitingForJobToComplete()
	}
}

func (manager *DomainProjectUpdateManager) updateStage(updateFunc func(stage) (stage, error)) error {
	errc := make(chan error)
	manager.updateQueue <- stageUpdate{fun: updateFunc, errc: errc}

	// Wait for the function to run
	err := <-errc
	close(errc)
	return err
}

// publish a project update failed event
func (manager *DomainProjectUpdateManager) sendFailedEvent(msg string, projectUpdateID string) {
	logrus.Infof("Sending Failed Event message: %s", msg)
	event := &automate_event.EventMsg{
		EventID:   createEventUUID(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateFailed},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: manager.producer,
		},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				ProjectUpdateIDTag: {
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
		logrus.Warnf("Publishing Failed event %v", err)
	}
}

// publish a project update status event
func (manager *DomainProjectUpdateManager) updateStatus(jobStatus JobStatus, projectUpdateID string) {
	manager.estimatedEndTimeInSec = jobStatus.EstimatedEndTimeInSec
	manager.percentageComplete = jobStatus.PercentageComplete

	event := &automate_event.EventMsg{
		EventID:   createEventUUID(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdateStatus},
		Published: ptypes.TimestampNow(),
		Producer: &automate_event.Producer{
			ID: manager.producer,
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
				"EstimatedTimeCompleteInSec": {
					Kind: &_struct.Value_NumberValue{
						NumberValue: float64(jobStatus.EstimatedEndTimeInSec),
					},
				},
				ProjectUpdateIDTag: {
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
