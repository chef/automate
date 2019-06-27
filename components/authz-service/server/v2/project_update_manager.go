package v2

import (
	"context"
	"fmt"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/authz-service/config"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	project_update_tags "github.com/chef/automate/lib/authz"
)

// Failure cases
// * No status event for 5 minutes
// * Domain service sends failure message

// Recovery
// * Domain service was down during start
// * Authz service restarts

const (
	minutesWithoutCheckingInFailure = 5
	sleepTimeBetweenStatusChecksSec = 5
)

type update struct {
	fun  func(config.ProjectUpdateStage) (config.ProjectUpdateStage, error)
	errc chan error
}

type ProjectUpdateMgr interface {
	Cancel() error
	Start() error
	ProcessFailEvent(eventMessage *automate_event.EventMsg) error
	ProcessStatusEvent(eventMessage *automate_event.EventMsg) error
	Failed() bool
	FailureMessage() string
	PercentageComplete() float64
	EstimatedTimeComplete() time.Time
	State() string
}

// ProjectUpdateManager - project update manager
type ProjectUpdateManager struct {
	stage              config.ProjectUpdateStage
	eventServiceClient automate_event.EventServiceClient
	configManager      *config.Manager
	updateQueue        chan<- update
}

// NewProjectUpdateManager - create a new project update manager
func NewProjectUpdateManager(
	eventServiceClient automate_event.EventServiceClient,
	configManager *config.Manager) *ProjectUpdateManager {
	updateQueue := make(chan update, 100)
	manager := &ProjectUpdateManager{
		stage:              configManager.GetProjectUpdateStage(),
		eventServiceClient: eventServiceClient,
		configManager:      configManager,
		updateQueue:        updateQueue,
	}

	go manager.stageUpdater(updateQueue)

	manager.resumePreviousState()

	return manager
}

// Cancel - stop or cancel domain service project update processes
// Action
func (manager *ProjectUpdateManager) Cancel() error {
	return manager.updateStage(func(stage config.ProjectUpdateStage) (config.ProjectUpdateStage, error) {
		switch stage.State {
		case config.NotRunningState:
			// do nothing job is not running
		case config.RunningState:
			logrus.Debugf("Cancelling project update for ID %q", manager.stage.ProjectUpdateID)
			err := manager.sendCancelProjectUpdateEvent()
			if err != nil {
				return stage, err
			}
		default:
			// error state not found
			return stage, errors.New(fmt.Sprintf(
				"Internal error state %q eventID %q", manager.stage.State, manager.stage.ProjectUpdateID))
		}

		return stage, nil
	})
}

// Start - start the domain services project update processes
// Action
func (manager *ProjectUpdateManager) Start() error {
	return manager.updateStage(func(stage config.ProjectUpdateStage) (config.ProjectUpdateStage, error) {
		switch stage.State {
		case config.NotRunningState:
			projectUpdateID, err := createProjectUpdateID()
			if err != nil {
				return stage, err
			}

			err = manager.sendProjectUpdateStartEvent(projectUpdateID)
			if err != nil {
				return stage, err
			}

			return stage.StartRunning(projectUpdateID), nil
		case config.RunningState:
			return stage, errors.New(fmt.Sprintf(
				"Can not start another project update %q is running", stage.ProjectUpdateID))
		default:
			// error state not found
			return stage, errors.New(fmt.Sprintf(
				"Internal error state %q eventID %q", stage.State, stage.ProjectUpdateID))
		}
	})
}

// ProcessFailEvent - react to a domain service failed event
// Action
func (manager *ProjectUpdateManager) ProcessFailEvent(eventMessage *automate_event.EventMsg) error {
	return manager.updateStage(func(stage config.ProjectUpdateStage) (config.ProjectUpdateStage, error) {
		switch stage.State {
		case config.NotRunningState:
		case config.RunningState:
			if eventMessage.Producer == nil || eventMessage.Producer.ID == "" {
				return stage, errors.New("Producer ID was not provided in status message")
			}
			fromProducer := eventMessage.Producer.ID
			projectUpdateID, err := getProjectUpdateID(eventMessage)
			if err != nil {
				return stage, err
			}
			if stage.ProjectUpdateID != projectUpdateID {
				// projectUpdateID does not match currently running update; ignore
				return stage, nil
			}

			domainServiceIndex, err := stage.FindDomainServiceIndex(fromProducer)
			if err != nil {
				return stage, err
			}

			failureMessage := getFailureMessage(eventMessage)

			log.Infof("Fail message from producer %q projectUpdateID %q failureMessage: %s",
				fromProducer, projectUpdateID, failureMessage)

			stage.DomainServices[domainServiceIndex].FailureMessage = failureMessage
			stage.DomainServices[domainServiceIndex].Failed = true
			stage.DomainServices[domainServiceIndex].LastUpdate = time.Now()
			err = manager.sendCancelProjectUpdateEvent()
			if err != nil {
				return stage, err
			}
		default:
		}

		return manager.checkedDomainServices(stage)
	})
}

// ProcessStatusEvent - record the status update from the domain service.
// Action
func (manager *ProjectUpdateManager) ProcessStatusEvent(
	eventMessage *automate_event.EventMsg) error {
	return manager.updateStage(func(stage config.ProjectUpdateStage) (config.ProjectUpdateStage, error) {
		switch stage.State {
		case config.NotRunningState:
		case config.RunningState:
			if eventMessage.Producer == nil || eventMessage.Producer.ID == "" {
				return stage, errors.New("Producer was not provided in status message")
			}
			fromProducer := eventMessage.Producer.ID
			projectUpdateID, err := getProjectUpdateID(eventMessage)
			if err != nil {
				return stage, err
			}

			if stage.ProjectUpdateID != projectUpdateID {
				// projectUpdateID does not match currently running update; ignore
				return stage, nil
			}

			percentageComplete, err := getPercentageComplete(eventMessage)
			if err != nil {
				return stage, err
			}

			estimatedTimeComplete, err := getEstimatedTimeCompleteInSec(eventMessage)
			if err != nil {
				return stage, err
			}

			completed, err := getCompleted(eventMessage)
			if err != nil {
				return stage, err
			}

			log.Debugf("Status message from producer: %q projectUpdateID: %q percentageComplete: %f "+
				"estimatedTimeComplete: %v completed: %t",
				fromProducer, projectUpdateID, percentageComplete, estimatedTimeComplete, completed)

			domainServiceIndex, err := stage.FindDomainServiceIndex(fromProducer)
			if err != nil {
				return stage, err
			}

			stage.DomainServices[domainServiceIndex].Complete = completed
			stage.DomainServices[domainServiceIndex].EstimatedTimeComplete = estimatedTimeComplete
			stage.DomainServices[domainServiceIndex].PercentageComplete = percentageComplete
			stage.DomainServices[domainServiceIndex].LastUpdate = time.Now()
		default:
		}

		return manager.checkedDomainServices(stage)
	})
}

// Failed - did the last update attempt fail
// Read State
func (manager *ProjectUpdateManager) Failed() bool {
	return manager.stage.Failed
}

// FailureMessage - detailed descriptions of the failure
// Read State
func (manager *ProjectUpdateManager) FailureMessage() string {
	return manager.stage.FailureMessage
}

// PercentageComplete - percentage of the job that is complete
// The percentage complete of the job that is going to run the longest is returned
// Read State
func (manager *ProjectUpdateManager) PercentageComplete() float64 {
	switch manager.stage.State {
	case config.NotRunningState:
	case config.RunningState:
		if !manager.Failed() {
			longestDomainService := config.ProjectUpdateDomainService{
				PercentageComplete:    1.0,
				EstimatedTimeComplete: time.Time{},
			}
			for _, ds := range manager.stage.DomainServices {
				if !ds.Complete && ds.EstimatedTimeComplete.After(longestDomainService.EstimatedTimeComplete) {
					longestDomainService = ds
				}
			}
			return longestDomainService.PercentageComplete
		}
	default:
	}

	return 1.0
}

// EstimatedTimeComplete - the estimated date and time of completion of the longest running job
// Read State
func (manager *ProjectUpdateManager) EstimatedTimeComplete() time.Time {
	switch manager.stage.State {
	case config.NotRunningState:
	case config.RunningState:
		longestEstimatedTimeComplete := time.Time{}
		if !manager.Failed() {
			for _, ds := range manager.stage.DomainServices {
				if !ds.Complete && ds.EstimatedTimeComplete.After(longestEstimatedTimeComplete) {
					longestEstimatedTimeComplete = ds.EstimatedTimeComplete
				}
			}
		}
		return longestEstimatedTimeComplete
	default:
	}

	return time.Time{}
}

// State - The current state of the manager. either running or not running
// Read State
func (manager *ProjectUpdateManager) State() string {
	return manager.stage.State
}

// Watching the domain services complete the update process.
// A domain service is complete when:
// * send a failure event
// * sends a complete status event
// * does not send status for 5 minutes
func (manager *ProjectUpdateManager) waitingForJobToComplete() {
	for manager.stage.State == config.RunningState {
		time.Sleep(time.Second * sleepTimeBetweenStatusChecksSec)
		err := manager.updateStage(
			func(stage config.ProjectUpdateStage) (config.ProjectUpdateStage, error) {
				return manager.checkedDomainServices(stage)
			})
		if err != nil {
			log.Errorf("checked Domain Services. %v", err)
		}
	}
}

func (manager *ProjectUpdateManager) checkedDomainServices(
	stage config.ProjectUpdateStage) (config.ProjectUpdateStage, error) {
	oldestDomainServiceUpdateTime := stage.OldestDomainServiceUpdateTime()
	if stage.AreDomainServicesComplete() {
		stage = stage.StopRunning()
	} else if stage.HasFailedDomainService() {
		failureMessage := ""
		for _, ds := range stage.DomainServices {
			if ds.Failed {
				if failureMessage != "" {
					failureMessage = fmt.Sprintf("%s; %s: %s",
						failureMessage, ds.Name, ds.FailureMessage)
				} else {
					failureMessage = fmt.Sprintf("%s: %s", ds.Name, ds.FailureMessage)
				}
			}
		}
		stage.Failed = true
		stage.FailureMessage = failureMessage
		stage = stage.StopRunning()
	} else if oldestDomainServiceUpdateTime.Before(
		time.Now().Add((-1) * time.Minute * minutesWithoutCheckingInFailure)) {
		stage.Failed = true
		stage.FailureMessage = fmt.Sprintf("A domain service has not check in for over %d minutes",
			minutesWithoutCheckingInFailure)

		stage = stage.StopRunning()
	} else if oldestDomainServiceUpdateTime.Before(time.Now().Add((-1) * time.Minute)) {
		// This domain service as not checked in within a minute
		// Send another start event
		log.Debug("Resending project update start event")
		err := manager.sendProjectUpdateStartEvent(stage.ProjectUpdateID)
		if err != nil {
			log.Errorf("Starting Project Update for domain resources. %v", err)
		}
	}

	return stage, nil
}

func (manager *ProjectUpdateManager) resumePreviousState() {
	// The LastUpdate field is stale and could have missed recent updates, updating it gives the
	// domain services time to send status updates
	for _, ds := range manager.stage.DomainServices {
		ds.LastUpdate = time.Now()
	}

	if manager.stage.State == config.RunningState {
		go manager.waitingForJobToComplete()
	}
}

// This function and the stageUpdater function allow the stage data to be accessed by a single thread. This
// prevents race conditions.
// The updateFunc function is placed in the updateQueue channel and ran in the order they are added.
// This allows only one thread to change the stage data.
// This also allows the code in the updateFunc function to be ran fully without any of the stage data
// changing.
func (manager *ProjectUpdateManager) updateStage(
	updateFunc func(config.ProjectUpdateStage) (config.ProjectUpdateStage, error)) error {
	errc := make(chan error)
	manager.updateQueue <- update{fun: updateFunc, errc: errc}

	// Wait for the function to run
	err := <-errc
	close(errc)
	return err
}

// stageUpdater - runs one update function at a time with the current stage data.
// creating a copy of the stage data to prevent the data in slices from being updated.
func (manager *ProjectUpdateManager) stageUpdater(updateQueue <-chan update) {
	for update := range updateQueue {
		updatedStage, err := update.fun(manager.stage.Copy())
		if err != nil {
			update.errc <- err
			continue
		}

		oldStage := manager.stage.Copy()
		if !updatedStage.Equal(oldStage) {
			err := manager.configManager.UpdateProjectUpdateStage(updatedStage)
			if err != nil {
				log.Errorf("Failure updating project update state %v", err)
				update.errc <- err
				continue
			}

			manager.stage = updatedStage
			manager.stageUpdateEvent(oldStage, updatedStage)
		}

		update.errc <- nil
	}
}

// When the stage state is updated to running start the waitingForJobToComplete function
func (manager *ProjectUpdateManager) stageUpdateEvent(oldStage config.ProjectUpdateStage,
	newStage config.ProjectUpdateStage) {
	if oldStage.State == config.NotRunningState && newStage.State == config.RunningState {
		go manager.waitingForJobToComplete()
	}
}

// Publish a project update event with the event-service
func (manager *ProjectUpdateManager) sendProjectUpdateStartEvent(projectUpdateID string) error {
	return sendProjectUpdateEvent(projectUpdateID,
		automate_event_type.ProjectRulesUpdate, manager.eventServiceClient)
}

// Publish a cancel project update event with the event service
func (manager *ProjectUpdateManager) sendCancelProjectUpdateEvent() error {
	return sendProjectUpdateEvent(manager.stage.ProjectUpdateID,
		automate_event_type.ProjectRulesCancelUpdate, manager.eventServiceClient)
}

func sendProjectUpdateEvent(projectUpdateID string, eventType string,
	eventServiceClient automate_event.EventServiceClient) error {
	eventUUID := createEventUUID()

	event := &automate_event.EventMsg{
		EventID:   eventUUID,
		Published: ptypes.TimestampNow(),
		Type:      &automate_event.EventType{Name: eventType},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: projectUpdateID,
					},
				},
			},
		},
	}

	pubReq := automate_event.PublishRequest{Msg: event}
	_, err := eventServiceClient.Publish(context.Background(), &pubReq)

	return err
}

func createProjectUpdateID() (string, error) {
	uuid, err := uuid.NewV4()
	if err != nil {
		return "", err
	}

	return uuid.String(), nil
}

// TODO move the creation of Event ID to the event service
func createEventUUID() string {
	uuid, err := uuid.NewV4()
	if err != nil {
		logrus.Errorf("Failed to create UUID %v", err)
		// Using a default UUID if the creation of the UUID fails
		return "39bffcd3-4325-4d18-bff5-5bd4410949ba"
	}

	return uuid.String()
}

// Get the Project Update ID out of the event Data fields
func getProjectUpdateID(event *automate_event.EventMsg) (string, error) {
	fieldName := project_update_tags.ProjectUpdateIDTag
	if event.Data != nil && event.Data.Fields != nil && event.Data.Fields[fieldName] != nil &&
		event.Data.Fields[fieldName].GetStringValue() != "" {
		return event.Data.Fields[fieldName].GetStringValue(), nil
	}

	return "", fmt.Errorf("Event message sent without a %s field eventID: %q",
		fieldName, event.EventID)
}

func getFailureMessage(event *automate_event.EventMsg) string {
	fieldName := "message"
	if event.Data != nil && event.Data.Fields != nil && event.Data.Fields[fieldName] != nil &&
		event.Data.Fields[fieldName].GetStringValue() != "" {
		return event.Data.Fields[fieldName].GetStringValue()
	}

	return ""
}

func getCompleted(event *automate_event.EventMsg) (bool, error) {
	fieldName := "Completed"
	if event.Data != nil && event.Data.Fields != nil && event.Data.Fields[fieldName] != nil {
		return event.Data.Fields[fieldName].GetBoolValue(), nil
	}

	return false, fmt.Errorf("Event message sent without a %s field eventID: %q",
		fieldName, event.EventID)
}

func getPercentageComplete(event *automate_event.EventMsg) (float64, error) {
	fieldName := "PercentageComplete"
	if event.Data != nil && event.Data.Fields != nil && event.Data.Fields[fieldName] != nil {
		return event.Data.Fields[fieldName].GetNumberValue(), nil
	}

	return 0.0, fmt.Errorf("Event message sent without a %s field eventID: %q",
		fieldName, event.EventID)
}

func getEstimatedTimeCompleteInSec(event *automate_event.EventMsg) (time.Time, error) {
	fieldName := "EstimatedTimeCompleteInSec"
	if event.Data != nil && event.Data.Fields != nil && event.Data.Fields[fieldName] != nil {
		estimatedTimeCompleteInSec := int64(event.Data.Fields[fieldName].GetNumberValue())
		return time.Unix(estimatedTimeCompleteInSec, 0), nil
	}

	return time.Time{}, fmt.Errorf("Event message sent without a %s field eventID: %q",
		fieldName, event.EventID)
}
