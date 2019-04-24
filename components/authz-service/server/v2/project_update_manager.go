package v2

import (
	"context"
	"fmt"
	"time"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/authz-service/config"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	project_update_tags "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
)

// Failure cases
// * No status event for 5 minutes
// * Domain service sends failure message
// * Domain service was down during start
// * Authz service restarts
//
// On failure send a cancel event to stop non failed domain service's task.

const (
	minutesWithoutCheckingInFailure = 5
	sleepTimeBetweenStatusChecksSec = 5
)

// ProjectUpdateManager - project update manager
type ProjectUpdateManager struct {
	stage              config.ProjectUpdateStage
	eventServiceClient automate_event.EventServiceClient
	configManager      *config.Manager
}

// NewProjectUpdateManager - create a new project update manager
func NewProjectUpdateManager(
	eventServiceClient automate_event.EventServiceClient,
	configManager *config.Manager) *ProjectUpdateManager {
	manager := &ProjectUpdateManager{
		stage:              configManager.GetProjectUpdateStage(),
		eventServiceClient: eventServiceClient,
		configManager:      configManager,
	}

	manager.resumePreviousState()

	return manager
}

func (manager *ProjectUpdateManager) resumePreviousState() {
	for _, ds := range manager.stage.DomainServices {
		ds.LastUpdate = time.Now()
	}

	if manager.stage.State == config.RunningState {
		go manager.waitingForJobToComplete()
	}
}

// Complete - is the current update process complete
func (manager *ProjectUpdateManager) Complete() bool {
	switch manager.stage.State {
	case config.NotRunningState:
	case config.RunningState:
		for _, ds := range manager.stage.DomainServices {
			if !ds.Complete {
				return false
			}
		}
	default:
	}

	return true
}

// UpdateFailed - did the last update attempt fail
func (manager *ProjectUpdateManager) UpdateFailed() bool {
	return manager.stage.Failed
}

// FailureMessages - list out the detailed descriptions of the failure
func (manager *ProjectUpdateManager) FailureMessages() []string {
	return manager.stage.FailureMessages
}

// PercentageComplete - percentage of the job complete
func (manager *ProjectUpdateManager) PercentageComplete() float64 {
	switch manager.stage.State {
	case config.NotRunningState:
	case config.RunningState:
		if !manager.UpdateFailed() {
			latestEstimatedTimeCompelete := time.Time{}
			latestDomainService := &config.ProjectUpdateDomainService{
				PercentageComplete: 1.0,
			}
			for _, ds := range manager.stage.DomainServices {
				if !ds.Complete && ds.EstimatedTimeCompelete.After(latestEstimatedTimeCompelete) {
					latestEstimatedTimeCompelete = ds.EstimatedTimeCompelete
					latestDomainService = ds
				}
			}
			return latestDomainService.PercentageComplete
		}
	default:
	}

	return 1.0
}

// EstimatedTimeCompelete - the estimated date and time of compeletion.
func (manager *ProjectUpdateManager) EstimatedTimeCompelete() time.Time {
	switch manager.stage.State {
	case config.NotRunningState:
	case config.RunningState:
		latestEstimatedTimeCompelete := time.Time{}
		if !manager.UpdateFailed() {
			for _, ds := range manager.stage.DomainServices {
				if !ds.Complete && ds.EstimatedTimeCompelete.After(latestEstimatedTimeCompelete) {
					latestEstimatedTimeCompelete = ds.EstimatedTimeCompelete
				}
			}
		}
		return latestEstimatedTimeCompelete
	default:
	}

	return time.Time{}
}

// State - The current state of the manager
func (manager *ProjectUpdateManager) State() string {
	return manager.stage.State
}

// Cancel - stop or cancel domain service project update process
func (manager *ProjectUpdateManager) Cancel() error {
	switch manager.stage.State {
	case config.NotRunningState:
		// do nothing job is not running
	case config.RunningState:
		logrus.Debugf("Cancelling project update for ID %q", manager.stage.ProjectUpdateID)
		err := manager.cancelProjectUpdateForDomainResources()
		if err != nil {
			return err
		}
	default:
		// error state not found
		return errors.New(fmt.Sprintf(
			"Internal error state %q eventID %q", manager.stage.State, manager.stage.ProjectUpdateID))
	}

	return nil
}

// Start - start the domain services project update process
func (manager *ProjectUpdateManager) Start() error {
	switch manager.stage.State {
	case config.NotRunningState:
		projectUpdateID, err := createProjectUpdateID()
		if err != nil {
			return err
		}
		err = manager.startProjectUpdateForDomainResources(projectUpdateID)
		if err != nil {
			return err
		}

		manager.clearFailedState()
		manager.stage.ProjectUpdateID = projectUpdateID
		manager.resetDomainServicesData()
		manager.stage.State = config.RunningState
		manager.configManager.UpdateProjectUpdateStage(manager.stage)
		go manager.waitingForJobToComplete()
		return nil
	case config.RunningState:
		return errors.New(fmt.Sprintf(
			"Can not start another project update %q is running", manager.stage.ProjectUpdateID))
	default:
		// error state not found
		return errors.New(fmt.Sprintf(
			"Internal error state %q eventID %q", manager.stage.State, manager.stage.ProjectUpdateID))
	}
}

// ProcessFailEvent - react to a domain service failing to update project tags
func (manager *ProjectUpdateManager) ProcessFailEvent(
	eventMessage *automate_event.EventMsg) error {
	switch manager.stage.State {
	case config.NotRunningState:
	case config.RunningState:
		if eventMessage.Producer == nil || eventMessage.Producer.ID == "" {
			return errors.New("Producer was not provided in status message")
		}
		fromProducer := eventMessage.Producer.ID
		projectUpdateID, err := getProjectUpdateID(eventMessage)
		if err != nil {
			return err
		}
		if manager.stage.ProjectUpdateID != projectUpdateID {
			// projectUpdateID does not match currently running update; ignore
			return nil
		}

		domainService, err := manager.findDomainService(fromProducer)
		if err != nil {
			return err
		}

		failureMessage := getFailureMessage(eventMessage)

		log.Infof("Fail message from producer %q projectUpdateID %q failureMessage: %s",
			fromProducer, projectUpdateID, failureMessage)

		domainService.FailureMessage = failureMessage
		domainService.Failed = true
		domainService.LastUpdate = time.Now()
		manager.configManager.UpdateProjectUpdateStage(manager.stage)
		err = manager.cancelProjectUpdateForDomainResources()
		if err != nil {
			return err
		}
	default:
	}

	return nil
}

// ProcessStatusEvent - record the status update from the domain service.
func (manager *ProjectUpdateManager) ProcessStatusEvent(
	eventMessage *automate_event.EventMsg) error {
	switch manager.stage.State {
	case config.NotRunningState:
	case config.RunningState:
		if eventMessage.Producer == nil || eventMessage.Producer.ID == "" {
			return errors.New("Producer was not provided in status message")
		}
		fromProducer := eventMessage.Producer.ID
		projectUpdateID, err := getProjectUpdateID(eventMessage)
		if err != nil {
			return err
		}

		if manager.stage.ProjectUpdateID != projectUpdateID {
			// projectUpdateID does not match currently running update; ignore
			return nil
		}

		percentageComplete, err := getPercentageComplete(eventMessage)
		if err != nil {
			return err
		}

		estimatedTimeCompelete, err := getEstimatedTimeCompeleteInSec(eventMessage)
		if err != nil {
			return err
		}

		completed, err := getCompleted(eventMessage)
		if err != nil {
			return err
		}

		log.Debugf("Status message from producer: %q projectUpdateID: %q percentageComplete: %f "+
			"estimatedTimeCompelete: %v completed: %t",
			fromProducer, projectUpdateID, percentageComplete, estimatedTimeCompelete, completed)

		domainService, err := manager.findDomainService(fromProducer)
		if err != nil {
			return err
		}

		domainService.Complete = completed
		domainService.EstimatedTimeCompelete = estimatedTimeCompelete
		domainService.PercentageComplete = percentageComplete
		domainService.LastUpdate = time.Now()
		manager.configManager.UpdateProjectUpdateStage(manager.stage)

		manager.checkIfComplete()
	default:
	}

	return nil
}

func (manager *ProjectUpdateManager) clearFailedState() {
	manager.stage.Failed = false
	manager.stage.FailureMessages = []string{}
}

func (manager *ProjectUpdateManager) findDomainService(
	producerID string) (*config.ProjectUpdateDomainService, error) {
	for _, domainService := range manager.stage.DomainServices {
		if domainService.Name == producerID {
			return domainService, nil
		}
	}

	return &config.ProjectUpdateDomainService{}, errors.New(fmt.Sprintf(
		"Domain service not found with producerID %q", producerID))
}

func (manager *ProjectUpdateManager) startProjectUpdateForDomainResources(
	projectUpdateID string) error {
	eventUUID := createEventUUID()

	event := &automate_event.EventMsg{
		EventID:   eventUUID,
		Published: ptypes.TimestampNow(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: projectUpdateID,
					},
				},
			},
		},
	}

	pubReq := automate_event.PublishRequest{Msg: event}
	_, err := manager.eventServiceClient.Publish(context.Background(), &pubReq)

	return err
}

func (manager *ProjectUpdateManager) cancelProjectUpdateForDomainResources() error {
	eventUUID := createEventUUID()

	event := &automate_event.EventMsg{
		EventID:   eventUUID,
		Published: ptypes.TimestampNow(),
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesCancelUpdate},
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: manager.stage.ProjectUpdateID,
					},
				},
			},
		},
	}

	pubReq := automate_event.PublishRequest{Msg: event}
	_, err := manager.eventServiceClient.Publish(context.Background(), &pubReq)

	return err
}

// Watching the domain services complete the update process.
// A domain service is complete if it fails or sends a complete status.
func (manager *ProjectUpdateManager) waitingForJobToComplete() {
	for manager.stage.State == config.RunningState {
		manager.checkIfComplete()
		manager.checkForMissingDomainServices()
		time.Sleep(time.Second * sleepTimeBetweenStatusChecksSec)
	}
}

func (manager *ProjectUpdateManager) checkForMissingDomainServices() {
	fiveMinutesAgo := time.Now().Add((-1) * time.Minute * minutesWithoutCheckingInFailure)
	oneMinutesAgo := time.Now().Add((-1) * time.Minute)
	for _, domainService := range manager.stage.DomainServices {
		if domainService.LastUpdate.Before(fiveMinutesAgo) {
			domainService.Failed = true
			domainService.FailureMessage = fmt.Sprintf("Has not check in for over %d minutes",
				minutesWithoutCheckingInFailure)
			manager.configManager.UpdateProjectUpdateStage(manager.stage)
		} else if domainService.LastUpdate.Before(oneMinutesAgo) {
			// This domain service as not checked in within a minute
			// Send another start event
			log.Debug("Resending project update start event")
			err := manager.startProjectUpdateForDomainResources(manager.stage.ProjectUpdateID)
			if err != nil {
				log.Errorf("Starting Project Update for domain resources. %v", err)
			}
		}
	}
}

func (manager *ProjectUpdateManager) checkIfComplete() {
	complete := true
	failure := false
	for _, domainService := range manager.stage.DomainServices {
		if !domainService.Complete && !domainService.Failed {
			complete = false
			break
		} else if domainService.Failed {
			failure = true
		}
	}

	if complete {
		if failure {
			log.Info("Finished domain services project update with a failure")
			failureMessages := make([]string, 0)
			for _, ds := range manager.stage.DomainServices {
				if ds.Failed {
					failureMessages = append(failureMessages, fmt.Sprintf("%s failed with %s",
						ds.Name, ds.FailureMessage))
				}
			}
			manager.stage.Failed = true
			manager.stage.FailureMessages = failureMessages
		} else {
			log.Info("Finished domain services project update with success")
		}

		manager.resetDomainServicesData()
		manager.stage.State = config.NotRunningState
		manager.configManager.UpdateProjectUpdateStage(manager.stage)
	}
}

func (manager *ProjectUpdateManager) resetDomainServicesData() {
	manager.stage.DomainServices = []*config.ProjectUpdateDomainService{
		&config.ProjectUpdateDomainService{
			Name:       event_ids.ComplianceInspecReportProducerID,
			LastUpdate: time.Now(),
		},
		&config.ProjectUpdateDomainService{
			Name:       event_ids.InfraClientRunsProducerID,
			LastUpdate: time.Now(),
		},
	}
}

func createProjectUpdateID() (string, error) {
	uuid, err := uuid.NewV4()
	if err != nil {
		return "", err
	}

	return uuid.String(), nil
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

	return 0.0, fmt.Errorf("Event message sent without a %s field eventID: %q", fieldName, event.EventID)
}

func getEstimatedTimeCompeleteInSec(event *automate_event.EventMsg) (time.Time, error) {
	fieldName := "EstimatedTimeCompeleteInSec"
	if event.Data != nil && event.Data.Fields != nil && event.Data.Fields[fieldName] != nil {
		estimatedTimeCompeleteInSec := int64(event.Data.Fields[fieldName].GetNumberValue())
		return time.Unix(estimatedTimeCompeleteInSec, 0), nil
	}

	return time.Time{}, fmt.Errorf("Event message sent without a %s field eventID: %q",
		fieldName, event.EventID)
}
