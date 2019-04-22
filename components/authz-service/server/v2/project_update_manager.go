package v2

import (
	"context"
	"fmt"
	"time"

	automate_event "github.com/chef/automate/api/interservice/event"
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

// TODO
// Failure cases
// * No status event for one minute
// * Domain service sends failure message
// On failure send a cancel event to stop non failed domain service's task.

const (
	RunningState    = "running"
	NotRunningState = "not_running"
)

type domainService struct {
	name                   string
	percentageComplete     float64
	estimatedTimeCompelete time.Time
	lastUpdate             time.Time
	complete               bool
	failed                 bool
	failureMessage         string
}

// ProjectUpdateManager - project update manager
type ProjectUpdateManager struct {
	state              string
	projectUpdateID    string
	eventServiceClient automate_event.EventServiceClient
	domainServices     []*domainService
}

// NewProjectUpdateManager - create a new project update manager
func NewProjectUpdateManager(
	eventServiceClient automate_event.EventServiceClient) *ProjectUpdateManager {
	return &ProjectUpdateManager{
		state:              NotRunningState,
		eventServiceClient: eventServiceClient,
	}
}

// Complete - is the current update process complete
func (manager *ProjectUpdateManager) Complete() bool {
	switch manager.state {
	case NotRunningState:
	case RunningState:
		for _, ds := range manager.domainServices {
			if !ds.complete {
				return false
			}
		}
	default:
	}

	return true
}

// UpdateFailed - did the last update attempt fail
func (manager *ProjectUpdateManager) UpdateFailed() bool {
	for _, ds := range manager.domainServices {
		if ds.failed {
			return true
		}
	}

	return false
}

func (manager *ProjectUpdateManager) FailureMessages() []string {
	failureMessages := make([]string, 0)
	if manager.UpdateFailed() {
		for _, ds := range manager.domainServices {
			if ds.failed {
				failureMessages = append(failureMessages, fmt.Sprint("%s failed with %s",
					ds.name, ds.failureMessage))
			}
		}
	}

	return failureMessages
}

// PercentageComplete - percentage of the job complete
func (manager *ProjectUpdateManager) PercentageComplete() float64 {
	switch manager.state {
	case NotRunningState:
	case RunningState:
		if !manager.UpdateFailed() {
			latestEstimatedTimeCompelete := time.Time{}
			var latestDomainService *domainService
			for _, ds := range manager.domainServices {
				if !ds.complete && ds.estimatedTimeCompelete.After(latestEstimatedTimeCompelete) {
					latestEstimatedTimeCompelete = ds.estimatedTimeCompelete
					latestDomainService = ds
				}
			}
			if latestDomainService != nil {
				return latestDomainService.percentageComplete
			}
		}
	default:
	}

	return 1.0
}

// EstimatedTimeCompelete - the estimated date and time of compeletion.
func (manager *ProjectUpdateManager) EstimatedTimeCompelete() time.Time {
	switch manager.state {
	case NotRunningState:
	case RunningState:
		latestEstimatedTimeCompelete := time.Time{}
		if !manager.UpdateFailed() {
			for _, ds := range manager.domainServices {
				if !ds.complete && ds.estimatedTimeCompelete.After(latestEstimatedTimeCompelete) {
					latestEstimatedTimeCompelete = ds.estimatedTimeCompelete
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
	return manager.state
}

// Cancel - stop or cancel domain service project update process
func (manager *ProjectUpdateManager) Cancel() error {
	switch manager.state {
	case NotRunningState:
		// do nothing job is not running
	case RunningState:
		logrus.Debugf("Cancelling project update for ID %q", manager.projectUpdateID)
		err := manager.cancelProjectUpdateForDomainResources()
		if err != nil {
			return err
		}
	default:
		// error state not found
		return errors.New(fmt.Sprintf(
			"Internal error state %q eventID %q", manager.state, manager.projectUpdateID))
	}

	return nil
}

// Start - start the domain services project update process
func (manager *ProjectUpdateManager) Start() error {
	switch manager.state {
	case NotRunningState:
		projectUpdateID, err := createProjectUpdateID()
		if err != nil {
			return err
		}
		err = manager.startProjectUpdateForDomainResources(projectUpdateID)
		if err != nil {
			return err
		}

		manager.changeState(RunningState)
		manager.projectUpdateID = projectUpdateID
		go manager.waitingForJobToComplete()
		return nil
	case RunningState:
		return errors.New(fmt.Sprintf(
			"Can not start another project update %q is running", manager.projectUpdateID))
	default:
		// error state not found
		return errors.New(fmt.Sprintf(
			"Internal error state %q eventID %q", manager.state, manager.projectUpdateID))
	}
}

func (manager *ProjectUpdateManager) ProcessFailEvent(
	eventMessage *automate_event.EventMsg) error {
	switch manager.state {
	case NotRunningState:
	case RunningState:
		if eventMessage.Producer == nil || eventMessage.Producer.ID == "" {
			return errors.New("Producer was not provided in status message")
		}
		fromProducer := eventMessage.Producer.ID
		projectUpdateID, err := getProjectUpdateID(eventMessage)
		if err != nil {
			return err
		}
		if manager.projectUpdateID != projectUpdateID {
			// projectUpdateID does not match currently running update; ignore
			return nil
		}
		log.Infof("Fail message from producer %q projectUpdateID %q", fromProducer, projectUpdateID)

		domainService, err := manager.findDomainService(fromProducer)
		if err != nil {
			return err
		}

		failureMessage := getFailureMessage(eventMessage)

		domainService.failureMessage = failureMessage
		domainService.failed = true
		domainService.lastUpdate = time.Now()
	default:
	}

	return nil
}

func (manager *ProjectUpdateManager) ProcessStatusMessage(
	eventMessage *automate_event.EventMsg) error {
	switch manager.state {
	case NotRunningState:
	case RunningState:
		if eventMessage.Producer == nil || eventMessage.Producer.ID == "" {
			return errors.New("Producer was not provided in status message")
		}
		fromProducer := eventMessage.Producer.ID
		projectUpdateID, err := getProjectUpdateID(eventMessage)
		if err != nil {
			return err
		}

		if manager.projectUpdateID != projectUpdateID {
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

		log.Infof("Status message from producer: %q projectUpdateID: %q percentageComplete: %f "+
			"estimatedTimeCompelete: %v completed: %t",
			fromProducer, projectUpdateID, percentageComplete, estimatedTimeCompelete, completed)

		domainService, err := manager.findDomainService(fromProducer)
		if err != nil {
			return err
		}

		domainService.complete = completed
		domainService.estimatedTimeCompelete = estimatedTimeCompelete
		domainService.percentageComplete = percentageComplete
		domainService.lastUpdate = time.Now()

		manager.checkIfComplete()
	default:
	}

	return nil
}

func (manager *ProjectUpdateManager) changeState(newState string) {
	if manager.state != newState {
		manager.resetDomainServicesData()
		manager.state = newState
	}
}

func (manager *ProjectUpdateManager) findDomainService(
	producerID string) (*domainService, error) {
	for _, domainService := range manager.domainServices {
		if domainService.name == producerID {
			return domainService, nil
		}
	}

	return &domainService{}, errors.New(fmt.Sprintf(
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
						StringValue: manager.projectUpdateID,
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
//
// TODO
// * Allow a failed domain service some extra time to resolve its self
// * Act when a domain service has not send a status for over a minute
func (manager *ProjectUpdateManager) waitingForJobToComplete() {
	for manager.state == RunningState {
		manager.checkIfComplete()
		time.Sleep(time.Second * 5)
	}
}

func (manager *ProjectUpdateManager) checkIfComplete() {
	complete := true
	failure := false
	for _, domainService := range manager.domainServices {
		if !domainService.complete && !domainService.failed {
			complete = false

			if domainService.failed {
				failure = true
			}
			break
		}
	}

	if complete {
		if failure {
			log.Info("Finished domain services project update with a failure")
		} else {
			log.Info("Finished domain services project update with success")
		}
		manager.changeState(NotRunningState)
	}
}

func (manager *ProjectUpdateManager) resetDomainServicesData() {
	manager.domainServices = []*domainService{
		&domainService{
			name: event_ids.ComplianceInspecReportProducerID,
		},
		&domainService{
			name: event_ids.InfraClientRunsProducerID,
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
