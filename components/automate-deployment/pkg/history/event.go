package history

import (
	"fmt"
	"time"

	"github.com/gofrs/uuid"
)

// EventID is an ID for each event that happened
type EventID string

var (
	noEventID  EventID
	errEventID EventID = "error"
)

// Event is the base struct for Events. We have start events and
// complete events indicating when a thing started and ended.
type Event struct {
	// ID is an identifier for a thing that happened. The start
	// and completion of that thing have the same ID.
	ID EventID
	// Timestamp is when the event occurred
	Timestamp time.Time
	// EventClass describes what is the class of events, such
	// as a service upgrade
	EventClass EventClass
}

func genID() (EventID, error) {
	id, err := uuid.NewV4()
	if err != nil {
		return errEventID, err
	}

	return EventID(id.String()), nil
}

// StartEvent is emitted when an Event starts
type StartEvent struct {
	Event
	// Cause is the parent event id
	Cause EventID
	// Tags are optional key, value pairs assigned to the
	// event. For example, we would use a tag to describe
	// the service name for a service upgrade.
	Tags []Tag
}

// CompleteEvent is emitted when an Event completes
type CompleteEvent struct {
	Event
	Err error
}

func (c *CompleteEvent) IsSuccess() bool {
	return c.Err == nil
}

// EventClass describes a type of event that can happen
type EventClass struct {
	Name        string
	Description string
}

func (ec EventClass) String() string {
	return fmt.Sprintf("[%s] %s", ec.Name, ec.Description)
}

// StartupEvent is a constructor to describe events that can happen during the startup
// of deployment service
func StartupEvent(name string, description string) EventClass {
	return EventClass{
		Name:        fmt.Sprintf("Startup_%s", name),
		Description: description,
	}
}

var (
	EventStartupInit           = StartupEvent("Init", "Initializing Deployment Service")
	EventStartupGenerateSecret = StartupEvent("GenerateSecret", "Generating Secret")
)
