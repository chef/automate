package history

import "time"

type EventID string

type EventClass = string

type Event struct {
	ID        EventID
	Timestamp time.Time
}

type StartEvent struct {
	Event
	Cause       EventID
	Name        EventClass
	Description string
	Tags        []Tag
}

type CompleteEvent struct {
	Event
	Err error
}

var (
	noEventID  EventID = ""
	errEventID EventID = "error"
)

func (c *CompleteEvent) IsSuccess() bool {
	return c.Err == nil
}
