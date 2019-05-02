package backup

import (
	"errors"
	"fmt"
	"sort"
	"strings"
	"unicode/utf8"

	term "github.com/buger/goterm"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

// EventHandler is a client side backup event stream handler.
type EventHandler struct {
	writer cli.FormatWriter
}

// EventHandlerOpt represents an configuration function for the event handler
type EventHandlerOpt func(*EventHandler)

// NewEventHandler returns a new instance of a backup event handler
func NewEventHandler(opts ...EventHandlerOpt) *EventHandler {
	handler := &EventHandler{}

	for _, opt := range opts {
		opt(handler)
	}

	return handler
}

// WithWriter configures the event sender's writer
func WithWriter(writer cli.FormatWriter) EventHandlerOpt {
	return func(handler *EventHandler) {
		handler.writer = writer
		term.Output = writer.BufferWriter()
	}
}

// Clear clears the screen
func (eh *EventHandler) Clear() {
	term.Clear()
}

// writeMessage removes the previous message from the TTY.
func (eh *EventHandler) writeMessage(m string) {
	term.Clear()
	term.MoveCursor(1, 1)
	term.Println(m) // nolint: errcheck
	term.Flush()
}

// HandleEventError takes a deployment event, parses for backup event types and
// handles them. If an event is a completion event it'll return that the stream
// has completed.
func (eh *EventHandler) HandleEventError(event *api.DeployEvent) (completed bool, err error) {
	switch e := event.Event.(type) {
	case *api.DeployEvent_Backup_:
		switch e.Backup.Status {
		case api.DeployEvent_RUNNING:
			if len(e.Backup.Operations) < 1 {
				eh.writeMessage("Waiting for backup operation progress")
			} else {
				eh.writeMessage(eh.formatMessage(e.Backup.Operations))
			}
		case api.DeployEvent_COMPLETE_OK:
			if e.Backup.GetDescription() != nil {
				eh.writer.Title("Backup info:")
				eh.writer.Bodyf("%s: %s", "sha256", e.Backup.Description.Sha256)
			}
			return true, nil
		case api.DeployEvent_COMPLETE_FAIL:
			// Look for a failed operation and report the reason it failed
			for _, o := range e.Backup.Operations {
				if o.Error != "" {
					return true, errors.New(o.Error)
				}
			}
			// If we can't find a failed sub-operation return a generic error
			return true, errors.New("Backup failed. Check the Chef Automate logs for more information.")
		default:
			eh.writer.Title(event.Format())
		}
	}

	return false, nil
}

func (eh *EventHandler) sortOps(ops []*api.DeployEvent_Backup_Operation) []*api.DeployEvent_Backup_Operation {
	sort.Slice(ops, func(x, y int) bool {
		return ops[x].Name < ops[y].Name
	})

	return ops
}

func (eh *EventHandler) formatMessage(ops []*api.DeployEvent_Backup_Operation) string {
	maxOpLen := 0
	maxOpTypeLen := 0

	for _, o := range ops {
		nameLength := utf8.RuneCountInString(o.Name)
		typeLength := utf8.RuneCountInString(eh.opTypeToString(o.Type))
		if nameLength > maxOpLen {
			maxOpLen = nameLength
		}

		if typeLength > maxOpTypeLen {
			maxOpTypeLen = typeLength
		}
	}
	padding := maxOpLen + maxOpTypeLen
	fmtStr := fmt.Sprintf("%%-%ds %%s\n", padding)

	m := strings.Builder{}
	m.WriteString(fmt.Sprintf("%s in progress\n\n", strings.Title(eh.opTypeToString(ops[0].Type))))
	for _, o := range eh.sortOps(ops) {
		m.WriteString(fmt.Sprintf(fmtStr,
			o.Name,
			fmt.Sprintf("(sync %.2f%%)", o.SyncProgress),
		))
	}

	return m.String()
}

func (eh *EventHandler) opTypeToString(t api.DeployEvent_Backup_Operation_Type) string {
	switch t {
	case api.DeployEvent_Backup_Operation_BACKUP:
		return "backup"
	case api.DeployEvent_Backup_Operation_RESTORE:
		return "restore"
	default:
		return ""
	}
}
