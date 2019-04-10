package a1upgrade

import (
	"fmt"
	"strings"

	term "github.com/buger/goterm"
	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

var (
	// ErrMigrationFailed is returned from HandleStatus if the migration status is failed
	ErrMigrationFailed = errors.New("Automate 1 data migrations failed")
	// ErrMigrationStatusUnknown is returned from HandleStatus if the migration status is unknown
	ErrMigrationStatusUnknown = errors.New("Automate 1 data migrations failed")
)

// StatusHandler is a client side backup event stream handler.
type StatusHandler struct {
	noTTY  bool
	writer cli.FormatWriter
}

// StatusHandlerOpt represents an configuration function for the event handler
type StatusHandlerOpt func(*StatusHandler)

// NewStatusHandler returns a new instance of a backup event handler
func NewStatusHandler(opts ...StatusHandlerOpt) *StatusHandler {
	handler := &StatusHandler{}

	for _, opt := range opts {
		opt(handler)
	}

	return handler
}

// NoTTY configures the status handler to produce simple output suitable
// for non-TTY writers.
func NoTTY() StatusHandlerOpt {
	return func(handler *StatusHandler) {
		handler.noTTY = true
	}
}

// WithWriter configures the status handlers's writer
func WithWriter(writer cli.FormatWriter) StatusHandlerOpt {
	return func(handler *StatusHandler) {
		handler.writer = writer
		term.Output = writer.BufferWriter()
	}
}

// writeMessage removes the previous message from the TTY.
func (sh *StatusHandler) writeMessage(m string) {
	if sh.noTTY {
		sh.writer.Print(m)
		return
	}
	term.MoveCursor(1, 1)
	term.Clear()
	term.Print(m)
	term.Flush()
}

func buildStatusList(statuses []*api.A1UpgradeStatusResponse_ServiceMigrationStatus) string {
	msg := new(strings.Builder)
	for _, svcStatus := range statuses {
		switch svcStatus.Status {
		case api.A1UpgradeStatusResponse_IN_PROGRESS:
			msg.WriteString(fmt.Sprintf(" - %s MIGRATING: %d%% %s\n", svcStatus.ServiceName, svcStatus.Progress, svcStatus.Info))
		case api.A1UpgradeStatusResponse_COMPLETE:
			msg.WriteString(fmt.Sprintf(" ✓ %s COMPLETE!\n", svcStatus.ServiceName))
		case api.A1UpgradeStatusResponse_FAILED:
			msg.WriteString(fmt.Sprintf(" ✗ %s FAILED: %s\n", svcStatus.ServiceName, svcStatus.Info))
		case api.A1UpgradeStatusResponse_UNKNOWN:
			msg.WriteString(fmt.Sprintf(" ✗ %s UNKNOWN: %s\n", svcStatus.ServiceName, svcStatus.Info))
		}
	}
	return msg.String()
}

// HandleStatus handles status
func (sh *StatusHandler) HandleStatus(status *api.A1UpgradeStatusResponse) (done bool, err error) {
	switch status.OverallStatus {
	case api.A1UpgradeStatusResponse_IN_PROGRESS:
		msg := buildStatusList(status.ServiceStatuses)
		sh.writeMessage(msg)
		return false, nil
	case api.A1UpgradeStatusResponse_COMPLETE:
		msg := buildStatusList(status.ServiceStatuses)
		sh.writeMessage(msg)
		sh.writer.Success("Migration complete!")
		return true, nil
	case api.A1UpgradeStatusResponse_FAILED:
		msg := buildStatusList(status.ServiceStatuses)
		sh.writeMessage(msg)
		sh.writer.Fail("Migration failed.")
		return true, ErrMigrationFailed
	case api.A1UpgradeStatusResponse_UNKNOWN:
		buildStatusList(status.ServiceStatuses)
		sh.writer.Fail("Upgrade status is unknown")
		return true, ErrMigrationStatusUnknown
	}

	return false, nil
}
