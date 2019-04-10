package converge

import (
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

// The following are type assertions. They make sure all each event implements Event,
// and certain events implement a more specific event interface
var _ ServiceStartEvent = (*InstallServiceStarted)(nil)
var _ ServiceFinishEvent = (*InstallServiceFinished)(nil)
var _ ServiceStartEvent = (*ConfigureServiceStarted)(nil)
var _ ServiceFinishEvent = (*ConfigureServiceFinished)(nil)
var _ ServiceStartEvent = (*RunServiceStarted)(nil)
var _ ServiceFinishEvent = (*RunServiceFinished)(nil)
var _ ServiceEvent = (*UnloadServiceStarted)(nil)
var _ ServiceFinishEvent = (*UnloadServiceFinished)(nil)

var _ ServiceEvent = (ServiceStartEvent)(nil)
var _ ServiceEvent = (ServiceFinishEvent)(nil)

var _ SupervisorUpgradeStartEvent = (*HabPackageInstallStarted)(nil)
var _ SupervisorUpgradeFinishEvent = (*HabPackageInstallFinished)(nil)
var _ SupervisorUpgradeStartEvent = (*ConfigureSystemdStarted)(nil)
var _ SupervisorUpgradeFinishEvent = (*ConfigureSystemdFinished)(nil)
var _ SupervisorUpgradeStartEvent = (*ReloadSystemdStarted)(nil)
var _ SupervisorUpgradeFinishEvent = (*ReloadSystemdFinished)(nil)
var _ SupervisorUpgradeStartEvent = (*RestartHabSupStarted)(nil)
var _ SupervisorUpgradeFinishEvent = (*RestartHabSupFinished)(nil)

var _ SupervisorUpgradeEvent = (SupervisorUpgradeStartEvent)(nil)
var _ SupervisorUpgradeEvent = (SupervisorUpgradeFinishEvent)(nil)

var _ Event = (ServiceEvent)(nil)
var _ Event = (SupervisorUpgradeEvent)(nil)

var _ Event = (*ConvergerStarted)(nil)
var _ Event = (*ConvergerFinished)(nil)

// Event is a type that can published to the EventSink
type Event interface {
	isEvent()
	Name() string
}

// ServiceEvent is an event that has a package
type ServiceEvent interface {
	Event
	isServiceEvent()
	Pkg() habpkg.Installable
}

// ServiceFinishEvent is an interface describing something that finished about the service
type ServiceFinishEvent interface {
	ServiceEvent
	Error() error
	Modified() bool
	isFinishEvent()
}

// ServiceStartEvent is an interface describing something that is starting about the service
type ServiceStartEvent interface {
	ServiceEvent
	isStartEvent()
}

// SupervisorUpgradeEvent is an event related to updating hab-sup and systemd
type SupervisorUpgradeEvent interface {
	Event
	isSupervisorUpgradeEvent()
}

// SupervisorUpgradeFinishEvent is a completed or failed event related
// to updating hab-sup and systemd
type SupervisorUpgradeFinishEvent interface {
	SupervisorUpgradeEvent
	Error() error
	Modified() bool
	isFinishEvent()
}

// SupervisorUpgradeStartEvent is the start of an event related to
// updating hab-sup and systemd
type SupervisorUpgradeStartEvent interface {
	SupervisorUpgradeEvent
	isStartEvent()
}

// EventSink is something that can receive converge events
type EventSink interface {
	Sink(Event)
}

// ConvergerStarted is an event that is published when the converger starts working on a converge
// request
type ConvergerStarted struct {
}

func (*ConvergerStarted) isEvent() {}

// Name returns the event name. Used for logging
func (*ConvergerStarted) Name() string {
	return "converger-started"
}

// ConvergerFinished is an event that is published when the converger finishes working on a converge
// request
type ConvergerFinished struct {
	err error
}

func (*ConvergerFinished) isEvent() {}

// Name returns the event name. Used for logging
func (*ConvergerFinished) Name() string {
	return "converger-finished"
}

// Error returns the error if the install errored, nil otherwise
func (e *ConvergerFinished) Error() error { return e.err }

type started struct {
	pkg habpkg.Installable
}

// Pkg returns the package this event is about
func (e *started) Pkg() habpkg.Installable { return e.pkg }

func (e *started) isEvent()        {}
func (e *started) isStartEvent()   {}
func (e *started) isServiceEvent() {}

type finished struct {
	pkg      habpkg.Installable
	err      error
	modified bool
}

func (e *finished) isEvent()        {}
func (e *finished) isFinishEvent()  {}
func (e *finished) isServiceEvent() {}

// Pkg returns the package this event is about
func (e *finished) Pkg() habpkg.Installable { return e.pkg }

// Error returns the error if the install errored, nil otherwise
func (e *finished) Error() error { return e.err }

// Modified returns true if the package was installed
func (e *finished) Modified() bool { return e.modified }

type supStarted struct{}

func (e *supStarted) isEvent()                  {}
func (e *supStarted) isStartEvent()             {}
func (e *supStarted) isSupervisorUpgradeEvent() {}

type supFinished struct {
	err      error
	modified bool
}

func (e *supFinished) isEvent()                  {}
func (e *supFinished) isFinishEvent()            {}
func (e *supFinished) isSupervisorUpgradeEvent() {}

// Error returns the error if the install errored, nil otherwise
func (e *supFinished) Error() error { return e.err }

// Modified returns true if the package was installed
func (e *supFinished) Modified() bool { return e.modified }

// HabPackageInstallStarted is published when a package is about to be installed
// unrelated to a service.
type HabPackageInstallStarted struct {
	pkg habpkg.Installable
	supStarted
}

// Name returns the event name. Used for logging
func (*HabPackageInstallStarted) Name() string {
	return "install-package-started"
}

// Pkg returns the package this event is about
func (e *HabPackageInstallStarted) Pkg() habpkg.Installable { return e.pkg }

// HabPackageInstallFinished is published when a package is installed or
// fails to install, unrelated to a service.
type HabPackageInstallFinished struct {
	pkg habpkg.Installable
	supFinished
}

// Name returns the event name. Used for logging
func (*HabPackageInstallFinished) Name() string {
	return "install-package-finished"
}

// Pkg returns the package this event is about
func (e *HabPackageInstallFinished) Pkg() habpkg.Installable { return e.pkg }

// ConfigureSystemdStarted is published when the systemd configuration
// is about to be rendered and written to disk.
type ConfigureSystemdStarted struct {
	supStarted
}

// Name returns the event name. Used for logging
func (*ConfigureSystemdStarted) Name() string {
	return "configure-systemd-unit-started"
}

// ConfigureSystemdFinished is published when the systemd
// configuration has been written to disk or when an error has occurred
// attempting to write the configuration to disk.
type ConfigureSystemdFinished struct {
	supFinished
}

// Name returns the event name. Used for logging
func (*ConfigureSystemdFinished) Name() string {
	return "configure-systemd-unit-finished"
}

// ReloadSystemdStarted is published when the system deamon is about to be reloaded.
type ReloadSystemdStarted struct {
	supStarted
}

// Name returns the event name. Used for logging
func (*ReloadSystemdStarted) Name() string {
	return "reload-systemd-started"
}

// ReloadSystemdFinished is published when the systemd daemon has been
// reloaded or an error has occurred attempting to reload it.
type ReloadSystemdFinished struct {
	supFinished
}

// Name returns the event name. Used for logging
func (*ReloadSystemdFinished) Name() string {
	return "reload-systemd-finished"
}

// RestartHabSupStarted is published when the system deamon is about to be reloaded.
type RestartHabSupStarted struct {
	supStarted
}

// Name returns the event name. Used for logging
func (*RestartHabSupStarted) Name() string {
	return "restart-hab-sup-started"
}

// RestartHabSupFinished is published when the systemd daemon has been
// reloaded or an error has occurred attempting to reload it.
type RestartHabSupFinished struct {
	supFinished
}

// Name returns the event name. Used for logging
func (*RestartHabSupFinished) Name() string {
	return "restart-hab-sup-finished"
}

// InstallServiceStarted is published when a package is about to be installed
type InstallServiceStarted struct {
	started
}

// Name returns the event name. Used for logging
func (*InstallServiceStarted) Name() string {
	return "install-service-started"
}

// InstallServiceFinished is published when something is installed or fails to install
type InstallServiceFinished struct {
	finished
}

// Name returns the event name. Used for logging
func (*InstallServiceFinished) Name() string {
	return "install-service-finished"
}

// ConfigureServiceStarted is published when a package is about to be installed
type ConfigureServiceStarted struct {
	started
}

// Name returns the event name. Used for logging
func (*ConfigureServiceStarted) Name() string {
	return "configure-service-started"
}

// ConfigureServiceFinished is published when something is installed or fails to install
type ConfigureServiceFinished struct {
	finished
}

// Name returns the event name. Used for logging
func (*ConfigureServiceFinished) Name() string {
	return "configure-service-finished"
}

// RunServiceStarted is published when a package is about to be installed
type RunServiceStarted struct {
	started
}

// Name returns the event name. Used for logging
func (*RunServiceStarted) Name() string {
	return "run-service-started"
}

// RunServiceFinished is published when something is installed or fails to install
type RunServiceFinished struct {
	finished
}

// Name returns the event name. Used for logging
func (*RunServiceFinished) Name() string {
	return "run-service-finished"
}

// UnloadServiceStarted is published when a package is about to be installed
type UnloadServiceStarted struct {
	started
}

// Name returns the event name. Used for logging
func (*UnloadServiceStarted) Name() string {
	return "unload-service-started"
}

// UnloadServiceFinished is published when something is installed or fails to install
type UnloadServiceFinished struct {
	finished
}

// Name returns the event name. Used for logging
func (*UnloadServiceFinished) Name() string {
	return "unload-service-finished"
}

type eventWriter struct {
	eventSink EventSink
}

func newEventWriter(eventSink EventSink) *eventWriter {
	return &eventWriter{
		eventSink: eventSink,
	}
}

func (w *eventWriter) InstallingService(pkg habpkg.Installable) {
	w.eventSink.Sink(&InstallServiceStarted{
		started: started{
			pkg: pkg,
		},
	})
}

func (w *eventWriter) InstallFailed(pkg habpkg.Installable, err error) {
	w.eventSink.Sink(&InstallServiceFinished{
		finished: finished{
			pkg: pkg,
			err: err,
		},
	})
}

func (w *eventWriter) InstallSuccess(pkg habpkg.Installable, modified bool) {
	w.eventSink.Sink(&InstallServiceFinished{
		finished: finished{
			pkg:      pkg,
			modified: modified,
		},
	})
}

func (w *eventWriter) ConfiguringService(pkg habpkg.Installable) {
	w.eventSink.Sink(&ConfigureServiceStarted{
		started: started{
			pkg: pkg,
		},
	})
}

func (w *eventWriter) ConfiguringFailed(pkg habpkg.Installable, err error) {
	w.eventSink.Sink(&ConfigureServiceFinished{
		finished: finished{
			pkg: pkg,
			err: err,
		},
	})
}

func (w *eventWriter) ConfiguringSuccess(pkg habpkg.Installable, modified bool) {
	w.eventSink.Sink(&ConfigureServiceFinished{
		finished: finished{
			pkg:      pkg,
			modified: modified,
		},
	})
}

func (w *eventWriter) StartingService(pkg habpkg.Installable) {
	w.eventSink.Sink(&RunServiceStarted{
		started: started{
			pkg: pkg,
		},
	})
}

func (w *eventWriter) StartingFailed(pkg habpkg.Installable, err error) {
	w.eventSink.Sink(&RunServiceFinished{
		finished: finished{
			pkg: pkg,
			err: err,
		},
	})
}

func (w *eventWriter) StartingSuccess(pkg habpkg.Installable, modified bool) {
	w.eventSink.Sink(&RunServiceFinished{
		finished: finished{
			pkg:      pkg,
			modified: modified,
		},
	})
}

func (w *eventWriter) UnloadingService(pkg habpkg.Installable) {
	w.eventSink.Sink(&UnloadServiceStarted{
		started: started{
			pkg: pkg,
		},
	})
}

func (w *eventWriter) UnloadingFailed(pkg habpkg.Installable, err error) {
	w.eventSink.Sink(&UnloadServiceFinished{
		finished: finished{
			pkg: pkg,
			err: err,
		},
	})
}

func (w *eventWriter) UnloadingSuccess(pkg habpkg.Installable, modified bool) {
	w.eventSink.Sink(&UnloadServiceFinished{
		finished: finished{
			pkg:      pkg,
			modified: modified,
		},
	})
}

func (w *eventWriter) InstallingPackage(pkg habpkg.Installable) {
	w.eventSink.Sink(&HabPackageInstallStarted{pkg: pkg})
}

func (w *eventWriter) InstallingPackageSuccess(pkg habpkg.Installable, modified bool) {
	w.eventSink.Sink(&HabPackageInstallFinished{
		pkg: pkg,
		supFinished: supFinished{
			modified: modified,
		},
	})
}

func (w *eventWriter) InstallingPackageFailed(pkg habpkg.Installable, err error) {
	w.eventSink.Sink(&HabPackageInstallFinished{
		pkg: pkg,
		supFinished: supFinished{
			err: err,
		},
	})
}

func (w *eventWriter) ConfiguringSystemd() {
	w.eventSink.Sink(&ConfigureSystemdStarted{})
}

func (w *eventWriter) ConfiguringSystemdSuccess(modified bool) {
	w.eventSink.Sink(&ConfigureSystemdFinished{
		supFinished: supFinished{
			modified: modified,
		},
	})
}

func (w *eventWriter) ConfiguringSystemdFailed(err error) {
	w.eventSink.Sink(&ConfigureSystemdFinished{
		supFinished: supFinished{
			err: err,
		},
	})

}

func (w *eventWriter) ReloadingSystemd() {
	w.eventSink.Sink(&ReloadSystemdStarted{})
}

func (w *eventWriter) ReloadingSystemdSuccess(modified bool) {
	w.eventSink.Sink(&ReloadSystemdFinished{
		supFinished: supFinished{
			modified: modified,
		},
	})
}

func (w *eventWriter) ReloadingSystemdFailed(err error) {
	w.eventSink.Sink(&ReloadSystemdFinished{
		supFinished: supFinished{
			err: err,
		},
	})
}

func (w *eventWriter) RestartingHabSup() {
	w.eventSink.Sink(&RestartHabSupStarted{})
}

func (w *eventWriter) RestartingHabSupSuccess(modified bool) {
	w.eventSink.Sink(&RestartHabSupFinished{
		supFinished: supFinished{
			modified: modified,
		},
	})
}

func (w *eventWriter) RestartingHabSupFailed(err error) {
	w.eventSink.Sink(&RestartHabSupFinished{
		supFinished: supFinished{
			err: err,
		},
	})
}
