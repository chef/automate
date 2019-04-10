package converge

import (
	"github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

const (
	deploymentServiceName    = "deployment-service"
	postgresqlServiceName    = "automate-postgresql"
	pgGatewayServiceName     = "automate-pg-gateway"
	esGatewayServiceName     = "automate-es-gateway"
	elasticsearchServiceName = "automate-elasticsearch"
)

type Converger interface {
	Converge(uint64, *Task, DesiredState, EventSink) error
	Stop()
}

// DesiredState is the desired state we wish to put our targets in. Each target
// will have services in a certain state.
type DesiredState struct {
	topology           Topology
	supervisor         SupervisorState
	ignoredPackages    []habpkg.HabPkg
	packageCleanupMode string
}

// NewDesiredState creates a DesiredState from a topology and supervisor
func NewDesiredState(topology Topology, supervisor SupervisorState, ignoredPackages []habpkg.HabPkg, packageCleanupMode string) DesiredState {
	return DesiredState{
		topology:           topology,
		supervisor:         supervisor,
		ignoredPackages:    ignoredPackages,
		packageCleanupMode: packageCleanupMode,
	}
}

type convergeRequest struct {
	version      uint64
	eventSink    EventSink
	desiredState DesiredState
	doneChan     chan error
}

// Converger is a server that handles converge requests. It will take each request,
// compile it into a plan, and execute that plan.
type converger struct {
	lastVersion uint64
	stop        chan struct{}
	inbox       chan convergeRequest
	compiler    Compiler
}

// ConvergerOpt represents a configuration for converger
type ConvergerOpt func(*converger)

// Task is a future like object that will receive a message once
// the requested task has been completed
type Task struct {
	ID uuid.UUID
	// C will receive a message when the task is done
	C chan error
}

// NewTask returns a Task which can be passed to the Converge()
// function. The Task is used to identify and signal completion of
// the requested converge.
func NewTask() (*Task, error) {
	id, err := uuid.NewV4()
	if err != nil {
		return nil, errors.Wrap(err, "could not create UUID")
	}
	doneChan := make(chan error, 1)
	return &Task{
		ID: id,
		C:  doneChan,
	}, nil
}

// WithCompiler sets the compiler to use when fulfilling a converge request.
// By default, we will use the PhaseOrderedCompiler. Specifying a compiler
// is useful in unit testing
func WithCompiler(compiler Compiler) ConvergerOpt {
	return func(converger *converger) {
		converger.compiler = compiler
	}
}

// WithLastVersion sets the minimum version the Converger will allow
// for converge requests. If a newer version is seen at any point, the
// Converger will use that as the minimum version.
// By default, this value is 0
func WithLastVersion(version uint64) ConvergerOpt {
	return func(converger *converger) {
		converger.lastVersion = version
	}
}

// WithMaxInboxSize sets the maximum backlog for converge requests. Converge
// requests surpassing the backlog will return an error
func WithMaxInboxSize(size int) ConvergerOpt {
	return func(converger *converger) {
		converger.inbox = make(chan convergeRequest, size)
	}
}

// StartConverger starts a converger. Options may be provided to override default
// values for the compiler, inbox size, last version.
// NOTE: A Converger runs in a goroutine, and this function launches it
func StartConverger(opts ...ConvergerOpt) Converger {
	converger := &converger{
		lastVersion: 0,
		inbox:       make(chan convergeRequest, 5),
		stop:        make(chan struct{}, 1),
		compiler:    NewPhaseOrderedCompiler(),
	}
	for _, opt := range opts {
		opt(converger)
	}
	go converger.run()
	return converger
}

// Converge requests the converger try to reach the specified desired state.
// version must be greater than or equal to the largest number passed to converge,
// otherwise the request will get ignored.
// This function returns an error if the backlog is full. Otherwise, a Task with
// a channel that will receive a message will be returned.
func (converger *converger) Converge(version uint64, task *Task, desiredState DesiredState, eventSink EventSink) error {
	select {
	case converger.inbox <- convergeRequest{
		version:      version,
		eventSink:    eventSink,
		desiredState: desiredState, //TODO(jaym) make a copy
		doneChan:     task.C,
	}:
		logrus.WithField("converger_inbox_len", len(converger.inbox)).Debug("Sent converge request to converger")
		return nil
	default:
		close(task.C)
		return errors.New("Inbox full")
	}
}

// Stop stops the converger
func (converger *converger) Stop() {
	converger.stop <- struct{}{}
}

func (converger *converger) run() {
	for {
		select {
		case <-converger.stop:
			//TODO(jaym) drain inbox and fail pending requests
			close(converger.inbox)
			close(converger.stop)
			return
		case req := <-converger.inbox:
			if req.version < converger.lastVersion {
				//TODO(jaym) send fail
				logrus.WithFields(logrus.Fields{
					"last_version":    converger.lastVersion,
					"request_version": req.version,
				}).Info("Skipping converge")
			} else {
				convergePlan, err := converger.compiler.Compile(req.desiredState)
				if err != nil {
					logrus.WithError(err).Error("Failed to compile")
				} else {
					err = convergePlan.Execute(req.eventSink)
					if err != nil {
						if err != api.ErrSelfUpgradePending {
							logrus.WithError(err).Error("Converge failed")
						}
					}
					converger.lastVersion = req.version
				}
				req.doneChan <- err
			}
			close(req.doneChan)
		}
	}
}
