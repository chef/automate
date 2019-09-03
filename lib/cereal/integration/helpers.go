package integration

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
	"github.com/stretchr/testify/suite"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

type taskExecutor struct {
	Name     string
	Executor cereal.TaskExecutor
}
type workflowExecutor struct {
	Name     string
	Executor cereal.WorkflowExecutor
}

type managerOpt struct {
	TaskExecutors     []taskExecutor
	WorkflowExecutors []workflowExecutor
	ManagerOpts       []cereal.ManagerOpt
	NoStart           bool
}
type managerOptFunc func(*managerOpt)

func WithWorkflowExecutor(name string, executor cereal.WorkflowExecutor) managerOptFunc {
	return func(o *managerOpt) {
		o.WorkflowExecutors = append(o.WorkflowExecutors, workflowExecutor{
			Name:     name,
			Executor: executor,
		})
	}
}

func WithTaskExecutor(name string, executor cereal.TaskExecutor) managerOptFunc {
	return func(o *managerOpt) {
		o.TaskExecutors = append(o.TaskExecutors, taskExecutor{
			Name:     name,
			Executor: executor,
		})
	}
}

func WithManagerOpts(opts ...cereal.ManagerOpt) managerOptFunc {
	return func(o *managerOpt) {
		o.ManagerOpts = append(o.ManagerOpts, opts...)
	}
}

func WithNoStart() managerOptFunc {
	return func(o *managerOpt) {
		o.NoStart = true
	}
}

type taskExecutorWrapper struct {
	f func(context.Context, cereal.Task) (interface{}, error)
}

func (w *taskExecutorWrapper) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	return w.f(ctx, task)
}

func WithTaskExecutorF(name string, f func(context.Context, cereal.Task) (interface{}, error)) managerOptFunc {
	return WithTaskExecutor(name, &taskExecutorWrapper{f})
}

type workflowExecutorWrapper struct {
	onStart        func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision
	onTaskComplete func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision
	onCancel       func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision
}

func (wrapper *workflowExecutorWrapper) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	return wrapper.onStart(w, ev)
}

func (wrapper *workflowExecutorWrapper) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {
	return wrapper.onTaskComplete(w, ev)
}

func (wrapper *workflowExecutorWrapper) OnCancel(w cereal.WorkflowInstance,
	ev cereal.CancelEvent) cereal.Decision {
	return wrapper.onCancel(w, ev)
}

// nolint: deadcode
// it's used. the linter just isn't using the right tags
func randName(name string) string {
	now := time.Now()
	return name + now.String()
}

type CerealTestSuite struct {
	suite.Suite
	newManager func(...managerOptFunc) *cereal.Manager
}

func NewSuiteForBackend(ctx context.Context, t *testing.T, d backend.Driver) *CerealTestSuite {
	cereal.MaxWakeupInterval = 2 * time.Second
	return &CerealTestSuite{
		newManager: func(opts ...managerOptFunc) *cereal.Manager {
			o := managerOpt{}
			for _, f := range opts {
				f(&o)
			}
			m, err := cereal.NewManager(d, o.ManagerOpts...)
			require.NoError(t, err)
			for _, w := range o.WorkflowExecutors {
				err := m.RegisterWorkflowExecutor(w.Name, w.Executor)
				require.NoError(t, err)
			}
			for _, te := range o.TaskExecutors {
				err := m.RegisterTaskExecutor(te.Name, te.Executor, cereal.TaskExecutorOpts{})
				require.NoError(t, err)
			}
			if !o.NoStart {
				m.Start(ctx) // nolint: errcheck
			}
			return m
		},
	}
}
