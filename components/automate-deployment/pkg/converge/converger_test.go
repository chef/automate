package converge_test

import (
	"errors"
	"testing"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

type mockCompiler struct {
	convergePlan converge.Plan
	err          error
}

func (m *mockCompiler) Compile(desiredState converge.DesiredState) (converge.Plan, error) {
	return m.convergePlan, m.err
}

type mockPlan struct {
	execFunc func() error
}

func (m *mockPlan) Execute(eventSink converge.EventSink) error {
	if m.execFunc != nil {
		return m.execFunc()
	}
	return nil
}

func waitForTask(t *testing.T, task *converge.Task) {
	select {
	case <-task.C:
	case <-time.After(time.Second):
		assert.Fail(t, "Task did not complete in time")
	}
}

func TestConverger(t *testing.T) {
	localhost := NewMockTarget()
	desiredState := converge.NewDesiredState(
		converge.Topology{
			localhost: []converge.Service{},
		}, converge.NewSkipSupervisorState(),
		[]habpkg.HabPkg{},
		"conservative",
	)

	t.Run("it completes on compile error", func(t *testing.T) {
		converger := converge.StartConverger(converge.WithCompiler(&mockCompiler{
			err: errors.New("couldn't compile"),
		}))

		task, err := converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(0, task, desiredState, nil)
		require.NoError(t, err)
		waitForTask(t, task)
		converger.Stop()
	})

	t.Run("versions that do not increase or stay the same are skipped", func(t *testing.T) {
		// Compiler will return a nil plan and cause problems if we try to use it
		plan := &mockPlan{}
		converger := converge.StartConverger(
			converge.WithCompiler(&mockCompiler{
				convergePlan: plan,
			}),
			converge.WithLastVersion(uint64(1)),
		)

		task, err := converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(3, task, desiredState, nil)
		require.NoError(t, err)
		waitForTask(t, task)

		plan.execFunc = func() error {
			assert.Fail(t, "Plan should not have executed because version decreased")
			return nil
		}

		task, err = converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(2, task, desiredState, nil)
		require.NoError(t, err)
		waitForTask(t, task)

		converger.Stop()
	})

	t.Run("version gets updated if execution of a plan fails", func(t *testing.T) {
		// Compiler will return a nil plan and cause problems if we try to use it
		plan := &mockPlan{}
		plan.execFunc = func() error {
			return errors.New("Badness happened")
		}
		converger := converge.StartConverger(
			converge.WithCompiler(&mockCompiler{
				convergePlan: plan,
			}),
			converge.WithLastVersion(uint64(1)),
		)

		task, err := converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(3, task, desiredState, nil)
		require.NoError(t, err)
		select {
		case <-task.C:
		case <-time.After(time.Second):
			assert.Fail(t, "Task did not complete in time")
		}

		planExecuted := false
		planExecutedPtr := &planExecuted
		plan.execFunc = func() error {
			*planExecutedPtr = true
			return nil
		}

		task, err = converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(2, task, desiredState, nil)
		require.NoError(t, err)
		waitForTask(t, task)
		assert.False(t, planExecuted)
		converger.Stop()
	})

	t.Run("returns an error if the inbox is full", func(t *testing.T) {
		waitChan := make(chan struct{})
		waitPlan := &mockPlan{
			execFunc: func() error {
				logrus.Info("Waiting")
				<-waitChan
				logrus.Info("Finished Waiting")
				return nil
			},
		}
		converger := converge.StartConverger(
			converge.WithCompiler(&mockCompiler{
				convergePlan: waitPlan,
			}),
			converge.WithMaxInboxSize(1),
		)
		// This work will be picked up
		task, err := converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(0, task, desiredState, nil)
		require.NoError(t, err)

		// Make sure the work go picked up
		time.Sleep(100 * time.Millisecond)

		//Fill up the queue
		task, err = converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(0, task, desiredState, nil)
		require.NoError(t, err)

		// Queue is filled, now we will fail
		task, err = converge.NewTask()
		require.NoError(t, err)
		err = converger.Converge(0, task, desiredState, nil)
		assert.Error(t, err)

		waitChan <- struct{}{}
		waitChan <- struct{}{}
		waitForTask(t, task)

		converger.Stop()
	})
}
