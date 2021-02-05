package converge

import (
	"testing"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

func TestTransition(t *testing.T) {
	localhost := &target.MockTarget{}
	desiredState := NewDesiredState(
		Topology{
			localhost: []Service{},
		}, NewSkipSupervisorState(),
		[]habpkg.HabPkg{},
		depot.ConservativeGC)

	testCompiler := func(err error) ConvergerOpt {
		return WithCompiler(&mockCompiler{
			convergePlan: &mockPlan{err: err},
		})
	}

	t.Run("starts in idle state",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(nil),
				WithDebugChannel(),
			)
			s := c.(*converger).debugGetState()
			_, ok := s.(*idle)
			assert.True(t, ok, "is in idle state")
			c.Stop()
		})
	t.Run("idle moves to waitingForRestart if converge returns ErrRestartPending",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(api.ErrRestartPending),
				WithDebugChannel(),
			)
			task, err := NewTask()
			require.NoError(t, err)

			c.Converge(task, desiredState, nil)
			<-task.C

			s := c.(*converger).debugGetState()
			_, ok := s.(*waitingForRestart)
			assert.True(t, ok, "is in waitingForRestart")
			c.Stop()
		})

	t.Run("idle moves to waitingForReconfigure if converge returns ErrSelfReconfigurePending",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(api.ErrSelfReconfigurePending),
				WithDebugChannel(),
			)
			task, err := NewTask()
			require.NoError(t, err)

			c.Converge(task, desiredState, nil)
			<-task.C

			s := c.(*converger).debugGetState()
			_, ok := s.(*waitingForReconfigure)
			assert.True(t, ok, "is in waitingForReconfigure")
			c.Stop()
		})
	t.Run("idle moves to waitingForRestart if converge returns ErrSelfUpgradePending",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(api.ErrSelfUpgradePending),
				WithDebugChannel(),
			)
			task, err := NewTask()
			require.NoError(t, err)

			c.Converge(task, desiredState, nil)
			<-task.C

			s := c.(*converger).debugGetState()
			_, ok := s.(*waitingForRestart)
			assert.True(t, ok, "is in waitingForRestart")
			c.Stop()
		})
	t.Run("waitingForRestart ignores stopRequest",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(nil),
				WithDebugChannel(),
			)
			c.(*converger).debugSetState(newWaitingForRestart())

			task, err := NewTask()
			require.NoError(t, err)
			c.StopServices(task, localhost, nil)
			err = <-task.C
			assert.Equal(t, api.ErrRestartPending, err)

			s := c.(*converger).debugGetState()
			_, ok := s.(*waitingForRestart)
			assert.True(t, ok, "is in waitingForRestart")
			c.Stop()
		})
	t.Run("waitingForRestart ignores convergeRequest",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(nil),
				WithDebugChannel(),
			)
			c.(*converger).debugSetState(newWaitingForRestart())

			task, err := NewTask()
			require.NoError(t, err)
			c.Converge(task, desiredState, nil)
			err = <-task.C
			assert.Equal(t, api.ErrRestartPending, err)

			s := c.(*converger).debugGetState()
			_, ok := s.(*waitingForRestart)
			assert.True(t, ok, "is in waitingForRestart")
			c.Stop()
		})
	t.Run("waitingForRestart moves to idle after timeout",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(nil),
				WithDebugChannel(),
			)
			oldTimeout := waitingForRestartTimeout
			waitingForRestartTimeout = 1 * time.Millisecond
			defer func() {
				waitingForRestartTimeout = oldTimeout
			}()
			c.(*converger).debugSetState(newWaitingForRestart())
			time.Sleep(5 * time.Millisecond)
			s := c.(*converger).debugGetState()
			_, ok := s.(*idle)
			assert.True(t, ok, "is in idle")
			c.Stop()
		})
	t.Run("waitingForReconfigure ignores convergeRequest",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(nil),
				WithDebugChannel(),
			)
			c.(*converger).debugSetState(newWaitingForReconfigure())
			task, err := NewTask()
			require.NoError(t, err)
			c.Converge(task, desiredState, nil)
			err = <-task.C
			assert.Equal(t, api.ErrSelfReconfigurePending, err)

			s := c.(*converger).debugGetState()
			_, ok := s.(*waitingForReconfigure)
			assert.True(t, ok, "is in waitingForReconfigure")
			c.Stop()
		})
	t.Run("waitingForReconfigure ignores stopRequest",
		func(t *testing.T) {
			c := StartConverger(
				testCompiler(nil),
				WithDebugChannel(),
			)
			c.(*converger).debugSetState(newWaitingForReconfigure())
			task, err := NewTask()
			require.NoError(t, err)
			c.StopServices(task, localhost, nil)
			err = <-task.C
			assert.Equal(t, api.ErrSelfReconfigurePending, err)

			s := c.(*converger).debugGetState()
			_, ok := s.(*waitingForReconfigure)
			assert.True(t, ok, "is in waitingForReconfigure")
			c.Stop()
		})
	t.Run("waitingForReconfig returns to idle after timeout",
		func(t *testing.T) {
			logrus.SetLevel(logrus.DebugLevel)
			c := StartConverger(
				testCompiler(nil),
				WithDebugChannel(),
			)

			c.(*converger).debugSetState(newWaitingForReconfigure())
			c.(*converger).inbox <- &timeout{}

			// TODO(ssd) 2019-04-22: Technically this is
			// racy because nothing ensures the timeout
			// has been processed.
			s := c.(*converger).debugGetState()
			_, ok := s.(*idle)
			assert.True(t, ok, "is in idle")
			c.Stop()
		})
}

type mockCompiler struct {
	convergePlan Plan
}

func (m *mockCompiler) Compile(desiredState DesiredState) (Plan, error) {
	return m.convergePlan, nil
}

type mockPlan struct {
	err error
}

func (m *mockPlan) Execute(eventSink EventSink) error {
	return m.err
}
