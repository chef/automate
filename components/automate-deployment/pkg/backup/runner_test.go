package backup

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
)

func TestRunnerCreate(t *testing.T) {
	t.Run("Creates a backup", func(t *testing.T) {
		r, ctx, dep, sender, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Second)
		defer cleanup()
		r.specs = []Spec{}
		task, err := r.CreateBackup(ctx, dep, sender)

		require.NoError(t, err)
		require.NotNil(t, task)

		err = waitForBackup(sender)
		require.NoError(t, err)
	})

	t.Run("Creates backup fails when the timeout is exceeded", func(t *testing.T) {
		r, ctx, dep, sender, cleanup := testBackupRunner(testDefaultSpecs(), 0)
		defer cleanup()

		r.specs = []Spec{}

		task, err := r.CreateBackup(ctx, dep, sender)

		require.NoError(t, err)
		require.NotNil(t, task)

		err = waitForBackup(sender)
		require.Error(t, err)
	})
}

func TestCreateBackupEvent(t *testing.T) {
	t.Run("builds an aggregate event", func(t *testing.T) {
		r, _, _, _, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Millisecond)
		defer cleanup()

		task := &api.BackupTask{Id: ptypes.TimestampNow()}

		// Add some running operations
		r.updateOperationStatus(task.TaskID(), api.DeployEvent_Backup_Operation{
			Status:        api.DeployEvent_RUNNING,
			Name:          "service-a",
			SyncProgress:  float64(100),
			AsyncProgress: 76,
		})
		r.updateOperationStatus(task.TaskID(), api.DeployEvent_Backup_Operation{
			Status:        api.DeployEvent_COMPLETE_OK,
			Name:          "service-b",
			SyncProgress:  100,
			AsyncProgress: 100,
		})

		e := r.createBackupEvent(task.TaskID(), api.DeployEvent_RUNNING)

		assert.Equal(t, api.DeployEvent_RUNNING, e.Status)
		assert.Equal(t, 2, len(e.Operations))

		op := operationByName("service-a", e.Operations)
		assert.NotNil(t, op)
		assert.Equal(t, "service-a", op.Name)
		assert.Equal(t, api.DeployEvent_RUNNING, op.Status)
		assert.Equal(t, float64(100), op.SyncProgress)
		assert.Equal(t, float64(76), op.AsyncProgress)

		op = operationByName("service-b", e.Operations)
		assert.NotNil(t, op)
		assert.Equal(t, "service-b", op.Name)
		assert.Equal(t, api.DeployEvent_COMPLETE_OK, op.Status)
		assert.Equal(t, float64(100), op.SyncProgress)
		assert.Equal(t, float64(100), op.AsyncProgress)
	})
}

func operationByName(name string, ops []*api.DeployEvent_Backup_Operation) *api.DeployEvent_Backup_Operation {
	for _, v := range ops {
		if v.Name == name {
			return v
		}
	}
	return nil
}

func TestListBackups(t *testing.T) {
	t.Run("lists the backups", func(t *testing.T) {
		dir, err := ioutil.TempDir("", "list-backups-test")
		require.NoError(t, err, "test backup directory")
		defer os.RemoveAll(dir)

		r, ctx, dep, sender, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Second)
		defer cleanup()

		r.locationSpec = testLocationSpec(dir)
		task, err := r.CreateBackup(ctx, dep, sender)
		require.NoError(t, err)
		require.NotNil(t, task)

		err = waitForBackup(sender)
		require.NoError(t, err)

		backups, err := r.ListBackups(ctx)
		require.NoError(t, err)
		assert.Equal(t, 1, len(backups))
	})

	t.Run("lists backup only requires the runner to be configured with a backupDir to work", func(t *testing.T) {
		dir, err := ioutil.TempDir("", "list-backups-test")
		require.NoError(t, err, "test backup directory")
		defer os.RemoveAll(dir)

		// Fake out a backup dir
		os.MkdirAll(filepath.Join(dir, "20180418151753"), defaultDirPerms)

		r := NewRunner(WithBackupLocationSpecification(testLocationSpec(dir)))
		ctx := context.Background()
		backups, err := r.ListBackups(ctx)
		require.NoError(t, err)
		assert.Equal(t, 1, len(backups))
	})
}

func TestBackupDelete(t *testing.T) {
	t.Run("round-trip create/delete", func(t *testing.T) {
		t.Skip()
		dir, err := ioutil.TempDir("", "delete-backups-test")
		require.NoError(t, err, "test backup directory")
		defer os.RemoveAll(dir)

		r, ctx, dep, sender, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Second)
		defer cleanup()

		r.locationSpec = testLocationSpec(dir)
		task, err := r.CreateBackup(ctx, dep, sender)
		require.NoError(t, err)
		require.NotNil(t, task)

		err = waitForBackup(sender)
		require.NoError(t, err)

		err = r.DeleteBackups(ctx, dep, []*api.BackupTask{task})
		require.NoError(t, err)

		backups, err := r.ListBackups(ctx)
		require.NoError(t, err)
		assert.Equal(t, 0, len(backups))
	})
}

func TestCancelBackup(t *testing.T) {
	t.Run("when default/idle cancel fails", func(t *testing.T) {
		r, ctx, _, _, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Second)
		defer cleanup()

		require.Equal(t, api.BackupStatusResponse_IDLE, r.RunningTask(ctx).Status.OpType)

		err := r.Cancel(ctx)
		require.Error(t, err)
	})

	for oname, ot := range map[string]api.BackupStatusResponse_OperationType{
		"create":  api.BackupStatusResponse_CREATE,
		"delete":  api.BackupStatusResponse_DELETE,
		"restore": api.BackupStatusResponse_RESTORE,
	} {
		t.Run(fmt.Sprintf("%s cancel succeeds", oname), func(t *testing.T) {
			r, ctx, _, _, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Second)
			defer cleanup()
			success := false
			cancelFunc := func() {
				success = true
			}
			var err error

			r.runningTask, err = newCancellableTask(ot, []string{"backupid"}, cancelFunc)
			require.NoError(t, err)

			err = r.Cancel(ctx)
			require.NoError(t, err)
			require.True(t, success)
		})
	}
}

func waitForBackup(e events.EventSender) error {
	waitChan := make(chan error)
	go e.StreamTo(func(evt *api.DeployEvent) error {
		if done := evt.GetBackup(); done != nil {
			if done.GetStatus() == api.DeployEvent_COMPLETE_OK {
				close(waitChan)
			} else if done.GetStatus() == api.DeployEvent_COMPLETE_FAIL {
				waitChan <- errors.New("deploy failed")
			}
		}
		return nil
	})
	err := <-waitChan
	return err
}

func testBackupRunner(specs []Spec, timeout time.Duration) (*Runner, context.Context, *deployment.Deployment, events.EventSender, func()) {
	tmpDir, _ := ioutil.TempDir("", "runner-test")

	sender := events.NewMemoryEventSender("test-backup")
	ctx, _ := context.WithTimeout(context.Background(), timeout)
	dep, _ := deployment.CreateDeployment()
	dep.Lock()

	return NewRunner(
		WithSpecs(specs),
		WithBackupLocationSpecification(testLocationSpec("/tmp")),
	), ctx, dep, sender, func() { os.RemoveAll(tmpDir) }
}

func testDefaultSpecs() []Spec {
	return []Spec{
		{
			Name:          "config-mgmt",
			WriteMetadata: true,
			testSyncOps:   []testOperation{{name: "cfg-mgmt-sync"}},
			testAsyncOps:  []testOperation{{name: "cfg-mgmt-async"}},
		},
		{
			Name:          "deployment-service",
			WriteMetadata: true,
			testSyncOps:   []testOperation{{name: "deployment-sync"}},
			testAsyncOps:  []testOperation{{name: "deployment-async"}},
		},
	}
}
