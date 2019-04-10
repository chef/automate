package backup

import (
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
	"github.com/chef/automate/components/automate-deployment/pkg/events"
)

func TestRunnerCreate(t *testing.T) {
	t.Run("Creates a backup", func(t *testing.T) {
		r, _, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Millisecond)
		defer cleanup()
		r.specs = []Spec{}
		task, err := r.CreateBackup()

		require.NoError(t, err)
		require.NotNil(t, task)
	})

	t.Run("Creates backup fails when the timeout is exceeded", func(t *testing.T) {
		r, eventSender, cleanup := testBackupRunner(testDefaultSpecs(), 0)
		defer cleanup()

		r.specs = []Spec{}

		task, err := r.CreateBackup()

		require.NoError(t, err)
		require.NotNil(t, task)

		err = waitForBackup(eventSender)
		require.Error(t, err)

	})
}

func TestCreateBackupEvent(t *testing.T) {
	t.Run("builds an aggregate event", func(t *testing.T) {
		r, _, cleanup := testBackupRunner([]Spec{}, 5*time.Millisecond)
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

		r, eventSender, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Second)
		defer cleanup()

		r.locationSpec = testLocationSpec(dir)
		task, err := r.CreateBackup()
		require.NoError(t, err)
		require.NotNil(t, task)

		err = waitForBackup(eventSender)
		require.NoError(t, err)

		backups, err := r.ListBackups()
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
		backups, err := r.ListBackups()
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

		r, eventSender, cleanup := testBackupRunner(testDefaultSpecs(), 5*time.Second)
		defer cleanup()

		r.locationSpec = testLocationSpec(dir)
		task, err := r.CreateBackup()
		require.NoError(t, err)
		require.NotNil(t, task)

		err = waitForBackup(eventSender)
		require.NoError(t, err)

		err = r.DeleteBackups([]*api.BackupTask{task})
		require.NoError(t, err)

		backups, err := r.ListBackups()
		require.NoError(t, err)
		assert.Equal(t, 0, len(backups))
	})
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

func testBackupRunner(specs []Spec, timeout time.Duration) (*Runner, events.EventSender, func()) {
	tmpDir, _ := ioutil.TempDir("", "runner-test")

	sender := events.NewMemoryEventSender("test-backup")

	return NewRunner(
		WithEventSender(sender),
		WithSpecs(specs),
		WithTimeout(timeout),
		WithBackupLocationSpecification(testLocationSpec("/tmp")),
	), sender, func() { os.RemoveAll(tmpDir) }
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

func TestRetryFunc(t *testing.T) {
	t.Run("does not retry on success", func(t *testing.T) {
		times := 0
		err := retry(3, 0, func() error {
			times++
			return nil
		})
		assert.NoError(t, err)
		assert.Equal(t, times, 1)
	})

	t.Run("retries a maximum amount of times when function keeps erroring", func(t *testing.T) {
		times := 0
		err := retry(3, 0, func() error {
			times++
			return errors.New("blah")
		})
		assert.Error(t, err)
		assert.Equal(t, times, 3)
	})

	t.Run("stops retrying after success", func(t *testing.T) {
		times := 0
		err := retry(3, 0, func() error {
			times++
			if times == 2 {
				return nil
			}
			return errors.New("blah")
		})
		assert.NoError(t, err)
		assert.Equal(t, times, 2)
	})
}
