package backup

import (
	"context"
	"io/ioutil"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/deployment"
)

// TestBackup tests the backup execution
func TestBackup(t *testing.T) {
	t.Run("with sync failure", func(t *testing.T) {
		exec, bctx, _, cleanup := testBackupExecutor(testFailSyncSpec)
		defer cleanup()

		err := exec.Backup(bctx)

		require.Error(t, err, "returns an error of a sync operation fails")
		assert.Contains(t, "test operation failed", err.Error())
	})

	t.Run("with successful operations", func(t *testing.T) {
		exec, bctx, _, cleanup := testBackupExecutor(testSuccessSpec)
		defer cleanup()

		require.NoError(t, exec.Backup(bctx))
	})

	t.Run("timeout doesn't hang", func(t *testing.T) {
		exec, bctx, _, cleanup := testBackupExecutorWithTimeout(testSuccessSpec, 0)
		defer cleanup()

		err := exec.Backup(bctx)
		require.Error(t, err, "a timeout has happened")
		assert.Contains(t, err.Error(), "context deadline exceeded")
	})
}

func TestProgressCalculator(t *testing.T) {
	syncCal := NewProgressCalculator()
	asyncCal := NewProgressCalculator()
	svcs := []string{"service-a", "service-b", "service-c"}
	wg := sync.WaitGroup{}

	for _, s := range svcs {
		syncCal.Update(OperationProgress{Name: s, Progress: 0})
		asyncCal.Update(OperationProgress{Name: s, Progress: 0})
	}

	syncCal.Update(OperationProgress{Name: "service-a", Progress: 80})
	syncCal.Update(OperationProgress{Name: "service-b", Progress: 40})

	wg.Add(1)
	go func() {
		defer wg.Done()
		asyncCal.Update(OperationProgress{Name: "service-a", Progress: 100})
	}()

	wg.Add(1)
	go func() {
		defer wg.Done()
		asyncCal.Update(OperationProgress{Name: "service-c", Progress: 20})
	}()

	wg.Wait()

	assert.Equal(t, float64(40), syncCal.Percent())
	assert.Equal(t, float64(40), asyncCal.Percent())
}

func testLocationSpec(tmpdir string) LocationSpecification {
	return FilesystemLocationSpecification{
		Path: tmpdir,
	}
}

func testBackupExecutorWithTimeout(spec Spec, timeout time.Duration) (*Executor, Context, chan api.DeployEvent_Backup_Operation, func()) {
	// buffer the event channel so we don't block on events
	eventChan := make(chan api.DeployEvent_Backup_Operation, 100)
	errChan := make(chan error)
	tmpDir, _ := ioutil.TempDir("", "backup-executor")
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	parsed, _ := time.Parse(api.BackupTaskFormat, "20060102150405")
	ts, _ := ptypes.TimestampProto(parsed)
	bctx := Context{
		ctx:          ctx,
		backupTask:   &api.BackupTask{Id: ts},
		locationSpec: testLocationSpec(tmpDir),
	}

	return NewExecutor(
		WithEventChan(eventChan),
		WithErrorChan(errChan),
		WithSpec(spec),
		WithCancel(cancel),
	), bctx, eventChan, func() { os.RemoveAll(tmpDir) }
}

func testBackupExecutor(spec Spec) (*Executor, Context, chan api.DeployEvent_Backup_Operation, func()) {
	return testBackupExecutorWithTimeout(spec, 2*time.Second)
}

var testSuccessSpec = Spec{
	Name: "service-a",
	testSyncOps: []testOperation{
		{name: "sync-op-1"},
		{name: "sync-op-2"},
		{name: "sync-op-3"},
	},
}

var testFailSyncSpec = Spec{
	Name: "service-a",
	testSyncOps: []testOperation{
		{name: "sync-op-1"},
		{
			name: "sync-op-2-fail",
			fail: true,
		},
		{name: "sync-op-3"},
	},
}
