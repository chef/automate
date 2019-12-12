package backup

import (
	"bytes"
	"io/ioutil"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
)

func TestBackupHandler(t *testing.T) {
	t.Run("when a backup succeeds", func(t *testing.T) {
		eh := newTestBackupHandler()

		e := newBackupEvent(
			api.DeployEvent_COMPLETE_OK,
			[]*api.DeployEvent_Backup_Operation{
				{
					Status:       api.DeployEvent_COMPLETE_OK,
					Name:         "service-a",
					SyncProgress: 100,
				},
			},
		)
		e.GetBackup().Description = &api.BackupDescription{Sha256: "example-sha256-for-test"}

		completed, err := eh.HandleEventError(&e)
		require.True(t, completed, "completes if COMPLETE_OK message is received")
		require.NoError(t, err)
	})

	t.Run("when a backup fails with a sub-operation error", func(t *testing.T) {
		eh := newTestBackupHandler()

		e := newBackupEvent(
			api.DeployEvent_COMPLETE_FAIL,
			[]*api.DeployEvent_Backup_Operation{
				{
					Status:       api.DeployEvent_COMPLETE_OK,
					Name:         "service-a",
					SyncProgress: 100,
				},
				{
					Status:       api.DeployEvent_COMPLETE_FAIL,
					Name:         "service-b",
					SyncProgress: 100,
					Error:        "sub-operation failed",
				},
			},
		)

		completed, err := eh.HandleEventError(&e)
		require.True(t, completed, "returns true if COMPLETE_FAIL message is received")
		require.Error(t, err)
		assert.Equal(t, "sub-operation failed", err.Error(), "reports sub-operation error message")
	})

	t.Run("when a backup fails without a sub-operation error", func(t *testing.T) {
		eh := newTestBackupHandler()

		e := newBackupEvent(
			api.DeployEvent_COMPLETE_FAIL,
			[]*api.DeployEvent_Backup_Operation{
				{
					Status:       api.DeployEvent_COMPLETE_OK,
					Name:         "service-a",
					SyncProgress: 100,
				},
				{
					Status:       api.DeployEvent_COMPLETE_FAIL,
					Name:         "service-b",
					SyncProgress: 100,
				},
			},
		)

		completed, err := eh.HandleEventError(&e)
		require.True(t, completed, "returns true if COMPLETE_FAIL message is received")
		require.Error(t, err)
		assert.Equal(t, "Backup failed. Check the Chef Automate logs for more information.", err.Error())
	})

	t.Run("when RUNNING with backup operations", func(t *testing.T) {
		eh := newTestBackupHandler()

		e := newBackupEvent(
			api.DeployEvent_RUNNING,
			[]*api.DeployEvent_Backup_Operation{
				{
					Status:       api.DeployEvent_COMPLETE_OK,
					Name:         "service-a",
					SyncProgress: 100,
				},
				{
					Status:       api.DeployEvent_RUNNING,
					Name:         "service-b",
					SyncProgress: 66,
				},
			},
		)

		completed, err := eh.HandleEventError(&e)
		require.False(t, completed, "returns false because the backup has not completed")
		require.NoError(t, err)
	})
}

func newTestBackupHandler() *EventHandler {
	w := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))
	return NewEventHandler(WithWriter(w))
}

func newBackupEvent(status api.DeployEvent_Status,
	ops []*api.DeployEvent_Backup_Operation) api.DeployEvent {

	return events.EventForBackup(1234, "test", time.Now(), &api.DeployEvent_Backup{
		Status:     status,
		Operations: ops,
	})
}
