package deployment

import (
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewBackupTaskFromId(t *testing.T) {
	t.Run("works with valid it", func(t *testing.T) {
		task := NewBackupTask()
		newTask, err := NewBackupTaskFromID(task.TaskID())

		require.NoError(t, err, "failed to parse id")
		assert.Equal(t, task.TaskID(), newTask.TaskID())
	})

	t.Run("fails with invalid namespace", func(t *testing.T) {
		timestamp := time.Now().Format(time.RFC3339)
		// valid ex: backup-create-2018-04-13T22:13:36.906203398Z
		badTaskID := fmt.Sprintf("invalid-namespace-%s", timestamp)
		newTask, err := NewBackupTaskFromID(badTaskID)

		require.Error(t, err)
		require.Nil(t, newTask)
	})

	t.Run("fails with invalid timestamp", func(t *testing.T) {
		// valid ex: backup-create-2018-04-13T22:13:36.906203398Z
		newTask, err := NewBackupTaskFromID("backup-create-123456789101112")

		require.Error(t, err)
		require.Nil(t, newTask)
	})
}
