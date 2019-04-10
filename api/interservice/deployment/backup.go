package deployment

import (
	"fmt"

	pbts "github.com/golang/protobuf/ptypes/timestamp"

	"time"

	"github.com/golang/protobuf/ptypes"
)

// BackupTaskFormat is the timestamp format used for the
// on-disk path to the backup related to a backup task. While
// lower-resolution than the full timestamp, it is path-friendly.
const BackupTaskFormat = "20060102150405"

// TaskID returns a string representation of the BackupID's task ID. It will
// be used to track the backup event tasks in the event sender store.
func (b *BackupTask) TaskID() string {
	return backupFormat(b.Id)
}

// NewBackupTask returns a new backup task
func NewBackupTask() *BackupTask {
	return &BackupTask{Id: ptypes.TimestampNow()}
}

type FormattedBackupTask struct {
	Id    string `json:"id"`
	State string `json:"state"`
}

func (b *BackupTask) Formatted() FormattedBackupTask {
	t := FormattedBackupTask{
		Id: b.TaskID(),
	}

	switch b.State {
	case BackupTask_IN_PROGRESS:
		t.State = "in_progress"
	case BackupTask_COMPLETED:
		t.State = "completed"
	default:
		t.State = "UNKNOWN"
	}

	return t
}

// NewBackupTaskFromID returns a new backup task from an existing TaskID.
func NewBackupTaskFromID(id string) (*BackupTask, error) {
	tstamp, err := time.Parse(BackupTaskFormat, id)

	if err != nil {
		return nil, err
	}

	tp, err := ptypes.TimestampProto(tstamp)
	if err != nil {
		return nil, err
	}

	return &BackupTask{Id: tp}, nil
}

// TaskID returns a string representation of the BackupDeleteID's task ID. It
// will be used to track the backup event tasks in the event sender store.
func (d *BackupDeleteTask) TaskID() string {
	return backupFormat(d.Id)
}

// TaskID returns a string representation of the BackupRestoreID's task ID. It
// will be used to track the backup event tasks in the event sender store.
func (r *BackupRestoreTask) TaskID() string {
	return backupFormat(r.Id)
}

func backupFormat(ts *pbts.Timestamp) string {
	t, err := ptypes.Timestamp(ts)
	if err != nil {
		return fmt.Sprintf("(%v)", err)
	}
	return t.Format(BackupTaskFormat)
}

// NewBackupRestoreTask returns a new backup restore task
func NewBackupRestoreTask() *BackupRestoreTask {
	return &BackupRestoreTask{Id: ptypes.TimestampNow()}
}

// NewBackupRestoreTaskFromBackupID returns a new backup task from an existing TaskID.
func NewBackupRestoreTaskFromBackupID(backupID string) (*BackupRestoreTask, error) {
	rt := NewBackupRestoreTask()
	bt, err := NewBackupTaskFromID(backupID)

	if err != nil {
		return nil, err
	}

	rt.Backup = bt

	return rt, nil
}
