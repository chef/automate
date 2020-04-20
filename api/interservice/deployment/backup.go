package deployment

import (
	"fmt"
	"sort"
	"strings"

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

func formatSnapshotIntegrity(snapshots []*SnapshotIntegrity) string {
	b := strings.Builder{}
	fmtStr := "%-16s %-18s %-11s %-17s\n"
	b.WriteString(fmt.Sprintf(fmtStr, "Backup", "Last Verification", "Corrupted", "Missing Packages"))

	sort.Slice(snapshots, func(i, j int) bool {
		if snapshots[i].GetId().GetSeconds() < snapshots[j].GetId().GetSeconds() {
			return true
		}

		if snapshots[i].GetId().GetSeconds() == snapshots[j].GetId().GetSeconds() {
			return snapshots[i].GetId().GetNanos() < snapshots[j].GetId().GetNanos()
		}

		return false
	})

	for _, snap := range snapshots {
		id := ""
		i, err := ptypes.Timestamp(snap.GetId())
		if err != nil {
			id = fmt.Sprintf("(%v)", err)
		} else {
			id = i.Format(BackupTaskFormat)
		}

		lvs := ""
		lv, err := ptypes.Timestamp(snap.GetLastVerified())
		if err != nil {
			lvs = fmt.Sprintf("(%v)", err)
		} else {
			lvs = lv.Format(BackupTaskFormat)
		}

		missings := ""
		if m := snap.GetMissing(); len(m) > 0 {
			missings = fmt.Sprintf("%d", len(m))
		}

		corrupteds := ""
		if snap.GetCorrupted() {
			corrupteds = "true"
		}

		b.WriteString(fmt.Sprintf(fmtStr, id, lvs, corrupteds, missings))
	}

	return b.String()

}

// Format formats the backup intregrity status response
func (res *BackupIntegrityShowResponse) Format() string {
	return formatSnapshotIntegrity(res.GetSnapshots())
}

// Format formats the backup intregrity response
func (res *ValidateBackupIntegrityResponse) Format() string {
	return formatSnapshotIntegrity(res.GetSnapshots())
}
