package backup

import (
	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/deployment"
)

type CancellableTask struct {
	Status *api.BackupStatusResponse
	Cancel func()
}

func newCancellableTask(opType api.BackupStatusResponse_OperationType, taskIds []string, cancel func()) (*CancellableTask, error) {
	t := &CancellableTask{
		Status: &api.BackupStatusResponse{
			OpType:  opType,
			TaskIds: taskIds,
		},
		Cancel: cancel,
	}

	switch opType {
	case api.BackupStatusResponse_CREATE,
		api.BackupStatusResponse_DELETE,
		api.BackupStatusResponse_RESTORE,
		api.BackupStatusResponse_IDLE,
		api.BackupStatusResponse_VERIFY_INTEGRITY:
	default:
		return t, errors.New("invalid opType")
	}

	if opType == api.BackupStatusResponse_CREATE || opType == api.BackupStatusResponse_RESTORE {
		if len(taskIds) != 1 {
			return t, errors.New("action supports singular task ID")
		}
	}

	if opType == api.BackupStatusResponse_DELETE {
		if len(taskIds) < 1 {
			return t, errors.New("action requires at least one task ID")
		}
	}

	if opType == api.BackupStatusResponse_IDLE {
		if len(taskIds) != 0 {
			return t, errors.New("action requires no task ID's")
		}
	}

	return t, nil
}
