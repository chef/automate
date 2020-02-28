package client

import (
	"context"
	"io"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

// CreateBackup makes a gRPC request to the deployment service to start a
// new backup create routine. The server returns a backup ID which can be used
// to stream backup events.
func CreateBackup(conTimeout, reqTimeout time.Duration, _ cli.FormatWriter) (*api.CreateBackupResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.CreateBackupResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	res, err := con.CreateBackup(ctx, &api.CreateBackupRequest{})
	if err != nil {
		err = status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to create a backup failed",
		)
	}

	return res, err
}

// StreamBackupStatus makes a gRPC request to the deployment service and
// streams deployment events for a task to a backup event handler.
func StreamBackupStatus(conTimeout, reqTimeout time.Duration, taskID string, writer cli.FormatWriter) (*api.DeployEvent, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return nil, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	stream, err := con.DeployStatus(ctx, &api.DeployStatusRequest{
		DeploymentId: &api.DeploymentID{},
		TaskId:       taskID,
	})

	if err != nil {
		return nil, status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to stream backup events failed",
		)
	}

	eh := backup.NewEventHandler(backup.WithWriter(writer))
	for {
		event, err := stream.Recv()

		switch err {
		case io.EOF:
			return nil, nil
		case nil:
			completed, err := eh.HandleEventError(event)

			// continue in our event handling loop until our handler tells us that
			// the stream has completed or we've hit an error and should exit.
			if completed {
				if err == nil {
					return event, err
				}

				return nil, status.Wrap(
					err,
					status.BackupError,
					"Unable to handle backup event",
				)
			}
		default:
			return nil, status.Wrap(
				err,
				status.DeploymentServiceCallError,
				"Request to stream backup events failed",
			)

		}
	}
}

// ListBackups makes a gRPC request to the deployment service to list available
// backups.
func ListBackups(conTimeout, reqTimeout time.Duration) (*api.ListBackupsResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.ListBackupsResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	res, err := con.ListBackups(ctx, &api.ListBackupsRequest{})
	if err != nil {
		err = status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to list backups failed",
		)
	}

	return res, err
}

// ShowBackup makes a gRPC request to the deployment service to list available
// backups.
func ShowBackup(conTimeout, reqTimeout time.Duration, id *api.BackupTask) (*api.ShowBackupResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.ShowBackupResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	req := &api.ShowBackupRequest{Backup: id}
	res, err := con.ShowBackup(ctx, req)
	if err != nil {
		err = status.Wrapf(
			err,
			status.DeploymentServiceCallError,
			"Request to show backup %s failed",
			id.TaskID(),
		)
	}

	return res, err
}

// BackupStatus makes a gRPC request to the deployment-service for the
// backup runner status
func BackupStatus(conTimeout, reqTimeout time.Duration) (*api.BackupStatusResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.BackupStatusResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	req := &api.BackupStatusRequest{}
	res, err := con.BackupStatus(ctx, req)
	if err != nil {
		err = status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request for backup status failed",
		)
	}

	return res, err
}

// CancelBackup makes a gRPC request to the deployment-service and cancels the
// the running backup operation
func CancelBackup(conTimeout, reqTimeout time.Duration) (*api.CancelBackupResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.CancelBackupResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	req := &api.CancelBackupRequest{}
	res, err := con.CancelBackup(ctx, req)
	if err != nil {
		err = status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to cancel backup failed",
		)
	}

	return res, err
}

// DeleteBackups makes a gRPC request to the deployment service to start a
// new backup create routine. The server returns a backup ID which can be used
// to stream backup events.
func DeleteBackups(conTimeout, reqTimeout time.Duration, ids []*api.BackupTask) (*api.DeleteBackupsResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.DeleteBackupsResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	req := &api.DeleteBackupsRequest{
		Backups: ids,
	}
	res, err := con.DeleteBackups(ctx, req)
	if err != nil {
		err = status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to delete backups failed",
		)
	}

	return res, err
}

// BackupIntegrityShow makes a gRPC request to the deployment-service and returns
// the backup integrity status
func BackupIntegrityShow(conTimeout, reqTimeout time.Duration) (*api.BackupIntegrityShowResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.BackupIntegrityShowResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	req := &api.BackupIntegrityShowRequest{}
	res, err := con.BackupIntegrityShow(ctx, req)
	if err != nil {
		err = status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to cancel backup failed",
		)
	}

	return res, err
}

// ValidateBackupIntegrity makes a gRPC request to the deployment-service and returns
// the backup integrity status
func ValidateBackupIntegrity(conTimeout, reqTimeout time.Duration, ids []*api.BackupTask) (*api.ValidateBackupIntegrityResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()

	if err != nil {
		return &api.ValidateBackupIntegrityResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connection to deployment-service failed",
		)
	}

	req := &api.ValidateBackupIntegrityRequest{
		Backups: ids,
	}
	res, err := con.ValidateBackupIntegrity(ctx, req)
	if err != nil {
		err = status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to cancel backup failed",
		)
	}

	return res, err
}

func newCon(conTimeout, reqTimeout time.Duration) (api.DeployClientStreamer, context.Context, func(), error) {
	reqCtx, cancel := context.WithTimeout(context.Background(), reqTimeout)
	con, err := Connection(conTimeout)
	return con, reqCtx, cancel, err
}
