package deployment

import (
	"errors"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

const (
	upgradePendingMessage     = "deployment-service upgrade pending"
	reconfigurePendingMessage = "deployment-service reconfiguration pending"
	restartPendingMessage     = "deployment-service is waiting to shut down"
	backupInProgressMessage   = "deployment-service backup is running"
)

var (
	// ErrSelfUpgradePending is returned when a upgrade is pending
	ErrSelfUpgradePending = errors.New(upgradePendingMessage)

	// ErrSelfReconfigurePending is returned when the deployment
	// service needs to reconfigure itself.
	ErrSelfReconfigurePending = errors.New(reconfigurePendingMessage)

	// ErrRestartPending is returned when the deployment service
	// expects to be stopped by some external process (hab-sup or
	// systemd)
	ErrRestartPending = errors.New(restartPendingMessage)

	// ErrBackupInProgress is returned when the requested action
	// is not possible because a backup is currently being taken.
	ErrBackupInProgress = errors.New(backupInProgressMessage)
)

func IsDeploymentServicePendingError(err error) bool {
	return err == ErrSelfReconfigurePending ||
		err == ErrSelfUpgradePending ||
		err == ErrRestartPending ||
		err == ErrBackupInProgress
}

func isDeploymentServicePendingMessage(msg string) bool {
	return msg == reconfigurePendingMessage ||
		msg == upgradePendingMessage ||
		msg == restartPendingMessage ||
		msg == backupInProgressMessage
}

func IsRetriableGRPCStatus(err error) bool {
	if grpcStatus, ok := status.FromError(err); ok {
		// first test for grpc codes:
		// codes.Unavailable means that we can retry
		if grpcStatus.Code() == codes.Unavailable {
			return true
		}

		// back-compat: old versions of the server don't return a code so we
		// should check the message
		if grpcStatus.Code() == codes.Unknown {
			return isDeploymentServicePendingMessage(grpcStatus.Message())
		}
	}
	return false
}
