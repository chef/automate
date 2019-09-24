package deployment

import (
	"errors"
)

const (
	upgradePendingMessage     = "deployment-service upgrade pending"
	reconfigurePendingMessage = "deployment-service reconfiguration pending"
	restartPendingMessage     = "deployment-service is waiting to shut down"
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
)

func IsDeploymentServicePendingError(err error) bool {
	return err == ErrSelfReconfigurePending ||
		err == ErrSelfUpgradePending ||
		err == ErrRestartPending
}

func IsDeploymentServicePendingMessage(msg string) bool {
	return msg == reconfigurePendingMessage ||
		msg == upgradePendingMessage ||
		msg == restartPendingMessage
}
