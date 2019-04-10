package deployment

import (
	"errors"
)

var (
	upgradePendingMessage     = "deployment-service upgrade pending"
	reconfigurePendingMessage = "deployment-service reconfiguration pending"

	// ErrSelfUpgradePending is returned when a upgrade is pending
	ErrSelfUpgradePending = errors.New(upgradePendingMessage)

	// ErrSelfReconfigurePending is returned when the deployment
	// service needs to reconfigure itself.
	ErrSelfReconfigurePending = errors.New(reconfigurePendingMessage)
)

func IsDeploymentServicePendingError(err error) bool {
	return err == ErrSelfReconfigurePending ||
		err == ErrSelfUpgradePending
}

func IsDeploymentServicePendingMessage(msg string) bool {
	return msg == reconfigurePendingMessage ||
		msg == upgradePendingMessage
}
