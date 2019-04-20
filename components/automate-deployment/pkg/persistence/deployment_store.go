package persistence

import (
	"errors"
	"io"

	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
)

// ErrNotInitialized is returned if the Initialize function is not called
var ErrNotInitialized = errors.New("The deployment store was not initialized")

// ErrDoesNotExist is an error that is returned if the deployment was not found
var ErrDoesNotExist = errors.New("The requested deployment does not exist")

// DeploymentUpdateCallback is a function that will be called when UpdateDeployment is called
// It will receive the current deployment, and be expected to modify it.
type DeploymentUpdateCallback func(*deployment.Deployment) error

// DeploymentStore can create, update, and retrieve deployments
type DeploymentStore interface {
	Initialize() error
	UpdateDeployment(DeploymentUpdateCallback) (*deployment.Deployment, error)
	GetDeployment() (*deployment.Deployment, error)
	WriteTo(w io.Writer) error // nolint: vet
	Close() error
}
