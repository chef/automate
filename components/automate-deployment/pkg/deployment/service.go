package deployment

import (
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	manifest "github.com/chef/automate/components/automate-deployment/pkg/manifest"
)

// ServiceDeploymentState describes what state we expect a service in
type ServiceDeploymentState int

// WARNING: These are serialized as part of the service state. Be careful
const (
	// Skip describes the default deployment state. We haven't been told what to do
	// with the service, so we'll skip it
	Skip ServiceDeploymentState = iota
	// Installed deployment state says that the service should be installed but not running
	Installed
	// Running deployment state means the service should be running
	Running
	// Removed deployment state means the service should not be running
	Removed
)

// A Service represents a Chef Automate service managed by the
// deployment service.
type Service struct {
	habpkg.Installable
	// TODO(jaym) The things below should be moved out into something about
	// serializing the deployment state
	DeploymentState ServiceDeploymentState
	SSLKey          string
	SSLCert         string
}

// NewServiceFromHabPackage inflates a Service from a habpkg.HabPkg by
// reading the embedded service data and unmarshaling it into the
// struct.
func NewServiceFromHabPackage(habPackage habpkg.HabPkg) (service *Service) {
	return &Service{Installable: &habPackage}
}

// ServiceFromManifest creates a service from the manifest. It will prefer harts
// if available
func ServiceFromManifest(m manifest.ReleaseManifest, name string) *Service {
	pkg := manifest.InstallableFromManifest(m, name)
	if pkg == nil {
		return nil
	}

	return &Service{
		Installable: pkg,
	}
}
