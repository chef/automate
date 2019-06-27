package target

import (
	"context"
	"net"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/platform/command"
)

type DesiredProcessState string

const (
	ProcessStateUp      DesiredProcessState = "up"
	ProcessStateDown    DesiredProcessState = "down"
	ProcessStateUnknown DesiredProcessState = "unknown"
)

// DeployedService represents a Habitat supervised service
type DeployedService struct {
	Pkg                 habpkg.HabPkg
	Binds               []string
	UpdateStrategy      string
	DesiredProcessState DesiredProcessState
}

// A bootstrapper is capable of getting the deployment-service
// configured and started
type Bootstrapper interface {
	// InstallHabitat should install the `hab` binary, using the
	// version specified in the manifest.
	InstallHabitat(context.Context, manifest.ReleaseManifest, cli.BodyWriter) error
	// InstallDeploymentService installs the deployment-service
	// package specified in the manifest.
	InstallDeploymentService(context.Context, *dc.ConfigRequest, manifest.ReleaseManifest) error
	// SetupSupervisor should install our habitat supervisor
	// configuration into the systems init system. If this
	// function completes without error, the Habitat supervisor
	// should be available to load further Chef Automate services.
	SetupSupervisor(context.Context, *dc.ConfigRequest, manifest.ReleaseManifest, cli.FormatWriter) error
	// DeployDeploymentService configures and starts the
	// deployment-service. If this function exits without an
	// error, the deployment-service should be loaded via Habitat.
	DeployDeploymentService(ctx context.Context, cfg *dc.ConfigRequest, releaseManifest manifest.ReleaseManifest,
		bootstrapBundlePath string, writer cli.BodyWriter) error
	// SetHabitatEnvironment is a work-around for backwards
	// compatibility. It should set the PATH and HAB_SUP_BINARY
	// environment variables in the current process to ensure that
	// further Habitat interactions use the correct version of hab
	// and hab-sup. This should be called after SetupSupervisor.
	SetHabitatEnvironment(manifest.ReleaseManifest) error
}

// NOTE(ssd) 2018-10-08: This is mostly the HabCmd interface
// but without the output return since Target is currently
// responsible for logging.
type ServiceManager interface {
	IsInstalled(context.Context, habpkg.VersionedPackage) (bool, error)
	IsBinlinked(habpkg.VersionedPackage, string) (bool, error)
	BinlinkPackage(context.Context, habpkg.VersionedPackage, string) (string, error)
	InstallService(context.Context, habpkg.Installable, string) error
	RemoveService(context.Context, habpkg.VersionedPackage) error
	LoadService(context.Context, habpkg.VersionedPackage, ...LoadOption) error
	UnloadService(context.Context, habpkg.VersionedPackage) error
	StartService(context.Context, habpkg.VersionedPackage) error
	StopService(context.Context, habpkg.VersionedPackage) error
}

// Target encapsulates all commands interacting with a2 stack
type Target interface {
	Bootstrapper
	ServiceManager

	LoadDeploymentService(context.Context, habpkg.VersionedPackage) error

	DeployedServices(ctx context.Context) (map[string]DeployedService, error)
	Status(ctx context.Context, serviceNames []string) *api.ServiceStatus

	Stop(ctx context.Context) error
	EnsureStopped() error
	Disable() error
	EnsureDisabled() error

	DestroySupervisor() error
	DestroyData() error
	DestroyPkgCache() error

	SetUserToml(name, config string) error
	GetUserToml(pkg habpkg.VersionedPackage) (string, error)

	GetDeploymentServiceReconfigurePending() (bool, error)
	SetDeploymentServiceReconfigurePending() error
	UnsetDeploymentServiceReconfigurePending() error

	SymlinkHabSup(habSupP habpkg.HabPkg) error
	GetSymlinkedHabSup() (habpkg.HabPkg, error)

	RenderAutomateUnitFile(proxyConfig string, habP habpkg.HabPkg, habLauncherP habpkg.HabPkg) (string, error)
	GetAutomateUnitFile() ([]byte, error)
	WriteAutomateUnitFile([]byte) error

	SystemdReloadRequired() (bool, error)
	SystemdReload() error
	SystemdRunning() (bool, error)

	HabSupRestartRequired(habpkg.HabPkg) (bool, error)
	HabSupRestart(context.Context, []string) (bool, error)

	CommandExecutor() command.Executor
	HabAPIClient() *habapi.Client
	HabSup() HabSup

	// IPs() returns a list of IPs assigned to the target
	IPs() []net.IP

	HabCache() depot.HabCache
}
