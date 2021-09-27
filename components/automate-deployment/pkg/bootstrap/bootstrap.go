package bootstrap

import (
	"context"

	"github.com/pkg/errors"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

type compatBootstrapper struct {
	target target.Target
	dsCmd  *DeploymentServiceCommand
}

// NewCompatBootstrapper returns a Bootstrapper that uses the latest
// bootstrap method available for version of Chef Automate version
// being deployed. This allows us to bootstrap older versions of
// deployment-service that do not support the `setup-supervisor` or
// `deploy-service` commands.
func NewCompatBootstrapper(t target.Target) target.Bootstrapper {
	return &compatBootstrapper{
		target: t,
	}
}

func FullBootstrap(ctx context.Context,
	b target.Bootstrapper,
	m manifest.ReleaseManifest,
	config *dc.ConfigRequest,
	bootstrapBundlePath string,
	writer cli.FormatWriter) error {

	writer.Body("Installing Habitat")
	err := b.InstallHabitat(ctx, m, writer)
	if err != nil {
		return err
	}

	writer.Body("Installing the Chef Automate deployment-service")
	err = b.InstallDeploymentService(ctx, config, m)
	if err != nil {
		return err
	}

	err = b.SetupSupervisor(ctx, config, m, writer)
	if err != nil {
		return err
	}

	err = b.SetHabitatEnvironment(m)
	if err != nil {
		return err
	}

	writer.Title("Bootstrapping deployment-service on localhost")
	err = b.DeployDeploymentService(ctx, config, m, bootstrapBundlePath, writer)
	if err != nil {
		return err
	}

	return nil
}

// InstallHabitat installs the Habitat binary. This always runs
// directly on the target.
func (b *compatBootstrapper) InstallHabitat(ctx context.Context, m manifest.ReleaseManifest, writer cli.BodyWriter) error {
	return b.target.InstallHabitat(ctx, m, writer)
}

// InstallDeploymentService installs the deployment-service.  This
// always runs directly on the target.
func (b *compatBootstrapper) InstallDeploymentService(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest) error {
	return b.target.InstallDeploymentService(ctx, config, m)
}

// SetHabitatEnvironment sets Habitat related environment variables in
// the current process. This helps ensure that in the case of a
// chef-automate driven bootstrap of older deployment-service
// versions, we are using the correct version of Habitat.
func (b *compatBootstrapper) SetHabitatEnvironment(m manifest.ReleaseManifest) error {
	return b.target.SetHabitatEnvironment(m)
}

// SetupSupervisor configures and starts the top-level supervisor
// configuration. It prefers to do this via the `deployment-service
// setup-supervisor` command.
func (b *compatBootstrapper) SetupSupervisor(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest, writer cli.FormatWriter) error {
	dsPkg := manifest.VersionedPackageFromManifest(m, "deployment-service")
	if dsPkg == nil {
		return errors.New("deployment-service was not found in the manifest")
	}

	dsCmd := b.getDSCmd(dsPkg, b.target)
	if dsCmd.CanBootstrap() {
		return dsCmd.SetupSupervisor(ctx, config, m)
	}
	return b.target.SetupSupervisor(ctx, config, m, writer)
}

// DeployDeploymentService configures and starts the
// deployment-service. It assumes a running Habitat supervisor.  If
// available, it delegates this task to `deployment-service
// setup-service`.
func (b *compatBootstrapper) DeployDeploymentService(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest, bootstrapBundlePath string, writer cli.BodyWriter) error {
	dsPkg := manifest.VersionedPackageFromManifest(m, "deployment-service")
	if dsPkg == nil {
		return errors.New("deployment-service was not found in the manifest")
	}

	dsCmd := b.getDSCmd(dsPkg, b.target)
	if bootstrapBundlePath != "" && !dsCmd.CanUnpackBootstrapBundle() {
		return errors.New("deployment-service does not support the bootstrap bundle")
	}
	if dsCmd.CanBootstrap() {
		return dsCmd.DeployService(ctx, config, m, bootstrapBundlePath)
	}
	return b.target.DeployDeploymentService(ctx, config, m, bootstrapBundlePath, writer)
}

func (b *compatBootstrapper) getDSCmd(dsPkg habpkg.VersionedPackage, target target.Target) *DeploymentServiceCommand {
	// NOTE(ssd) 2018-07-03: Technically this might be wrong if
	// you call DeployDeploymentService and SetupSupervisor with
	// different release manifests.
	if b.dsCmd == nil {
		b.dsCmd = NewDeploymentServiceCommand(dsPkg, target)
	}
	return b.dsCmd
}

func (b *compatBootstrapper) InstallAutomateBackendDeployment(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest) error {
	return b.target.InstallAutomateBackendDeployment(ctx, config, m)
}
