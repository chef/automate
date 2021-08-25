package bootstrap

import (
	"context"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

func FullBootstrapHA(ctx context.Context,
	b target.Bootstrapper,
	m manifest.ReleaseManifest,
	bootstrapBundlePath string,
	writer cli.FormatWriter) error {

	writer.Body("Installing Habitat")
	err := b.InstallHabitat(ctx, m, writer)
	if err != nil {
		return err
	}

	writer.Body("Installing the Chef Automate backend deployment")
	err = InstallAutomateBackendDeployment(ctx)
	if err != nil {
		writer.Printf("Some error occured %s\n", err.Error())
		return err
	}

	/*err = b.SetHabitatEnvironment(m)
	if err != nil {
		return err
	}*/

	/*writer.Title("Bootstrapping deployment-service on localhost")
	err = b.DeployDeploymentService(ctx, config, m, bootstrapBundlePath, writer)
	if err != nil {
		return err
	}*/

	return nil
}

// InstallHabitat installs the Habitat binary. This always runs
// directly on the target.
func (b *compatBootstrapper) InstallHabitatHA(ctx context.Context, m manifest.ReleaseManifest, writer cli.BodyWriter) error {
	return b.target.InstallHabitat(ctx, m, writer)
}

// InstallDeploymentService installs the deployment-service.  This
// always runs directly on the target.
func (b *compatBootstrapper) InstallDeploymentServiceHA(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest) error {
	return b.target.InstallDeploymentService(ctx, config, m)
}

// SetHabitatEnvironment sets Habitat related environment variables in
// the current process. This helps ensure that in the case of a
// chef-automate driven bootstrap of older deployment-service
// versions, we are using the correct version of Habitat.
func (b *compatBootstrapper) SetHabitatEnvironmentHA(m manifest.ReleaseManifest) error {
	return b.target.SetHabitatEnvironment(m)
}

// SetupSupervisor configures and starts the top-level supervisor
// configuration. It prefers to do this via the `deployment-service
// setup-supervisor` command.
func (b *compatBootstrapper) SetupSupervisorHA(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest, writer cli.FormatWriter) error {
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
func (b *compatBootstrapper) DeployDeploymentServiceHA(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest, bootstrapBundlePath string, writer cli.BodyWriter) error {
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

func (b *compatBootstrapper) getDSCmdHA(dsPkg habpkg.VersionedPackage, target target.Target) *DeploymentServiceCommand {
	// NOTE(ssd) 2018-07-03: Technically this might be wrong if
	// you call DeployDeploymentService and SetupSupervisor with
	// different release manifests.
	if b.dsCmd == nil {
		b.dsCmd = NewDeploymentServiceCommand(dsPkg, target)
	}
	return b.dsCmd
}

func InstallAutomateBackendDeployment(ctx context.Context) error {
	localTarget := target.NewLocalTarget(false)
	var pkg habpkg.HabPkg = habpkg.New("chef", "automate-backend-deployment")
	output, err := localTarget.InstallPackage(ctx, &pkg, "stable")
	if err != nil {
		logrus.WithError(err).WithFields(logrus.Fields{
			"package": pkg.InstallIdent(),
			"output":  output,
		}).Error("install failed")
		return errors.Wrapf(err, "msg=\"failed to install\" package=%s output=%s", pkg.InstallIdent(), output)
	}
	logrus.Info("Chef Automate backend deployment Installed")
	return nil
}
