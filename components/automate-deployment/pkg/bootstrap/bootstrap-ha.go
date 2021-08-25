package bootstrap

import (
	"context"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

func FullBootstrapHA(ctx context.Context,
	b target.Bootstrapper,
	m manifest.ReleaseManifest,
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
	return nil
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
