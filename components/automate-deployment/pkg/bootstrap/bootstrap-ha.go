package bootstrap

import (
	"context"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
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
	err = b.InstallAutomateBackendDeployment(ctx)
	if err != nil {
		writer.Printf("Some error occured %s\n", err.Error())
		return err
	}
	return nil
}
