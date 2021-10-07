package bootstrap

import (
	"context"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

func FullBootstrapHA(ctx context.Context,
	b target.Bootstrapper,
	config *dc.ConfigRequest,
	currentM manifest.ReleaseManifest,
	writer cli.FormatWriter) error {

	writer.Body("Installing Habitat")
	err := b.InstallHabitat(ctx, currentM, writer)
	if err != nil {
		return err
	}

	writer.Body("Installing the Chef Automate backend deployment")
	err = b.InstallAutomateBackendDeployment(ctx, config, currentM)
	if err != nil {
		writer.Printf("Some error occurred %s\n", err.Error())
		return err
	}
	return nil
}
