package bootstrap

import (
	"context"
	"fmt"
	"os"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

func FullBootstrapHA(ctx context.Context,
	b target.Bootstrapper,
	config *dc.ConfigRequest,
	currentM manifest.ReleaseManifest,
	writer cli.FormatWriter, saas bool) error {
	const HAB_TMP_DIR = "/hab/tmp"

	writer.Body("Installing Habitat")
	err := b.InstallHabitat(ctx, currentM, writer)
	if err != nil {
		return err
	}

	writer.Body("Installing the Chef Automate backend deployment")
	err = b.InstallAutomateBackendDeployment(ctx, config, currentM, saas)
	if err != nil {
		writer.Printf("Some error occurred %s\n", err.Error())
		return err
	}

	writer.Body("Creating a temporary folder " + HAB_TMP_DIR)
	err = createHabTmpDir(writer, HAB_TMP_DIR)
	if err != nil {
		writer.Printf("Error occured while creating a temporary directory: %s\n", err.Error())
		return err
	}

	return nil
}

func createHabTmpDir(writer cli.FormatWriter, tmpDir string) error {
	var err error
	if _, err = os.Stat(tmpDir); os.IsNotExist(err) {
		err = os.MkdirAll(tmpDir, os.ModePerm)
		if err != nil {
			return fmt.Errorf("error while creating %s dir: %w", tmpDir, err)
		}

		// Change the permissions from 0777 to 1777
		err = os.Chmod(tmpDir, 1777|os.ModeSticky)
		if err != nil {
			return fmt.Errorf("error while modifying the permissions of %s dir: %w", tmpDir, err)
		}
		return nil
	}
	return err
}
