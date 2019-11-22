package main

import (
	"fmt"

	"github.com/boltdb/bolt"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb"
)

var opts = struct {
	Debug bool
	Force bool
}{}

func main() {
	cmd := &cobra.Command{
		Use:           "print-ds-config FILE",
		Short:         "render configuration directly from deployment-service database file",
		SilenceUsage:  true,
		SilenceErrors: true,
		Args:          cobra.ExactArgs(1),
		PersistentPreRun: func(*cobra.Command, []string) {
			if opts.Debug {
				logrus.SetLevel(logrus.DebugLevel)
			}
		},
		RunE: run,
	}

	cmd.PersistentFlags().BoolVarP(
		&opts.Debug,
		"debug",
		"d",
		false,
		"Enabled debug output")

	cmd.PersistentFlags().BoolVarP(
		&opts.Force,
		"force",
		"f",
		false,
		"Allow upgrade of deployment database. Be sure to only run on a copy of your database.")

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}

}

func run(_ *cobra.Command, args []string) error {
	dbFile := args[0]
	database, err := bolt.Open(dbFile, 0600, nil)
	if err != nil {
		return errors.Wrap(err, "could not open database")
	}
	defer database.Close()

	deploymentStore := boltdb.NewDeploymentStore(database)
	_, version, err := deploymentStore.TryRead()
	if err != nil {
		return errors.Wrap(err, "could not read deployment database")
	}

	logrus.Debugf("database is at version %s", version.Name())
	if _, isLatest := version.(boltdb.CurrentVersion); !isLatest && !opts.Force {
		return errors.Wrap(err, "database is not at most recent version. If you are sure you are running this on a COPY of your deployment database, plase pass the --force flag")
	}

	err = deploymentStore.Initialize()
	if err != nil {
		return errors.Wrap(err, "could not initialize database")
	}

	d, err := deploymentStore.GetDeployment()
	if err != nil {
		return errors.Wrap(err, "could not get deployment from database")
	}

	c := d.GetUserOverrideConfigForPersistence()
	c.Redact()
	renderedConfig, err := c.MarshalToTOML()
	if err != nil {
		return errors.Wrap(err, "could not render configuration to TOML")
	}

	fmt.Println(string(renderedConfig))
	return nil
}
