package main

import (
	"github.com/boltdb/bolt"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb"
)

const DBFile = "/hab/svc/deployment-service/data/bolt.db"

var opts = struct {
	Debug bool
}{}

func main() {
	cmd := &cobra.Command{
		Use:   "danger-delete-ds-certs",
		Short: "DANGER -- Removes certificates from a deployment-service database. Only run this tool if directed to by Chef Support",
		Long: `
!!! DANGER DANGER DANGER !!!

This tool modifies the deployment-service's bolt database.

Only run this tool if explicitly told to by Chef Support.
Do not run this tool when deployment-service is running.

!!! DANGER DANGER DANGER !!!
`,
		SilenceUsage:  true,
		SilenceErrors: true,
		Args:          cobra.NoArgs,
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

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}

}

func run(*cobra.Command, []string) error {
	logrus.Infof("Attempting to remove service certificates from %s", DBFile)
	database, err := bolt.Open(DBFile, 0600, nil)
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
	if _, isLatest := version.(boltdb.CurrentVersion); !isLatest {
		return errors.Wrap(err, "database is not at most recent version, cannot modifiy")
	}

	err = deploymentStore.Initialize()
	if err != nil {
		return errors.Wrap(err, "could not initialize database")
	}

	_, err = deploymentStore.UpdateDeployment(func(d *deployment.Deployment) error {
		for _, s := range d.ExpectedServices {
			if s.SSLKey != "" || s.SSLCert != "" {
				logrus.Infof("Found certificate data for %s", habpkg.ShortIdent(s))
				s.SSLKey = ""
				s.SSLCert = ""
			} else {
				logrus.Infof("No certificate data for %s", habpkg.ShortIdent(s))
			}
		}
		return nil
	})
	return err
}
