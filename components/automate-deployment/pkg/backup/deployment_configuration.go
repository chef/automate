package backup

import (
	"context"
	"io/ioutil"
	"path"

	"github.com/boltdb/bolt"
	"github.com/pkg/errors"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb"
)

// LoadDeploymentConfig loads the automate configuration directly from
// the bolt database contained in the given backup.
func LoadDeploymentConfig(bucket Bucket, verifier ObjectVerifier) (*deployment.AutomateConfig, error) {
	// This is a roundabout way for creating a random temporary file. ioutil.WriteFile lets
	// us rewrite the file at that path with the right permissions
	tmpfile, err := ioutil.TempFile("", "chef-automate-restore-bolt")
	if err != nil {
		return nil, errors.Wrap(err, "failed to create temporary file for deployment-service database")
	}
	if err := tmpfile.Close(); err != nil {
		return nil, errors.Wrap(err, "failed to close temporary file for deployment-service database")
	}

	// TODO(ssd) 2018-05-15: Gross, this hard-codes knowledge that
	// was previously only in the spec.
	dbStorageKey := path.Join("deployment-service", "bolt", "bolt.db")
	reader, err := bucket.NewReader(context.TODO(), dbStorageKey, verifier)
	if err != nil {
		return nil, errors.Wrap(err, "failed to open deployment-service database from backup")
	}
	defer reader.Close()

	data, err := ioutil.ReadAll(reader)
	if err != nil {
		return nil, errors.Wrap(err, "failed to deployment-service database from backup")
	}

	if err := ioutil.WriteFile(tmpfile.Name(), data, 0600); err != nil {
		return nil, errors.Wrap(err, "failed to close write file for deployment-service database")
	}

	database, err := bolt.Open(tmpfile.Name(), 0600, nil)
	if err != nil {
		return nil, errors.Wrap(err, "failed opening deployment-service database from backup")
	}
	defer database.Close()

	deploymentStore := boltdb.NewDeploymentStore(database)
	err = deploymentStore.Initialize()
	if err != nil {
		return nil, errors.Wrap(err, "failed to initialize deployment store from backup")
	}

	existingDeployment, err := deploymentStore.GetDeployment()
	if err != nil {
		return nil, errors.Wrap(err, "failed to get deployment from backed up deployment store")
	}

	return existingDeployment.Config, nil
}
