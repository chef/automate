package backup

import (
	"context"
	"io/ioutil"

	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	manifest_client "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

const (
	relativeManifestPath = "deployment-service/package-manifest/package-manifest.json"
)

// LoadBackupManifest will return a new ReleaseManifest that has been configured
// with the settings in the BackupRestoreTask. By default it will return the
// manifest contained in the backup.
//
// If "Upgrade" is set to true it will attempt to fetch the latest release
// manifest and configure it with the channel, hartifacts path and override origin.
//
// If "Airgap" is set to true it will use the manifest contained in the airgap
// install bundle
func LoadBackupManifest(bucket Bucket, r *api.BackupRestoreTask) (manifest.ReleaseManifest, error) {
	var manifestProvider manifest.ReleaseManifestProvider

	if r.Airgap {
		manifestProvider = manifest_client.NewDefaultClient(api.AirgapManifestPath)
	} else if r.Upgrade {
		// If we're set to do a "restore upgrade" then we'll use the latest manifest
		manifestProvider = manifest.NewLocalHartManifestProvider(
			manifest_client.NewHTTPClient(),
			r.HartifactsPath,
			r.OverrideOrigin,
		)
	} else {
		// TODO: currently we don't have a way to validate the manifest itself;
		// when we do, this needs to not use NoOpObjectVerifier
		reader, err := bucket.NewReader(context.TODO(), relativeManifestPath, &NoOpObjectVerifier{})
		if err != nil {
			return nil, errors.Wrap(err, "failed to open package manifest from backup")
		}
		defer reader.Close()

		data, err := ioutil.ReadAll(reader)
		if err != nil {
			return nil, errors.Wrap(err, "failed to read package manifest from backup")
		}

		manifestProvider = manifest_client.NewInMemoryClient(data)
	}

	return manifestProvider.GetCurrentManifest(context.Background(), r.Channel)
}
