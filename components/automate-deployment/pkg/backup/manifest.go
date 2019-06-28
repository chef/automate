package backup

import (
	"context"
	"io/ioutil"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	manifest_client "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

const (
	relativeManifestPath = "deployment-service/package-manifest/package-manifest.json"
)

type ReleaseManifestLocation interface {
	Provider() manifest.ReleaseManifestProvider
}

type latestManifestLocation struct {
	hartifactsPath string
	overrideOrigin string
}

func (m *latestManifestLocation) Provider() manifest.ReleaseManifestProvider {
	return manifest.NewLocalHartManifestProvider(
		manifest_client.NewHTTPClient(),
		m.hartifactsPath,
		m.overrideOrigin,
	)
}

func NewLatestManifestLocation(hartifacts, origin string) ReleaseManifestLocation {
	return &latestManifestLocation{
		hartifactsPath: hartifacts,
		overrideOrigin: origin,
	}
}

type onDiskManifestLocation struct {
	path string
}

func (m *onDiskManifestLocation) Provider() manifest.ReleaseManifestProvider {
	return manifest_client.NewDefaultClient(m.path)
}

func NewOnDiskManifestLocation(path string) ReleaseManifestLocation {
	return &onDiskManifestLocation{path}
}

type bucketManifestLocation struct {
	data []byte
}

func (m *bucketManifestLocation) Provider() manifest.ReleaseManifestProvider {
	return manifest_client.NewInMemoryClient(m.data)
}

func NewBucketManifestLocation(ctx context.Context, bucket Bucket) (ReleaseManifestLocation, error) {
	// TODO: currently we don't have a way to validate the manifest itself;
	// when we do, this needs to not use NoOpObjectVerifier
	reader, err := bucket.NewReader(ctx, relativeManifestPath, &NoOpObjectVerifier{})
	if err != nil {
		return nil, errors.Wrap(err, "failed to open package manifest from backup")
	}
	defer reader.Close()

	data, err := ioutil.ReadAll(reader)
	if err != nil {
		return nil, errors.Wrap(err, "failed to read package manifest from backup")
	}

	return &bucketManifestLocation{data}, nil
}
