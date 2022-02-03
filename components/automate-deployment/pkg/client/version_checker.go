package client

import (
	"context"
	"fmt"
	"runtime"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	manifest_client "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

const defaultVersionCheckTimeout = 500 * time.Millisecond
const packageURLTemplate = "https://packages.chef.io/files/automate/%s/chef-automate_%s_amd64.zip"

// ManifestVersionClient is a client that implements the ManifestVersion client
type ManifestVersionClient interface {
	ManifestVersion(ctx context.Context, in *api.ManifestVersionRequest, opts ...grpc.CallOption) (*api.ManifestVersionResponse, error)
}

// WarningWriter is an interface that we use to warn about version mismatches
type WarningWriter interface {
	Warn(string)
	Warnf(m string, f ...interface{})
	WarnError(error)
}

// WarnIfNotUpToDateAgainstServer warns if deployment services indicates a newer cli version
// is available than what is being used. Also warns if errors happen talking to deployment service
func WarnIfNotUpToDateAgainstServer(connection ManifestVersionClient, writer WarningWriter, version string) {
	var err error
	if connection == nil {
		connection, err = Connection(defaultVersionCheckTimeout)
		if err != nil {
			logrus.WithError(err).Debug("Unable to connect to server to get its version. Skipping version check.")
			return
		}
	}

	response, err := connection.ManifestVersion(context.Background(), &api.ManifestVersionRequest{})
	if err != nil {
		logrus.WithError(err).Debug("Could not retrieve version from manifest")
		return
	}

	if response.CliRelease == "" {
		logrus.Debug("Unable to find required cli version")
		return
	}

	if version < response.CliRelease {
		writer.Warn(versionOutdatedMessage(version, response.CliRelease, response.BuildTimestamp))
		return
	}
}

func CLIUpToDateWithLatestManifest(hartifactsPath, overrideOrigin, channel, version string) error {
	provider := manifest.NewLocalHartManifestProvider(
		manifest_client.NewHTTPClient(),
		hartifactsPath,
		overrideOrigin,
	)

	man, err := provider.GetCurrentManifest(context.Background(), channel)
	if err != nil {
		return errors.Wrap(err, "Failed to retrieve package manifest")
	}
	found, pkg := man.PackageForServiceName("automate-cli")
	if !found {
		return errors.New("Unable to find 'automate-cli' in latest manifest")
	}

	if version >= pkg.Release() {
		return nil
	}

	return errors.New(versionOutdatedMessage(version, pkg.Release(), man.Version()))
}

func versionOutdatedMessage(installedCLIVersion string, latestCLIVersion string, latestBuildTimestamp string) string {
	url := fmt.Sprintf(packageURLTemplate, latestBuildTimestamp, runtime.GOOS)
	return fmt.Sprintf("chef-automate CLI is outdated: \n"+
		"installed: %s\n"+
		"   latest: %s\n"+
		"Download the latest at %s\n",
		installedCLIVersion, latestCLIVersion, url)
}
