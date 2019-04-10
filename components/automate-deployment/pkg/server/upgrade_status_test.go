package server

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
)

var testUpToDateRelease = "20180207191900"
var testOutdatedRelease = "20180207191899"

func newTestManifest(names ...string) *manifest.A2 {
	packages := []habpkg.HabPkg{}
	for _, n := range names {
		packages = append(packages, habpkg.NewFQ(
			"chef",
			n,
			"0.1.0",
			testUpToDateRelease,
		))
	}

	return &manifest.A2{
		Build:    testUpToDateRelease,
		BuildSHA: "testsha",
		Packages: packages,
	}
}

func makeUpToDateServiceInfo(name string) habapi.ServiceInfo {
	return habapi.ServiceInfo{
		Pkg: habapi.ServicePkg{
			Name:    name,
			Origin:  "chef",
			Version: "0.1.0",
			Release: testUpToDateRelease,
		}}
}

func makeOutdatedServiceInfo(name string) habapi.ServiceInfo {
	return habapi.ServiceInfo{
		Pkg: habapi.ServicePkg{
			Name:    name,
			Origin:  "chef",
			Version: "0.1.0",
			Release: testOutdatedRelease,
		}}
}

func TestDetectUpgradingServices(t *testing.T) {
	manifest := newTestManifest("local-user-service", "ingest-service", "teams-service", "deployment-service")
	canonicalServices := []habpkg.HabPkg{
		habpkg.New("chef", "local-user-service"),
		habpkg.New("chef", "ingest-service"),
		habpkg.New("chef", "teams-service"),
	}

	t.Run("it returns the deployment-service as upgrading if it is older than the manifest",
		func(t *testing.T) {
			runningServices := []habapi.ServiceInfo{
				makeOutdatedServiceInfo("deployment-service"),
			}
			ret, err := detectUpgradingServices(manifest, runningServices, canonicalServices)
			require.Nil(t, err)
			require.Equal(t, 1, len(ret))
			assert.Equal(t, "deployment-service", ret[0].Target.Name)
		})

	t.Run("it returns any services where the current version doesn't match the manifest version",
		func(t *testing.T) {
			runningServices := []habapi.ServiceInfo{
				makeUpToDateServiceInfo("deployment-service"),
				makeUpToDateServiceInfo("teams-service"),
				makeUpToDateServiceInfo("ingest-service"),
				makeOutdatedServiceInfo("local-user-service"),
			}
			ret, err := detectUpgradingServices(manifest, runningServices, canonicalServices)
			require.Nil(t, err)
			require.Equal(t, 1, len(ret))
			assert.Equal(t, "local-user-service", ret[0].Target.Name)
		})

	t.Run("it returns any services in the canonical service list that aren't currently running",
		func(t *testing.T) {
			runningServices := []habapi.ServiceInfo{
				makeUpToDateServiceInfo("deployment-service"),
				makeUpToDateServiceInfo("teams-service"),
				makeUpToDateServiceInfo("ingest-service"),
			}
			ret, err := detectUpgradingServices(manifest, runningServices, canonicalServices)
			require.Nil(t, err)
			require.Equal(t, 1, len(ret))
			assert.Equal(t, "local-user-service", ret[0].Target.Name)
		})

	t.Run("it returns any services that are running but aren't in the canonical services list",
		func(t *testing.T) {
			runningServices := []habapi.ServiceInfo{
				makeUpToDateServiceInfo("deployment-service"),
				makeUpToDateServiceInfo("teams-service"),
				makeUpToDateServiceInfo("ingest-service"),
				makeUpToDateServiceInfo("local-user-service"),
				makeUpToDateServiceInfo("old-service"),
			}
			ret, err := detectUpgradingServices(manifest, runningServices, canonicalServices)
			require.Nil(t, err)
			require.Equal(t, 1, len(ret))
			assert.Nil(t, ret[0].Target)
			assert.Equal(t, "old-service", ret[0].Actual.Name)
		})
}
