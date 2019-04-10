package parser_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
)

var malformedJSON = `
{
[
}
`

var unknownManifest = `
{
  "schema_version": "99",
  "build": "20180207073355"
}
`

var noSchemaManifest = `
{
  "build": "20180207073355"
}
`

var goodV1Manifest = `
{
  "schema_version": "1",
  "git_sha": "a_test_sha",
  "build": "20180207073355",
  "packages": [
    "chef/authn-service/0.1.0/20180205105427",
    "chef/authz-service/0.1.0/20180207070425",
    "chef/automate-debug/0.1.0/20180122092256",
    "chef/automate-dex/0.1.0/20180207070707",
    "chef/automate-elasticsearch/5.6.4/20180207070906",
    "chef/automate-gateway/0.1.0/20180207071052",
    "chef/automate-load-balancer/0.1.0/20180123215807",
    "chef/automate-postgresql/9.6.3/20180207071231",
    "chef/automate-ui/2.0.0/20180206223720",
    "chef/compliance-service/1.11.1/20180207071420",
    "chef/config-mgmt-service/0.1.0/20180207071747",
    "chef/data-lifecycle-service/0.0.1/20180205110506",
    "chef/deployment-service/0.1.0/20180207071918",
    "chef/es-sidecar-service/1.0.0/20180207072056",
    "chef/ingest-service/0.1.0/20180207072223",
    "chef/license-control-service/1.0.0/20180205111105",
    "chef/local-user-service/0.1.0/20180205111232",
    "chef/notifications-service/1.0.0/20180207072353",
    "chef/session-service/0.1.0/20180207072848",
    "chef/teams-service/0.1.0/20180207073125"
  ]
}`

var goodA2Manifest = `
{
  "build": "20180716213113",
  "build_sha": "844b78fb80ca19c6a555de34d24e3e9b5a56ce06",
  "packages": [
    "chef/authn-service/0.1.0/20180716150156",
    "chef/authz-service/0.1.0/20180716150343",
    "chef/automate-cli/0.1.0/20180716191251",
    "chef/automate-cs-bookshelf/12.17.58/20180709232852",
    "chef/automate-cs-nginx/12.17.61/20180710212351",
    "chef/automate-cs-oc-bifrost/12.17.58/20180709232939",
    "chef/automate-cs-oc-erchef/12.17.61/20180709233025",
    "chef/automate-debug/0.1.0/20180619235057",
    "chef/automate-dex/0.1.0/20180716150529",
    "chef/automate-elasticsearch/6.2.2/20180621223817",
    "chef/automate-gateway/0.1.0/20180716190551",
    "chef/automate-load-balancer/0.1.0/20180619233210",
    "chef/automate-postgresql/9.6.8/20180619233409",
    "chef/automate-ui/2.0.0/20180716210152",
    "chef/compliance-service/1.11.1/20180716190030",
    "chef/config-mgmt-service/0.1.0/20180716152448",
    "chef/data-lifecycle-service/0.0.1/20180716152617",
    "chef/deployment-service/0.1.0/20180716191519",
    "chef/es-sidecar-service/1.0.0/20180716152743",
    "chef/ingest-service/0.1.0/20180716152903",
    "chef/license-control-service/1.0.0/20180716153033",
    "chef/local-user-service/0.1.0/20180716153158",
    "chef/notifications-service/1.0.0/20180716151130",
    "chef/secrets-service/1.0.0/20180716151442",
    "chef/session-service/0.1.0/20180709150923",
    "chef/teams-service/0.1.0/20180716151614",
    "core/rsync/3.1.2/20180608145950",
    "core/hab/0.59.0/20180712155441",
    "core/hab-sup/0.59.0/20180712161546",
    "core/hab-launcher/7797/20180625172404"
  ],
  "hart_overrides": [
    "chef/automate-cli:/src/results/chef-automate-cli-0.1.0-20180713011157-x86_64-linux.hart",
    "chef/deployment-service:/src/results/chef-deployment-service-0.1.0-20180703215041-x86_64-linux.hart"
  ]
}`

func TestManifestFromBytes(t *testing.T) {
	t.Run("unknown schema", func(t *testing.T) {
		_, err := parser.ManifestFromBytes([]byte(unknownManifest))
		assert.Error(t, err)
	})

	t.Run("no schema", func(t *testing.T) {
		_, err := parser.ManifestFromBytes([]byte(noSchemaManifest))
		assert.Error(t, err)
	})

	t.Run("malformed JSON", func(t *testing.T) {
		_, err := parser.ManifestFromBytes([]byte(malformedJSON))
		assert.Error(t, err)
	})

	t.Run("v1", func(t *testing.T) {
		manifest, err := parser.ManifestFromBytes([]byte(goodV1Manifest))
		assert.Nil(t, err)
		assert.Equal(t, "20180207073355", manifest.Build)
		assert.Equal(t, "a_test_sha", manifest.BuildSHA)
	})

	t.Run("A2", func(t *testing.T) {
		manifest, err := parser.ManifestFromBytes([]byte(goodA2Manifest))
		require.NoError(t, err)

		assert.Equal(t, 30, len(manifest.Packages))
		assert.Equal(t, 2, len(manifest.HartOverrides))
		assert.Equal(t, "20180716213113", manifest.Build)
		assert.Equal(t, "844b78fb80ca19c6a555de34d24e3e9b5a56ce06", manifest.BuildSHA)
	})
}

func TestPackageForServiceName(t *testing.T) {
	t.Run("v1", func(t *testing.T) {
		manifest, err := parser.ManifestFromBytes([]byte(goodV1Manifest))
		assert.Nil(t, err)

		found, pkg := manifest.PackageForServiceName("teams-service")
		assert.True(t, found)
		assert.Equal(t, "teams-service", pkg.Name())
		assert.Equal(t, "chef", pkg.Origin())
		assert.Equal(t, "0.1.0", pkg.Version())
		assert.Equal(t, "20180207073125", pkg.Release())
	})

	t.Run("A2", func(t *testing.T) {
		manifest, err := parser.ManifestFromBytes([]byte(goodA2Manifest))
		require.NoError(t, err)

		found, pkg := manifest.PackageForServiceName("automate-cli")
		require.True(t, found)
		assert.Equal(t, "automate-cli", pkg.Name())
		assert.Equal(t, "chef", pkg.Origin())
		assert.Equal(t, "0.1.0", pkg.Version())
		assert.Equal(t, "20180716191251", pkg.Release())
	})
}

func TestHartForServiceName(t *testing.T) {
	t.Run("A2", func(t *testing.T) {
		manifest, err := parser.ManifestFromBytes([]byte(goodA2Manifest))
		require.NoError(t, err)

		found, hart := manifest.HartForServiceName("automate-cli")
		require.True(t, found)
		assert.Equal(t, "automate-cli", hart.Name())
		assert.Equal(t, "chef", hart.Origin())
		assert.Equal(t, "0.1.0", hart.Version())
		assert.Equal(t, "20180713011157", hart.Release())
	})
}
