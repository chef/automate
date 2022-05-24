package client_test

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

var goodV1Manifest = `
{
  "schema_version": "1",
  "git_sha": "a_test_sha",
  "build": "20180207073355",
  "packages": [
    "chef/teams-service/0.1.0/20180207073125"
  ]
}`

var goodSemManifest = `
{
  "schema_version": "2",
  "git_sha": "a_test_sha",
  "build": "20180207073355",
  "version": "2.3.5",
  "packages": [
    "chef/teams-service/0.1.0/20180207073125"
  ]
}`

func TestGetCurrentManifest(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, goodSemManifest)
	}))
	defer ts.Close()

	testFmt := fmt.Sprintf("%s/%%s", ts.URL)
	client := client.NewHTTPClient(client.LatestSemanticURLFormat(testFmt), client.NoVerify(true))
	manifest, err := client.GetCurrentManifest(context.Background(), "current")
	require.NoError(t, err)
	assert.Equal(t, 1, len(manifest.Packages))
	assert.Equal(t, "2.3.5", manifest.Version())

	found, pkg := manifest.PackageForServiceName("teams-service")
	require.True(t, found)
	assert.Equal(t, "teams-service", pkg.Name())
	assert.Equal(t, "chef", pkg.Origin())
	assert.Equal(t, "0.1.0", pkg.Version())
	assert.Equal(t, "20180207073125", pkg.Release())
}

func TestGetBackCompatable(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.Contains(r.URL.String(), "latest_semver.json") {
			w.WriteHeader(http.StatusNotFound)
		}
		if strings.Contains(r.URL.String(), "latest.json") {
			fmt.Fprintln(w, goodV1Manifest)
		}
	}))
	defer ts.Close()

	testSemVerFmt := fmt.Sprintf("%s/%%s/latest_semver.json", ts.URL)
	testFmt := fmt.Sprintf("%s/%%s/latest.json", ts.URL)
	client := client.NewHTTPClient(client.LatestSemanticURLFormat(testSemVerFmt),
		client.LatestURLFormat(testFmt),
		client.NoVerify(true))
	manifest, err := client.GetCurrentManifest(context.Background(), "current")
	require.NoError(t, err)
	assert.Equal(t, 1, len(manifest.Packages))
	assert.Equal(t, "20180207073355", manifest.Version())

	found, pkg := manifest.PackageForServiceName("teams-service")
	require.True(t, found)
	assert.Equal(t, "teams-service", pkg.Name())
	assert.Equal(t, "chef", pkg.Origin())
	assert.Equal(t, "0.1.0", pkg.Version())
	assert.Equal(t, "20180207073125", pkg.Release())
}

func TestGetManifest(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, goodV1Manifest)
	}))
	defer ts.Close()

	testFmt := fmt.Sprintf("%s/%%s", ts.URL)
	client := client.NewHTTPClient(client.URLFormat(testFmt), client.NoVerify(true))
	manifest, err := client.GetManifest(context.Background(), "20180207073355")
	require.NoError(t, err)
	assert.Equal(t, 1, len(manifest.Packages))

	found, pkg := manifest.PackageForServiceName("teams-service")
	require.True(t, found)
	assert.Equal(t, "teams-service", pkg.Name())
	assert.Equal(t, "chef", pkg.Origin())
	assert.Equal(t, "0.1.0", pkg.Version())
	assert.Equal(t, "20180207073125", pkg.Release())
}

func TestGetManifestReturnNoSuchManifestOn404(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotFound)
	}))
	defer ts.Close()

	testFmt := fmt.Sprintf("%s/%%s", ts.URL)
	client := client.NewHTTPClient(client.URLFormat(testFmt), client.NoVerify(true))
	_, err := client.GetManifest(context.Background(), "20180207073355")
	require.Error(t, err)
	_, ok := err.(*manifest.NoSuchManifestError)
	require.True(t, ok, "error should be a NoSuchManifestError")
}

func TestVerification(t *testing.T) {
	chefSignedManifest :=
		`{
  "schema_version": "1",
  "build": "20181207131525",
  "hab": [
    "core/hab/0.69.0/20181127182011",
    "core/hab-sup/0.69.0/20181127183841",
    "core/hab-launcher/9106/20181126205526"
  ],
  "hab_build": "core/hab/0.59.0/20180712155441",
  "git_sha": "45e9604294d1b3908f9104e19a5538770ed8d4b2",
  "packages": [
    "chef/authn-service/0.1.0/20181206101503",
    "chef/authz-service/0.1.0/20181206164837",
    "chef/automate-chef-io/0.1.0/20181206205516",
    "chef/automate-cli/0.1.0/20181207131102",
    "chef/automate-cs-bookshelf/12.18.4/20181130132209",
    "chef/automate-cs-nginx/12.18.3/20181130135245",
    "chef/automate-cs-oc-bifrost/12.18.4/20181130132256",
    "chef/automate-cs-oc-erchef/12.18.4/20181130132353",
    "chef/automate-debug/0.1.0/20181011124837",
    "chef/automate-dex/0.1.0/20181130131252",
    "chef/automate-elasticsearch/6.2.2/20181130130730",
    "chef/automate-gateway/0.1.0/20181207011834",
    "chef/automate-load-balancer/0.1.0/20181130133337",
    "chef/automate-postgresql/9.6.8/20180807041839",
    "chef/automate-ui/2.0.0/20181206212345",
    "chef/automate-workflow-nginx/2.8.26/20181130135402",
    "chef/automate-workflow-server/2.8.30/20181207130859",
    "chef/backup-gateway/0.1.0/20181204224819",
    "chef/compliance-service/1.11.1/20181207011536",
    "chef/config-mgmt-service/0.1.0/20181206103043",
    "chef/data-lifecycle-service/0.0.1/20181206103221",
    "chef/deployment-service/0.1.0/20181207131244",
    "chef/es-sidecar-service/1.0.0/20181206103749",
    "chef/event-service/0.1.0/20181206102132",
    "chef/ingest-service/0.1.0/20181206103910",
    "chef/license-control-service/1.0.0/20181206102259",
    "chef/local-user-service/0.1.0/20181206104041",
    "chef/notifications-service/1.0.0/20181130164345",
    "chef/secrets-service/1.0.0/20181206102417",
    "chef/session-service/0.1.0/20181206102548",
    "chef/teams-service/0.1.0/20181206102727",
    "core/rsync/3.1.2/20180608145950"
  ]
}`

	manifestSignature := `-----BEGIN PGP SIGNATURE-----

iF0EABEIAB0WIQQRaF25LwNkCi/+fKgpQKupg++CagUCXApzDgAKCRApQKupg++C
aiqMAJ9eLFYKiyyXe0nJekBdvUPfVoxYGwCfcTkOHvDPo1UFsOeh0QppYDkTnjE=
=KMyQ
-----END PGP SIGNATURE-----`

	t.Run("With valid signature and data", func(t *testing.T) {
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if strings.HasSuffix(r.RequestURI, ".asc") {
				w.Write([]byte(manifestSignature))
			} else {
				w.Write([]byte(chefSignedManifest))
			}
		}))
		defer ts.Close()

		testFmt := fmt.Sprintf("%s/%%s", ts.URL)
		client := client.NewHTTPClient(client.LatestSemanticURLFormat(testFmt))
		_, err := client.GetCurrentManifest(context.Background(), "current")
		require.NoError(t, err)
	})

	t.Run("With invalid data", func(t *testing.T) {
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if strings.HasSuffix(r.RequestURI, ".asc") {
				w.Write([]byte(manifestSignature))
			} else {
				w.Write([]byte("not valid"))
			}
		}))
		defer ts.Close()

		testFmt := fmt.Sprintf("%s/%%s", ts.URL)
		client := client.NewHTTPClient(client.LatestSemanticURLFormat(testFmt))
		_, err := client.GetCurrentManifest(context.Background(), "current")
		require.Error(t, err)
		assert.Contains(t, err.Error(), "invalid signature")
	})

	t.Run("Cannot download signature", func(t *testing.T) {
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if strings.HasSuffix(r.RequestURI, ".asc") {
				w.WriteHeader(404)
			} else {
				w.Write([]byte(chefSignedManifest))
			}
		}))
		defer ts.Close()

		testFmt := fmt.Sprintf("%s/%%s", ts.URL)
		client := client.NewHTTPClient(client.LatestSemanticURLFormat(testFmt))
		_, err := client.GetCurrentManifest(context.Background(), "current")
		require.Error(t, err)
		assert.Contains(t, err.Error(), "404 Not Found")
	})
}
