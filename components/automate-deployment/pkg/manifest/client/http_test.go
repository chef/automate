package client_test

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
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

func TestGetCurrentManifest(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, goodV1Manifest)
	}))
	defer ts.Close()

	testFmt := fmt.Sprintf("%s/%%s", ts.URL)
	client := client.NewHTTPClient(client.LatestURLFormat(testFmt))
	manifest, err := client.GetCurrentManifest(context.Background(), "current")
	require.NoError(t, err)
	assert.Equal(t, 1, len(manifest.Packages))

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
	client := client.NewHTTPClient(client.URLFormat(testFmt))
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
	client := client.NewHTTPClient(client.URLFormat(testFmt))
	_, err := client.GetManifest(context.Background(), "20180207073355")
	require.Equal(t, err, manifest.NoSuchManifest)
}
