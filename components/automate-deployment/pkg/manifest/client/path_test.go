package client_test

import (
	"context"
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

var goodA2Manifest = `
{
  "build": "20180716213113",
  "build_sha": "844b78fb80ca19c6a555de34d24e3e9b5a56ce06",
  "packages": [
	"chef/automate-cli/0.1.0/20180716191251",
	"chef/automate-cs-bookshelf/12.17.58/20180709232852"
  ],
  "hart_overrides": [
    "chef/automate-cli:/src/results/chef-automate-cli-0.1.0-20180713011157-x86_64-linux.hart"
  ]
}`

func TestDirectoryGetCurrentManifest(t *testing.T) {
	dir, err := ioutil.TempDir("", "DirectoryManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)

	err = ioutil.WriteFile(path.Join(dir, "current.json"), []byte(goodV1Manifest), 0700)
	require.NoError(t, err, "writing test data")

	client := client.NewPathClient(dir)
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

func TestDirectoryGetCurrentManifestWhenDoesntExist(t *testing.T) {
	dir, err := ioutil.TempDir("", "DirectoryManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)

	client := client.NewPathClient(dir)
	_, err = client.GetCurrentManifest(context.Background(), "current")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "failed to read manifest at path")
}

func TestDirectoryGetCurrentManifestWhenInvalid(t *testing.T) {
	dir, err := ioutil.TempDir("", "DirectoryManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)

	err = ioutil.WriteFile(path.Join(dir, "current.json"), []byte("::::::"), 0700)
	require.NoError(t, err, "writing test data")

	client := client.NewPathClient(dir)
	_, err = client.GetCurrentManifest(context.Background(), "current")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "failed to parse manifest loaded from")
}

func TestFileGetCurrentManifestA2(t *testing.T) {
	dir, err := ioutil.TempDir("", "FileManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)
	filename := path.Join(dir, "manifest.json")
	client := client.NewPathClient(filename)

	err = ioutil.WriteFile(filename, []byte(goodA2Manifest), 0700)
	require.NoError(t, err, "writing test data")

	manifest, err := client.GetCurrentManifest(context.Background(), "current")
	require.NoError(t, err)
	assert.Equal(t, 2, len(manifest.Packages))
	assert.Equal(t, 1, len(manifest.HartOverrides))
	assert.Equal(t, "20180716213113", manifest.Build)
	assert.Equal(t, "844b78fb80ca19c6a555de34d24e3e9b5a56ce06", manifest.BuildSHA)

	found, pkg := manifest.PackageForServiceName("automate-cs-bookshelf")
	require.True(t, found)
	assert.Equal(t, "automate-cs-bookshelf", pkg.Name())
	assert.Equal(t, "chef", pkg.Origin())
	assert.Equal(t, "12.17.58", pkg.Version())
	assert.Equal(t, "20180709232852", pkg.Release())

	found, pkg = manifest.PackageForServiceName("automate-cli")
	require.True(t, found)
	assert.Equal(t, "automate-cli", pkg.Name())
	assert.Equal(t, "chef", pkg.Origin())
	assert.Equal(t, "0.1.0", pkg.Version())
	assert.Equal(t, "20180716191251", pkg.Release())

	found, hart := manifest.HartForServiceName("automate-cli")
	require.True(t, found)
	assert.Equal(t, "automate-cli", hart.Name())
	assert.Equal(t, "chef", hart.Origin())
	assert.Equal(t, "0.1.0", hart.Version())
	assert.Equal(t, "20180713011157", hart.Release())
}

func TestFileGetCurrentManifestV1(t *testing.T) {
	dir, err := ioutil.TempDir("", "FileManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)
	filename := path.Join(dir, "manifest.json")
	client := client.NewPathClient(filename)

	err = ioutil.WriteFile(filename, []byte(goodV1Manifest), 0700)
	require.NoError(t, err, "writing test data")

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

func TestFileGetCurrentManifestWhenDoesntExist(t *testing.T) {
	dir, err := ioutil.TempDir("", "FileManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)
	filename := path.Join(dir, "manifest.json")
	client := client.NewPathClient(filename)
	_, err = client.GetCurrentManifest(context.Background(), "current")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "failed to read manifest")
}

func TestFileGetCurrentManifestWhenInvalid(t *testing.T) {
	dir, err := ioutil.TempDir("", "FileManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)

	filename := path.Join(dir, "manifest.json")
	client := client.NewPathClient(filename)

	err = ioutil.WriteFile(filename, []byte("::::::"), 0700)
	_, err = client.GetCurrentManifest(context.Background(), "current")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "failed to parse manifest loaded from")
}

func TestFileGetManifest(t *testing.T) {
	dir, err := ioutil.TempDir("", "DirectoryManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)

	filename := path.Join(dir, "manifest.json")
	err = ioutil.WriteFile(filename, []byte(goodV1Manifest), 0700)
	require.NoError(t, err, "writing test data")

	client := client.NewPathClient(filename)
	manifest, err := client.GetManifest(context.Background(), "20180207073125")
	require.NoError(t, err)
	assert.Equal(t, 1, len(manifest.Packages))

	found, pkg := manifest.PackageForServiceName("teams-service")
	require.True(t, found)
	assert.Equal(t, "teams-service", pkg.Name())
	assert.Equal(t, "chef", pkg.Origin())
	assert.Equal(t, "0.1.0", pkg.Version())
	assert.Equal(t, "20180207073125", pkg.Release())
}

func TestDirectoryGetManifest(t *testing.T) {
	dir, err := ioutil.TempDir("", "DirectoryManifestProviderTest")
	require.NoError(t, err, "creating temporary manifest dir")
	defer os.RemoveAll(dir)

	err = ioutil.WriteFile(path.Join(dir, "20180207073125.json"), []byte(goodV1Manifest), 0700)
	require.NoError(t, err, "writing test data")

	client := client.NewPathClient(dir)
	manifest, err := client.GetManifest(context.Background(), "20180207073125")
	require.NoError(t, err)
	assert.Equal(t, 1, len(manifest.Packages))

	found, pkg := manifest.PackageForServiceName("teams-service")
	require.True(t, found)
	assert.Equal(t, "teams-service", pkg.Name())
	assert.Equal(t, "chef", pkg.Origin())
	assert.Equal(t, "0.1.0", pkg.Version())
	assert.Equal(t, "20180207073125", pkg.Release())
}
