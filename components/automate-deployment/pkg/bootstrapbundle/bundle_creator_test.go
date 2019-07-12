package bootstrapbundle

import (
	"bytes"
	"io/ioutil"
	"os"
	"os/user"
	"path"
	"path/filepath"
	"strconv"
	"syscall"
	"testing"

	"github.com/chef/automate/lib/product"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func createBundleCreator(t *testing.T) *Creator {
	t.Helper()

	allowedUsers := []string{}
	allowedGroups := []string{}

	uids := make(map[uint32]bool)
	gids := make(map[uint32]bool)
	filepath.Walk("testdata/bootstrap-test", func(path string, info os.FileInfo, err error) error {
		stat := info.Sys().(*syscall.Stat_t)

		uids[stat.Uid] = true
		gids[stat.Gid] = true
		return nil
	})

	for uid := range uids {
		u, err := user.LookupId(strconv.Itoa(int(uid)))
		require.NoError(t, err)
		allowedUsers = append(allowedUsers, u.Username)
	}

	for gid := range gids {
		g, err := user.LookupGroupId(strconv.Itoa(int(gid)))
		require.NoError(t, err)
		allowedGroups = append(allowedGroups, g.Name)
	}

	return &Creator{
		rootDir:       "testdata/bootstrap-test",
		allowedUsers:  allowedUsers,
		allowedGroups: allowedGroups,
	}
}

func TestFailOnMissingFile(t *testing.T) {
	bundleCreator := createBundleCreator(t)

	metadata := []*product.PackageMetadata{
		{
			Name: product.PackageName{
				Origin: "origin",
				Name:   "foo",
			},
			Bootstrap: []product.BootstrapSpec{
				{
					Type: "file",
					Path: "data/barfile",
				},
			},
		},
	}

	b := bytes.NewBuffer(nil)
	err := bundleCreator.Create(metadata, b)
	require.Error(t, err)
}

func TestFailOnNoFiles(t *testing.T) {
	bundleCreator := createBundleCreator(t)

	metadata := []*product.PackageMetadata{
		{
			Name: product.PackageName{
				Origin: "origin",
				Name:   "foo",
			},
		},
	}

	b := bytes.NewBuffer(nil)
	err := bundleCreator.Create(metadata, b)
	require.Equal(t, ErrNoFiles, err)
}

func TestAllowMissingBootstrapSpec(t *testing.T) {
	bundleCreator := createBundleCreator(t)

	metadata := []*product.PackageMetadata{
		{
			Name: product.PackageName{
				Origin: "origin",
				Name:   "bar",
			},
		},
		{
			Name: product.PackageName{
				Origin: "origin",
				Name:   "foo",
			},
			Bootstrap: []product.BootstrapSpec{
				{
					Type: "file",
					Path: "data/foofile",
				},
			},
		},
	}

	b := bytes.NewBuffer(nil)
	err := bundleCreator.Create(metadata, b)
	require.NoError(t, err)
}

func TestRoundTrip(t *testing.T) {
	tmpdir, err := ioutil.TempDir("", "a2-bootstrap-bundle-test")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)

	bundleCreator := createBundleCreator(t)

	bundleUnpacker := &Creator{
		rootDir:       tmpdir,
		allowedUsers:  bundleCreator.allowedUsers,
		allowedGroups: bundleCreator.allowedGroups,
	}

	metadata := []*product.PackageMetadata{
		{
			Name: product.PackageName{
				Origin: "origin",
				Name:   "foo",
			},
			Bootstrap: []product.BootstrapSpec{
				{
					Type: "file",
					Path: "data/foofile",
				},
			},
		},
		{
			Name: product.PackageName{
				Origin: "origin",
				Name:   "bar",
			},
		},
	}

	b := bytes.NewBuffer(nil)
	err = bundleCreator.Create(metadata, b)
	require.NoError(t, err)

	err = bundleUnpacker.Unpack(b)
	require.NoError(t, err)

	extractedFilePath := path.Join(tmpdir, "foo/data/foofile")
	require.FileExists(t, extractedFilePath)
	actualData, err := ioutil.ReadFile("testdata/bootstrap-test/foo/data/foofile")
	require.NoError(t, err)
	expectedData, err := ioutil.ReadFile("testdata/bootstrap-test/foo/data/foofile")
	require.NoError(t, err)
	assert.Equal(t, expectedData, actualData)
}
