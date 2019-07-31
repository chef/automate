package depot

import (
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

func TestLocalCacheListAll(t *testing.T) {
	client := FromLocalCache(WithLocalHabRoot(("testdata/local")))
	pkgs, err := client.ListAllPackages()
	require.NoError(t, err)
	assert.ElementsMatch(t, pkgs,
		[]habpkg.HabPkg{
			habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
			habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20181019225550"),
			habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20180126213931"),
			habpkg.NewFQ("chef", "other-service", "0.1.0", "20181019225550"),
			habpkg.NewFQ("chef", "other-service", "0.1.0", "20180126213931"),
			habpkg.NewFQ("chef", "other-service2", "0.1.0", "20181019225550"),
			habpkg.NewFQ("chef", "other-service2", "0.1.0", "20180126213931"),
			habpkg.NewFQ("core", "busybox-static", "1.28.1", "20180608102729"),
			habpkg.NewFQ("core", "glibc", "2.22", "20170513201042"),
			habpkg.NewFQ("core", "linux-headers", "4.3", "20170513200956"),
		},
	)
}

func TestLocalCacheDelete(t *testing.T) {
	t.Run("deleting a non-fully-qualified package is an error", func(t *testing.T) {
		cache, _, cleanup := newTestLocalCache(t)
		defer cleanup()
		err := cache.Delete(habpkg.New("chef", "mlsa"))
		require.Error(t, err)

		err = cache.Delete(habpkg.NewFQ("chef", "mlsa", "1.0.1", ""))
		require.Error(t, err)
	})

	t.Run("deleteing a non-existent package is not an error", func(t *testing.T) {
		cache, _, cleanup := newTestLocalCache(t)
		defer cleanup()
		err := cache.Delete(habpkg.NewFQ("chef", "foobar", "1.0.1", "20181011015551"))
		require.NoError(t, err)
	})

	t.Run("deleting a package removes both the installed package and the hart", func(t *testing.T) {
		cache, tmpdir, cleanup := newTestLocalCache(t)
		defer cleanup()
		err := cache.Delete(habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"))
		require.NoError(t, err)

		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-mlsa-1.0.1-20181011015551-x86_64-linux.hart"))
		FileNotExists(t, path.Join(tmpdir, "pkgs", "chef", "mlsa", "1.0.1", "20181011015551"))

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-busybox-static-1.28.1-20180608102729-x86_64-linux.hart"))
		assert.DirExists(t, path.Join(tmpdir, "pkgs", "core", "busybox-static", "1.28.1", "20180608102729"))

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20181019225550-x86_64-linux.hart"))
		assert.DirExists(t, path.Join(tmpdir, "pkgs", "chef", "deployment-service", "0.1.0", "20181019225550"))

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20180126213931-x86_64-linux.hart"))
		assert.DirExists(t, path.Join(tmpdir, "pkgs", "chef", "deployment-service", "0.1.0", "20180126213931"))

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-linux-headers-4.3-20170513200956-x86_64-linux.hart"))
		assert.DirExists(t, path.Join(tmpdir, "pkgs", "core", "linux-headers", "4.3", "20170513200956"))

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-glibc-2.22-20170513201042-x86_64-linux.hart"))
		assert.DirExists(t, path.Join(tmpdir, "pkgs", "core", "glibc", "2.22", "20170513201042"))
	})
}

func newTestLocalCache(t *testing.T) (*LocalCache, string, func()) {
	tmpdir, err := ioutil.TempDir("", "habpkg-cache-test")
	require.NoError(t, err, "failed to create temp dir")
	cache := FromLocalCache(WithLocalHabRoot(tmpdir))

	err = exec.Command("cp", "-r", "testdata/local/pkgs", tmpdir).Run()
	err = exec.Command("cp", "-r", "testdata/local/cache", tmpdir).Run()
	require.NoError(t, err, "failed to copy fixture")
	return cache, tmpdir, func() {
		os.RemoveAll(tmpdir)
	}
}
