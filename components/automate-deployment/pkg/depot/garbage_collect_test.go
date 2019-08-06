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

func TestDisabledGarbageCollector(t *testing.T) {
	t.Run("it never does anything", func(t *testing.T) {
		gc, tmpdir, cleanup := newTestGarbageCollector(t)
		defer cleanup()

		assert.Nil(t, gc.Collect([]habpkg.HabPkg{}, "disabled"))

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-mlsa-1.0.1-20181011015551-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-busybox-static-1.28.1-20180608102729-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20181019225550-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-linux-headers-4.3-20170513200956-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-glibc-2.22-20170513201042-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20180126213931-x86_64-linux.hart"))
	})
}

func TestAggroGarbageCollector(t *testing.T) {
	t.Run("fails if no gc roots are specified", func(t *testing.T) {
		gc, _, cleanup := newTestGarbageCollector(t)
		defer cleanup()

		require.Error(t, gc.AggressiveCollect(nil))
		require.Error(t, gc.AggressiveCollect([]habpkg.HabPkg{}))
	})

	t.Run("delete test with mlsa as root", func(t *testing.T) {
		gc, tmpdir, cleanup := newTestGarbageCollector(t)
		defer cleanup()
		err := gc.AggressiveCollect([]habpkg.HabPkg{
			habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
		})
		require.NoError(t, err)

		pkgs, err := gc.cache.ListAllPackages()
		require.NoError(t, err)
		assert.ElementsMatch(t, pkgs,
			[]habpkg.HabPkg{
				habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
				habpkg.NewFQ("core", "busybox-static", "1.28.1", "20180608102729"),
			},
		)

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-mlsa-1.0.1-20181011015551-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-busybox-static-1.28.1-20180608102729-x86_64-linux.hart"))
		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20181019225550-x86_64-linux.hart"))
		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "core-linux-headers-4.3-20170513200956-x86_64-linux.hart"))
		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "core-glibc-2.22-20170513201042-x86_64-linux.hart"))
	})

	t.Run("doesn't delete tdeps of root packages but does delete older versions", func(t *testing.T) {
		gc, tmpdir, cleanup := newTestGarbageCollector(t)
		defer cleanup()
		err := gc.AggressiveCollect([]habpkg.HabPkg{
			habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
			habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20181019225550"),
		})
		require.NoError(t, err)

		pkgs, err := gc.cache.ListAllPackages()
		require.NoError(t, err)
		assert.ElementsMatch(t, pkgs,
			[]habpkg.HabPkg{
				habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
				habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20181019225550"),
				habpkg.NewFQ("core", "busybox-static", "1.28.1", "20180608102729"),
				habpkg.NewFQ("core", "glibc", "2.22", "20170513201042"),
				habpkg.NewFQ("core", "linux-headers", "4.3", "20170513200956"),
			},
		)

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-mlsa-1.0.1-20181011015551-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-busybox-static-1.28.1-20180608102729-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20181019225550-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-linux-headers-4.3-20170513200956-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-glibc-2.22-20170513201042-x86_64-linux.hart"))

		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20180126213931-x86_64-linux.hart"))
	})

	t.Run("delete test with linux-headers as root", func(t *testing.T) {
		gc, tmpdir, cleanup := newTestGarbageCollector(t)
		defer cleanup()
		err := gc.AggressiveCollect([]habpkg.HabPkg{
			habpkg.NewFQ("core", "linux-headers", "4.3", "20170513200956"),
		})
		require.NoError(t, err)

		pkgs, err := gc.cache.ListAllPackages()
		require.NoError(t, err)
		assert.ElementsMatch(t, pkgs,
			[]habpkg.HabPkg{
				habpkg.NewFQ("core", "linux-headers", "4.3", "20170513200956"),
			},
		)
		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-mlsa-1.0.1-20181011015551-x86_64-linux.hart"))
		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "core-busybox-static-1.28.1-20180608102729-x86_64-linux.hart"))
		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20181019225550-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-linux-headers-4.3-20170513200956-x86_64-linux.hart"))
		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "core-glibc-2.22-20170513201042-x86_64-linux.hart"))
	})
}

func TestConservativeGarbageCollector(t *testing.T) {
	t.Run("does nothing if  no root packages are specified", func(t *testing.T) {
		gc, _, cleanup := newTestGarbageCollector(t)
		defer cleanup()

		require.NoError(t, gc.ConservativeCollect([]habpkg.HabPkg{}))
	})

	t.Run("does not delete packages that are not different versions of the given roots", func(t *testing.T) {
		gc, tmpdir, cleanup := newTestGarbageCollector(t)
		defer cleanup()
		err := gc.ConservativeCollect([]habpkg.HabPkg{
			habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
		})
		require.NoError(t, err)

		pkgs, err := gc.cache.ListAllPackages()
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

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-mlsa-1.0.1-20181011015551-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-busybox-static-1.28.1-20180608102729-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20181019225550-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20180126213931-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-linux-headers-4.3-20170513200956-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-glibc-2.22-20170513201042-x86_64-linux.hart"))
	})

	t.Run("deletes packages that are different versions of the given roots unless they are dependencies of another root", func(t *testing.T) {
		gc, tmpdir, cleanup := newTestGarbageCollector(t)
		defer cleanup()
		err := gc.ConservativeCollect([]habpkg.HabPkg{
			habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
			habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20181019225550"),
			habpkg.NewFQ("chef", "other-service", "0.1.0", "20181019225550"),
			habpkg.NewFQ("chef", "other-service2", "0.1.0", "20181019225550"),
		})
		require.NoError(t, err)

		pkgs, err := gc.cache.ListAllPackages()
		require.NoError(t, err)
		assert.ElementsMatch(t, pkgs,
			[]habpkg.HabPkg{
				habpkg.NewFQ("chef", "mlsa", "1.0.1", "20181011015551"),
				habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20181019225550"),
				habpkg.NewFQ("chef", "other-service", "0.1.0", "20181019225550"),
				// We expect both versions of other-service2 to be available because
				// - other-service2/0.1.0/20180126213931 is a dependency of other-service/0.1.0/20181019225550
				// - other-service2/0.1.0/20181019225550 is in our roots
				habpkg.NewFQ("chef", "other-service2", "0.1.0", "20181019225550"),
				habpkg.NewFQ("chef", "other-service2", "0.1.0", "20180126213931"),
				habpkg.NewFQ("core", "busybox-static", "1.28.1", "20180608102729"),
				habpkg.NewFQ("core", "glibc", "2.22", "20170513201042"),
				habpkg.NewFQ("core", "linux-headers", "4.3", "20170513200956"),
			},
		)

		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-mlsa-1.0.1-20181011015551-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-busybox-static-1.28.1-20180608102729-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20181019225550-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-linux-headers-4.3-20170513200956-x86_64-linux.hart"))
		assert.FileExists(t, path.Join(tmpdir, "cache", "artifacts", "core-glibc-2.22-20170513201042-x86_64-linux.hart"))

		FileNotExists(t, path.Join(tmpdir, "cache", "artifacts", "chef-deployment-service-0.1.0-20180126213931-x86_64-linux.hart"))
	})

}
func newTestGarbageCollector(t *testing.T) (*GarbageCollector, string, func()) {
	tmpdir, err := ioutil.TempDir("", "habpkg-gc-test")
	require.NoError(t, err, "failed to create temp dir")
	cache := FromLocalCache(WithLocalHabRoot(tmpdir))

	err = exec.Command("cp", "-r", "testdata/local/pkgs", tmpdir).Run()
	err = exec.Command("cp", "-r", "testdata/local/cache", tmpdir).Run()
	require.NoError(t, err, "failed to copy fixture")
	return NewGarbageCollector(cache), tmpdir, func() {
		os.RemoveAll(tmpdir)
	}
}

func FileNotExists(t *testing.T, path string) {
	_, err := os.Lstat(path)
	if err != nil {
		if os.IsNotExist(err) {
			return
		}
		t.Fatalf("error when running os.Lstat(%q): %s", path, err)
		return
	}
	t.Fatalf("File %s was found", path)
}
