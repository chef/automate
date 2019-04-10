package airgap

import (
	"io"
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFileCache(t *testing.T) {
	tmpdir, err := ioutil.TempDir("", "a2-install-bundle-test")
	require.NoError(t, err)

	defer os.RemoveAll(tmpdir)
	cacher := fileCacher{cacheDir: tmpdir}
	t.Run("non existing file is not cached", func(t *testing.T) {
		assert.False(t, cacher.IsCached("does-not-exist"))
	})

	t.Run("caching a file works", func(t *testing.T) {
		err := cacher.CacheFile("foo", func(w io.Writer) error {
			_, err := w.Write([]byte("hello"))
			return err
		})
		require.NoError(t, err)
		assert.True(t, cacher.IsCached("foo"))
		data, err := ioutil.ReadFile(path.Join(tmpdir, "foo"))
		require.NoError(t, err)
		require.Equal(t, []byte("hello"), data)
	})

	t.Run("the file does not exist if there was an error", func(t *testing.T) {
		err := cacher.CacheFile("bar", func(w io.Writer) error {
			_, err := w.Write([]byte("hello"))
			require.NoError(t, err)
			return errors.New("Test error")
		})
		assert.Error(t, err)
		assert.False(t, cacher.IsCached("bar"))
	})
}

func TestKeyCache(t *testing.T) {
	tmpdir, err := ioutil.TempDir("", "a2-install-bundle-test")
	require.NoError(t, err)

	defer os.RemoveAll(tmpdir)

	keyCache := NewKeyCache(tmpdir)

	t.Run("non existing file is not cached", func(t *testing.T) {
		assert.False(t, keyCache.IsCached("does-not-exist"))
	})

	t.Run("caching a key works", func(t *testing.T) {
		err := keyCache.CacheKey("foo", func(w io.Writer) error {
			_, err := w.Write([]byte("key data"))
			return err
		})
		require.NoError(t, err)
		assert.True(t, keyCache.IsCached("foo"))
		data, err := ioutil.ReadFile(path.Join(tmpdir, "foo.pub"))
		require.NoError(t, err)
		require.Equal(t, []byte("key data"), data)
	})
}

func TestHartifactCache(t *testing.T) {
	tmpdir, err := ioutil.TempDir("", "a2-install-bundle-test")
	require.NoError(t, err)

	defer os.RemoveAll(tmpdir)

	hartCache := NewHartifactCache(tmpdir)

	t.Run("non existing file is not cached", func(t *testing.T) {
		pkg := habpkg.NewFQ("core", "does-not-exist", "0.1.0", "20180101000000")
		assert.False(t, hartCache.IsCached(&pkg))
	})

	t.Run("caching a key works", func(t *testing.T) {
		pkg := habpkg.NewFQ("core", "foo", "0.1.0", "20180101000000")
		err := hartCache.CacheArtifact(&pkg, func(w io.Writer) error {
			_, err := w.Write([]byte("data"))
			return err
		})
		require.NoError(t, err)
		assert.True(t, hartCache.IsCached(&pkg))
		data, err := ioutil.ReadFile(path.Join(tmpdir, "core-foo-0.1.0-20180101000000-x86_64-linux.hart"))
		require.NoError(t, err)
		require.Equal(t, []byte("data"), data)
	})
}
