package a1upgrade_test

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
)

func TestIsEmptyDir(t *testing.T) {
	t.Run("it returns true if the path is an empty directory", func(t *testing.T) {
		tmpdir, err := ioutil.TempDir("", "FileUtilsTestDir")
		require.NoError(t, err)
		defer os.RemoveAll(tmpdir)

		res, err := a1upgrade.IsEmptyDir(tmpdir)
		require.NoError(t, err)
		assert.True(t, res)
	})

	t.Run("it returns false if the path is a nonempty directory", func(t *testing.T) {
		tmpdir, err := ioutil.TempDir("", "FileutilsTestDir")
		require.NoError(t, err)
		defer os.RemoveAll(tmpdir)
		ioutil.WriteFile(filepath.Join(tmpdir, "testfile"), []byte("test context"), os.ModePerm)

		res, err := a1upgrade.IsEmptyDir(tmpdir)
		require.NoError(t, err)
		assert.False(t, res)
	})

	t.Run("it returns false if the path isn't a directory", func(t *testing.T) {
		tmpfile, err := ioutil.TempFile("", "FileUtilsTestSrc")
		require.NoError(t, err)
		defer os.Remove(tmpfile.Name())

		res, err := a1upgrade.IsEmptyDir(tmpfile.Name())
		require.NoError(t, err)
		assert.False(t, res)
	})

	t.Run("it returns false if the path doesn't exist", func(t *testing.T) {
		res, err := a1upgrade.IsEmptyDir("/path/that/really/should/never/exist")
		require.NoError(t, err)
		assert.False(t, res)
	})
}
