package fileutils_test

import (
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
)

func TestPathExist(t *testing.T) {
	t.Run("it returns true if the path exists", func(t *testing.T) {
		tmpdir, err := ioutil.TempDir("", "FileUtilsTestDir")
		require.NoError(t, err)
		defer os.RemoveAll(tmpdir)

		res, err := fileutils.PathExists(tmpdir)
		require.NoError(t, err)
		assert.True(t, res)
	})

	t.Run("it returns false if the path doesn't exist", func(t *testing.T) {
		res, err := fileutils.PathExists("/path/that/really/should/never/exist")
		require.NoError(t, err)
		assert.False(t, res)
	})
}

func TestIsSymlink(t *testing.T) {
	tmpdir, err := ioutil.TempDir("", "FileUtilsTestDir")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)

	testData := []byte("test data")
	filePath := path.Join(tmpdir, "file")
	ioutil.WriteFile(filePath, testData, 0700)
	symlinkPath := path.Join(tmpdir, "symlink")
	err = os.Symlink(filePath, symlinkPath)
	require.NoError(t, err)

	t.Run("it returns true for a symlink", func(t *testing.T) {
		isSymlink, err := fileutils.IsSymlink(symlinkPath)
		require.NoError(t, err)
		assert.True(t, isSymlink, "%s should be a symlink", symlinkPath)
	})

	t.Run("it returns false for an ordinary file", func(t *testing.T) {
		isSymlink, err := fileutils.IsSymlink(filePath)
		require.NoError(t, err)
		assert.False(t, isSymlink, "%s should not be a symlink", filePath)
	})
}
