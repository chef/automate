package fileutils_test

import (
	"io/ioutil"
	"path"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
)

func TestCopyFile(t *testing.T) {
	t.Run("it copies the file", func(t *testing.T) {
		d1, d2 := setupCopy(t)

		srcData := []byte("test data")
		srcPath := path.Join(d1, "src")
		dstPath := path.Join(d2, "dst")
		require.NoError(t, ioutil.WriteFile(srcPath, srcData, 0700))
		require.NoError(t, fileutils.CopyFile(srcPath, dstPath))
		dstData, err := ioutil.ReadFile(dstPath)
		require.NoError(t, err)
		require.Equal(t, srcData, dstData)
	})

	t.Run("it fails when file exists", func(t *testing.T) {
		d1, d2 := setupCopy(t)

		srcData := []byte("src data")
		srcPath := path.Join(d1, "src")
		dstPath := path.Join(d2, "dst")
		dstData := []byte("dst data")
		require.NoError(t, ioutil.WriteFile(srcPath, srcData, 0700))
		require.NoError(t, ioutil.WriteFile(dstPath, dstData, 0700))
		require.Error(t, fileutils.CopyFile(srcPath, dstPath))
	})

	t.Run("it copies over existing file when overwrite is set", func(t *testing.T) {
		d1, d2 := setupCopy(t)

		srcData := []byte("src data")
		srcPath := path.Join(d1, "src")
		dstPath := path.Join(d2, "dst")
		dstData := []byte("dst data")
		require.NoError(t, ioutil.WriteFile(srcPath, srcData, 0700))
		require.NoError(t, ioutil.WriteFile(dstPath, dstData, 0700))
		require.NoError(t, fileutils.CopyFile(srcPath, dstPath, fileutils.Overwrite()))
		dstData, err := ioutil.ReadFile(dstPath)
		require.NoError(t, err)
		require.Equal(t, srcData, dstData)
	})
}

func setupCopy(t *testing.T) (string, string) {
	tmpDir := t.TempDir()
	tmpDir2 := t.TempDir()

	return tmpDir, tmpDir2
}
