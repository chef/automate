package fileutils_test

import (
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
)

func TestCopyFile(t *testing.T) {
	t.Run("it copies the file", func(t *testing.T) {
		d1, d2, cleanup := setupCopy(t)
		defer cleanup()

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
		d1, d2, cleanup := setupCopy(t)
		defer cleanup()

		srcData := []byte("src data")
		srcPath := path.Join(d1, "src")
		dstPath := path.Join(d2, "dst")
		dstData := []byte("dst data")
		require.NoError(t, ioutil.WriteFile(srcPath, srcData, 0700))
		require.NoError(t, ioutil.WriteFile(dstPath, dstData, 0700))
		require.Error(t, fileutils.CopyFile(srcPath, dstPath))
	})

	t.Run("it copies over existing file when overwrite is set", func(t *testing.T) {
		d1, d2, cleanup := setupCopy(t)
		defer cleanup()

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

func setupCopy(t *testing.T) (string, string, func()) {
	tmpDir, err := ioutil.TempDir("", "TestCopy1")
	if err != nil {
		t.Fatal("creating temp dir")
		os.RemoveAll(tmpDir)
	}

	tmpDir2, err := ioutil.TempDir("", "TestCopy2")
	if err != nil {
		t.Fatal("creating temp dir")
		os.RemoveAll(tmpDir2)
	}

	return tmpDir, tmpDir2, func() {
		_ = os.RemoveAll(tmpDir)
		_ = os.RemoveAll(tmpDir2)
	}
}
