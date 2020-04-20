package fileutils_test

import (
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
)

func TestCopyDir(t *testing.T) {
	t.Run("it fails when src directory isn't a directory", func(t *testing.T) {
		tmpDir, err := ioutil.TempDir("", "TestCopyDir")
		defer os.RemoveAll(tmpDir)
		require.NoError(t, err)

		srcData := []byte("src data")
		srcPath := path.Join(tmpDir, "src")
		dstPath := path.Join(tmpDir, "dst")
		require.NoError(t, ioutil.WriteFile(srcPath, srcData, 0700))

		require.Error(t, fileutils.CopyDir(srcPath, dstPath))
	})

	t.Run("it recursively copies files", func(t *testing.T) {
		tmpDir, err := ioutil.TempDir("", "TestCopyDir")
		defer os.RemoveAll(tmpDir)
		require.NoError(t, err)

		srcDir := filepath.Join(tmpDir, "src")
		dstDir := filepath.Join(tmpDir, "dst")

		file1Path := filepath.Join(srcDir, "file1")
		file2Path := filepath.Join(srcDir, "nest1", "file2")
		file3Path := filepath.Join(srcDir, "nest1", "nest2", "file3")
		file1PathDst := filepath.Join(dstDir, "file1")
		file2PathDst := filepath.Join(dstDir, "nest1", "file2")
		file3PathDst := filepath.Join(dstDir, "nest1", "nest2", "file3")
		file1Data := []byte("file1")
		file2Data := []byte("file2")
		file3Data := []byte("file3")
		require.NoError(t, os.MkdirAll(filepath.Dir(file3Path), 0700))
		require.NoError(t, ioutil.WriteFile(file1Path, file1Data, 0700))
		require.NoError(t, ioutil.WriteFile(file2Path, file2Data, 0700))
		require.NoError(t, ioutil.WriteFile(file3Path, file3Data, 0700))

		require.NoError(t, fileutils.CopyDir(srcDir, dstDir))
		file1DataDst, err := ioutil.ReadFile(file1PathDst)
		require.NoError(t, err)
		file2DataDst, err := ioutil.ReadFile(file2PathDst)
		require.NoError(t, err)
		file3DataDst, err := ioutil.ReadFile(file3PathDst)
		require.NoError(t, err)

		require.Equal(t, file1Data, file1DataDst)
		require.Equal(t, file2Data, file2DataDst)
		require.Equal(t, file3Data, file3DataDst)
	})

	t.Run("it recursively copies directories to given depth", func(t *testing.T) {
		tmpDir, err := ioutil.TempDir("", "TestCopyDir")
		defer os.RemoveAll(tmpDir)
		require.NoError(t, err)

		srcDir := filepath.Join(tmpDir, "src")
		dstDir := filepath.Join(tmpDir, "dst")

		file1Path := filepath.Join(srcDir, "file1")
		file2Path := filepath.Join(srcDir, "nest1", "file2")
		file3Path := filepath.Join(srcDir, "nest1", "nest2", "file3")
		file1PathDst := filepath.Join(dstDir, "file1")
		file2PathDst := filepath.Join(dstDir, "nest1", "file2")
		file3PathDst := filepath.Join(dstDir, "nest1", "nest2", "file3")
		file1Data := []byte("file1")
		file2Data := []byte("file2")
		file3Data := []byte("file3")
		require.NoError(t, os.MkdirAll(filepath.Dir(file3Path), 0700))
		require.NoError(t, ioutil.WriteFile(file1Path, file1Data, 0700))
		require.NoError(t, ioutil.WriteFile(file2Path, file2Data, 0700))
		require.NoError(t, ioutil.WriteFile(file3Path, file3Data, 0700))

		require.NoError(t, fileutils.CopyDir(srcDir, dstDir, fileutils.Depth(1)))
		file1DataDst, err := ioutil.ReadFile(file1PathDst)
		require.NoError(t, err)
		file2DataDst, err := ioutil.ReadFile(file2PathDst)
		require.NoError(t, err)
		_, err = ioutil.ReadFile(file3PathDst)
		require.Error(t, err)

		require.Equal(t, file1Data, file1DataDst)
		require.Equal(t, file2Data, file2DataDst)
	})

	t.Run("it doesn't recursively copy with no recursive set", func(t *testing.T) {
		tmpDir, err := ioutil.TempDir("", "TestCopyDir")
		defer os.RemoveAll(tmpDir)
		require.NoError(t, err)

		srcDir := filepath.Join(tmpDir, "src")
		dstDir := filepath.Join(tmpDir, "dst")

		file1Path := filepath.Join(srcDir, "file1")
		file2Path := filepath.Join(srcDir, "nest1", "file2")
		file1PathDst := filepath.Join(dstDir, "file1")
		file2PathDst := filepath.Join(dstDir, "nest1", "file2")
		file1Data := []byte("file1")
		file2Data := []byte("file2")
		require.NoError(t, os.MkdirAll(filepath.Dir(file2Path), 0700))
		require.NoError(t, ioutil.WriteFile(file1Path, file1Data, 0700))
		require.NoError(t, ioutil.WriteFile(file2Path, file2Data, 0700))

		require.NoError(t, fileutils.CopyDir(srcDir, dstDir, fileutils.NoRecursive()))
		file1DataDst, err := ioutil.ReadFile(file1PathDst)
		require.NoError(t, err)
		_, err = ioutil.ReadFile(file2PathDst)
		require.Error(t, err)
		require.Equal(t, file1Data, file1DataDst)
	})

	t.Run("it fails when the dest directory exists", func(t *testing.T) {
		tmpDir, err := ioutil.TempDir("", "TestCopyDir")
		defer os.RemoveAll(tmpDir)
		require.NoError(t, err)

		srcDir := filepath.Join(tmpDir, "src")
		dstDir := filepath.Join(tmpDir, "dst")

		require.NoError(t, os.Mkdir(srcDir, 0700))
		require.NoError(t, os.Mkdir(dstDir, 0700))

		require.Error(t, fileutils.CopyDir(srcDir, dstDir))
	})

	t.Run("it overwrites dest files when overwrite is set", func(t *testing.T) {
		tmpDir, err := ioutil.TempDir("", "TestCopyDir")
		defer os.RemoveAll(tmpDir)
		require.NoError(t, err)

		srcDir := filepath.Join(tmpDir, "src")
		dstDir := filepath.Join(tmpDir, "dst")

		file1Path := filepath.Join(srcDir, "file1")
		file2Path := filepath.Join(srcDir, "nest1", "file2")
		file1PathDst := filepath.Join(dstDir, "file1")
		file2PathDst := filepath.Join(dstDir, "nest1", "file2")
		file1Data := []byte("file1")
		file2Data := []byte("file2")
		file1DataDst := []byte("file1Dst")
		file2DataDst := []byte("file2Dst")
		require.NoError(t, os.MkdirAll(filepath.Dir(file2Path), 0700))
		require.NoError(t, os.MkdirAll(filepath.Dir(file2PathDst), 0700))
		require.NoError(t, ioutil.WriteFile(file1Path, file1Data, 0700))
		require.NoError(t, ioutil.WriteFile(file2Path, file2Data, 0700))
		require.NoError(t, ioutil.WriteFile(file1PathDst, file1DataDst, 0700))
		require.NoError(t, ioutil.WriteFile(file2PathDst, file2DataDst, 0700))

		require.NoError(t, fileutils.CopyDir(srcDir, dstDir, fileutils.Overwrite()))

		file1DataDst, err = ioutil.ReadFile(file1PathDst)
		require.NoError(t, err)
		file2DataDst, err = ioutil.ReadFile(file2PathDst)
		require.NoError(t, err)

		require.Equal(t, file1Data, file1DataDst)
		require.Equal(t, file2Data, file2DataDst)
	})
}
