package a1upgrade

import (
	"bytes"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

func pathExists(path string) bool {
	_, err := os.Stat(path)
	return !os.IsNotExist(err)
}

var myGroup = strconv.FormatInt(int64(os.Getgid()), 10)
var myUser = strconv.FormatInt(int64(os.Geteuid()), 10)
var testServiceName = "test-service"

func TestResumeMove(t *testing.T) {
	setup := func(opts ...FileMoverOpt) (*FileMover, string, string, func()) {
		tmpSrc, _ := ioutil.TempDir("", "FileMoverTestSrc")
		tmpDst, _ := ioutil.TempDir("", "FileMoverTestDst")
		srcPath := filepath.Join(tmpSrc, "some_dir")
		dstPath := "data/some_dir"
		mover := NewFileMover(srcPath, testServiceName, dstPath, opts...)
		mover.User = myUser
		mover.Group = myGroup
		mover.habBasePath = tmpDst

		os.MkdirAll(srcPath, os.ModePerm)
		require.NoError(t, ioutil.WriteFile(filepath.Join(srcPath, "testfile"), []byte("test context"), os.ModePerm))
		return mover, srcPath, path.Join(tmpDst, testServiceName, dstPath), func() {
			os.RemoveAll(tmpSrc)
			os.RemoveAll(tmpDst)
		}
	}

	w := cli.NewWriter(os.Stdout, os.Stderr, new(bytes.Buffer))

	t.Run("it renames the src directory to the new directory when possible", func(t *testing.T) {
		mover, src, dst, cleanup := setup()
		defer cleanup()

		err := mover.Move(w)
		require.NoError(t, err)
		assert.DirExists(t, dst)
		assert.False(t, pathExists(src))
	})

	t.Run("it rsyncs the src directory to the new directory if new directory exists", func(t *testing.T) {
		_, err := exec.LookPath("rsync")
		if err != nil {
			t.Fatal("rsync does not exist, cannot run rsync test")
		}
		mover, src, dst, cleanup := setup()
		defer cleanup()
		os.MkdirAll(dst, os.ModePerm)

		mover.RsyncCmd = "rsync"
		err = mover.Move(w)
		require.NoError(t, err)
		assert.True(t, pathExists(filepath.Join(dst, "testfile")))
		assert.True(t, pathExists(src)) // We don't currently remove files if we used rsync
	})

	t.Run("it rsyncs the src file if we request copy only", func(t *testing.T) {
		_, err := exec.LookPath("rsync")
		if err != nil {
			t.Fatal("rsync does not exist, cannot run rsync test")
		}
		mover, src, dst, cleanup := setup(ForceCopy())
		defer cleanup()
		os.MkdirAll(dst, os.ModePerm)
		mover.SrcPath = path.Join(mover.SrcPath, "testfile")
		mover.RelDestPath = path.Join(mover.RelDestPath, "testfile")
		mover.RsyncCmd = "rsync"
		err = mover.Move(w)
		require.NoError(t, err)
		assert.True(t, pathExists(filepath.Join(src, "testfile")))
		assert.True(t, pathExists(filepath.Join(dst, "testfile")))
		assert.True(t, pathExists(src)) // We don't currently remove files if we used rsync
	})

	t.Run("it doesn't move the directory again if the move is already complete", func(t *testing.T) {
		mover, src, dst, cleanup := setup()
		defer cleanup()

		err := mover.Move(w)
		require.NoError(t, err)

		newSrcFile := filepath.Join(src, "new-source-file")
		err = os.MkdirAll(src, os.ModePerm)
		require.NoError(t, err)
		require.NoError(t, ioutil.WriteFile(newSrcFile, []byte("test context"), os.ModePerm))

		newDstFile := filepath.Join(dst, "new-dest-file")
		require.NoError(t, ioutil.WriteFile(newDstFile, []byte("test context"), os.ModePerm))

		err = mover.Move(w)
		require.NoError(t, err)
		assert.True(t, pathExists(dst))
		assert.True(t, pathExists(newSrcFile), "new-source-file should still exist in src directory after no-op second move")
		assert.True(t, pathExists(newDstFile), "new-dest-file should still exist in dst directory after no-op second move")
		assert.False(t, pathExists(filepath.Join(dst, "new-source-file")), "new-source-file should not exist in dst directory after no-op second move")

	})
}
