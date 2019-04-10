// +build !windows

package fileutils

import (
	"io/ioutil"
	"os"
	"os/user"
	"path"
	"strings"
	"syscall"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestReadable(t *testing.T) {
	t.Run("with root user", func(t *testing.T) {
		require.True(t, readableBy(0, []uint32{0}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0500),
				},
			},
		}))
	})

	t.Run("same uid with read bit", func(t *testing.T) {
		require.True(t, readableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0400),
				},
			},
		}))
	})

	t.Run("same uid no read bits", func(t *testing.T) {
		require.False(t, readableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("same gid read bit", func(t *testing.T) {
		require.True(t, readableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0040),
				},
			},
		}))
	})

	t.Run("same gid no read bits", func(t *testing.T) {
		require.False(t, readableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("no uid or gid match global read bit", func(t *testing.T) {
		require.True(t, readableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0004),
				},
			},
		}))
	})

	t.Run("no uid or gid match no read bits", func(t *testing.T) {
		require.False(t, readableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("readable success", func(t *testing.T) {
		dir, err := ioutil.TempDir("", "TestReadable")
		require.NoError(t, err, "creating temporary dir")
		defer func() {
			os.Chmod(dir, os.FileMode(0777))
			os.RemoveAll(dir)
		}()

		me, err := user.Current()
		require.NoError(t, err)

		if me.Uid == "0" || me.Gid == "0" {
			t.Skip("skipping read check because user is root")
		}

		ok, err := Readable(me.Username, dir)
		require.NoError(t, err)
		require.True(t, ok)
	})
}

func TestWritable(t *testing.T) {
	t.Run("with root user", func(t *testing.T) {
		require.True(t, writableBy(0, []uint32{0}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0500),
				},
			},
		}))
	})

	t.Run("same uid with write bit", func(t *testing.T) {
		require.True(t, writableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0200),
				},
			},
		}))
	})

	t.Run("same uid no write bits", func(t *testing.T) {
		require.False(t, writableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("same gid write bit", func(t *testing.T) {
		require.True(t, writableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0020),
				},
			},
		}))
	})

	t.Run("same gid no write bits", func(t *testing.T) {
		require.False(t, writableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("no uid or gid match global write bit", func(t *testing.T) {
		require.True(t, writableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0002),
				},
			},
		}))
	})

	t.Run("no uid or gid match no write bits", func(t *testing.T) {
		require.False(t, writableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("writable success", func(t *testing.T) {
		dir, err := ioutil.TempDir("", "TestWritable")
		require.NoError(t, err, "creating temporary dir")
		defer func() {
			os.Chmod(dir, os.FileMode(0777))
			os.RemoveAll(dir)
		}()

		me, err := user.Current()
		require.NoError(t, err)

		if me.Uid == "0" || me.Gid == "0" {
			t.Skip("skipping write check because user is root")
		}

		ok, err := Writable(me.Username, dir)
		require.NoError(t, err)
		require.True(t, ok)
	})
}

func TestExecutable(t *testing.T) {
	t.Run("with root user", func(t *testing.T) {
		require.True(t, executableBy(0, []uint32{0}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0300),
				},
			},
		}))
	})

	t.Run("same uid with exec bit", func(t *testing.T) {
		require.True(t, executableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0100),
				},
			},
		}))
	})

	t.Run("same uid with exec bit missing root exec bit", func(t *testing.T) {
		require.False(t, executableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "base",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0044),
				},
			},
			PathStat{
				path: "parent",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0100),
				},
			},
		}))
	})

	t.Run("same uid no exec bits", func(t *testing.T) {
		require.False(t, executableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0222),
				},
			},
		}))
	})

	t.Run("same gid exec bit", func(t *testing.T) {
		require.True(t, executableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0010),
				},
			},
		}))
	})

	t.Run("same gid with exec bit missing root exec bit", func(t *testing.T) {
		require.False(t, executableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0010),
				},
			},
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0222),
				},
			},
		}))
	})

	t.Run("same gid no exec bits", func(t *testing.T) {
		require.False(t, executableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0020),
				},
			},
		}))
	})

	t.Run("no uid or gid match global exec bit", func(t *testing.T) {
		require.True(t, executableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0001),
				},
			},
		}))
	})

	t.Run("no uid or gid match no exec bits", func(t *testing.T) {
		require.False(t, executableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0222),
				},
			},
		}))
	})

	t.Run("executable success", func(t *testing.T) {
		dir, err := ioutil.TempDir("", "TestExecutable")
		require.NoError(t, err, "creating temporary dir")
		defer func() {
			os.Chmod(dir, os.FileMode(0777))
			os.RemoveAll(dir)
		}()

		me, err := user.Current()
		require.NoError(t, err)

		if me.Uid == "0" || me.Gid == "0" {
			t.Skip("skipping exec check because user is root")
		}

		ok, err := Executable(me.Username, dir)
		require.NoError(t, err)
		require.True(t, ok)
	})

	t.Run("executable parent missing exec", func(t *testing.T) {
		parent, err := ioutil.TempDir("", "TestExecutableParent")
		require.NoError(t, err, "creating temporary dir")
		defer func() {
			os.Chmod(parent, os.FileMode(0777))
			os.RemoveAll(parent)
		}()

		child := path.Join(parent, "child")
		reader := strings.NewReader("child body")
		err = AtomicWrite(child, reader, WithAtomicWriteFileMode(0777))
		require.NoError(t, err)

		// Make the parent not executable
		err = os.Chmod(parent, os.FileMode(0666))
		require.NoError(t, err)

		me, err := user.Current()
		require.NoError(t, err)

		if me.Uid == "0" || me.Gid == "0" {
			t.Skip("skipping exec check because user is root")
		}

		ok, err := Executable(me.Username, child)
		require.Error(t, err)
		require.False(t, ok)
	})
}

func TestReadWritable(t *testing.T) {
	t.Run("with root user", func(t *testing.T) {
		require.True(t, readWritableBy(0, []uint32{0}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0001),
				},
			},
		}))
	})

	t.Run("same uid with read/write bits", func(t *testing.T) {
		require.True(t, readWritableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0600),
				},
			},
		}))
	})

	t.Run("same uid no read/write bits", func(t *testing.T) {
		require.False(t, readWritableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("same gid read/write bits", func(t *testing.T) {
		require.True(t, readWritableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0060),
				},
			},
		}))
	})

	t.Run("same gid no read/write bits", func(t *testing.T) {
		require.False(t, readWritableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("no uid or gid match global read/write bits", func(t *testing.T) {
		require.True(t, readWritableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0006),
				},
			},
		}))
	})

	t.Run("no uid or gid match no read/write bits", func(t *testing.T) {
		require.False(t, readWritableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0111),
				},
			},
		}))
	})

	t.Run("read/writable success", func(t *testing.T) {
		dir, err := ioutil.TempDir("", "TestReadWritable")
		require.NoError(t, err, "creating temporary dir")
		defer func() {
			os.Chmod(dir, os.FileMode(0777))
			os.RemoveAll(dir)
		}()

		me, err := user.Current()
		require.NoError(t, err)

		if me.Uid == "0" || me.Gid == "0" {
			t.Skip("skipping read/write check because user is root")
		}

		ok, err := ReadWritable(me.Username, dir)
		require.NoError(t, err)
		require.True(t, ok)
	})
}

func TestReadWriteExecutable(t *testing.T) {
	t.Run("with root user", func(t *testing.T) {
		require.True(t, readWriteExecutableBy(0, []uint32{0}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0000),
				},
			},
		}))
	})

	t.Run("same uid with read/write/exec bits", func(t *testing.T) {
		require.True(t, readWriteExecutableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0700),
				},
			},
		}))
	})

	t.Run("same uid with with read/write/exec bit missing root exec bit", func(t *testing.T) {
		require.False(t, readWriteExecutableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "child",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0700),
				},
			},
			PathStat{
				path: "parent",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0600),
				},
			},
		}))
	})

	t.Run("same uid no read/write/exec bits", func(t *testing.T) {
		require.False(t, readWriteExecutableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0070),
				},
			},
		}))
	})

	t.Run("same gid read/write/exec bits", func(t *testing.T) {
		require.True(t, readWriteExecutableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0070),
				},
			},
		}))
	})

	t.Run("same gid with with read/write/exec bit missing root exec bit", func(t *testing.T) {
		require.False(t, readWriteExecutableBy(500, []uint32{300}, PathStats{
			PathStat{
				path: "base",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0060),
				},
			},
			PathStat{
				path: "parent",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0070),
				},
			},
		}))
	})

	t.Run("same gid no read/write/exec bits", func(t *testing.T) {
		require.False(t, readWriteExecutableBy(300, []uint32{400}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0766),
				},
			},
		}))
	})

	t.Run("no uid or gid match global read/write/exec bits", func(t *testing.T) {
		require.True(t, readWriteExecutableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0007),
				},
			},
		}))
	})

	t.Run("no uid or gid match no read/write/exec bits", func(t *testing.T) {
		require.False(t, readWriteExecutableBy(100, []uint32{100}, PathStats{
			PathStat{
				path: "test",
				stat: &syscall.Stat_t{
					Uid:  uint32(500),
					Gid:  uint32(400),
					Mode: uint32(0466),
				},
			},
		}))
	})

	t.Run("read/write/exec success", func(t *testing.T) {
		dir, err := ioutil.TempDir("", "TestReadWriteExecutable")
		require.NoError(t, err, "creating temporary dir")
		defer func() {
			os.Chmod(dir, os.FileMode(0777))
			os.RemoveAll(dir)
		}()

		me, err := user.Current()
		require.NoError(t, err)

		if me.Uid == "0" || me.Gid == "0" {
			t.Skip("skipping read/write/exec check because user is root")
		}

		ok, err := ReadWriteExecutable(me.Username, dir)
		require.NoError(t, err)
		require.True(t, ok)
	})

	t.Run("read/write/exec parent missing exec", func(t *testing.T) {
		parent, err := ioutil.TempDir("", "TestReadWriteExecutable")
		require.NoError(t, err, "creating temporary dir")
		defer func() {
			os.Chmod(parent, os.FileMode(0777))
			os.RemoveAll(parent)
		}()

		child := path.Join(parent, "foo")
		reader := strings.NewReader("child body")
		err = AtomicWrite(child, reader, WithAtomicWriteFileMode(0777))
		require.NoError(t, err)

		// Make the parent not executable
		err = os.Chmod(parent, os.FileMode(0666))
		require.NoError(t, err)

		me, err := user.Current()
		require.NoError(t, err)

		if me.Uid == "0" || me.Gid == "0" {
			t.Skip("skipping read/write/exec check because user is root")
		}

		ok, err := ReadWriteExecutable(me.Username, child)
		require.Error(t, err)
		require.False(t, ok)
	})
}

func TestMakeReadWriteExecutable(t *testing.T) {
	t.Run("rweModeFor uid match", func(t *testing.T) {
		require.Equal(t,
			os.FileMode(0722),
			rweModeFor(100, []uint32{100}, &syscall.Stat_t{
				Uid:  uint32(100),
				Gid:  uint32(400),
				Mode: uint32(0122),
			}),
		)
	})

	t.Run("rweModeFor gid match", func(t *testing.T) {
		require.Equal(t,
			os.FileMode(0272),
			rweModeFor(100, []uint32{100}, &syscall.Stat_t{
				Uid:  uint32(200),
				Gid:  uint32(100),
				Mode: uint32(0222),
			}),
		)
	})

	t.Run("rweModeFor no uid/gid match", func(t *testing.T) {
		require.Equal(t,
			os.FileMode(0557),
			rweModeFor(100, []uint32{100}, &syscall.Stat_t{
				Uid:  uint32(200),
				Gid:  uint32(200),
				Mode: uint32(0555),
			}),
		)
	})

	t.Run("execModeFor uid match", func(t *testing.T) {
		require.Equal(t,
			os.FileMode(0122),
			execModeFor(100, []uint32{100}, &syscall.Stat_t{
				Uid:  uint32(100),
				Gid:  uint32(400),
				Mode: uint32(0022),
			}),
		)
	})

	t.Run("execModeFor gid match", func(t *testing.T) {
		require.Equal(t,
			os.FileMode(0511),
			execModeFor(100, []uint32{100}, &syscall.Stat_t{
				Uid:  uint32(200),
				Gid:  uint32(100),
				Mode: uint32(0501),
			}),
		)
	})

	t.Run("execModeFor no uid/gid match", func(t *testing.T) {
		require.Equal(t,
			os.FileMode(0661),
			execModeFor(100, []uint32{100}, &syscall.Stat_t{
				Uid:  uint32(200),
				Gid:  uint32(200),
				Mode: uint32(0660),
			}),
		)
	})
}
