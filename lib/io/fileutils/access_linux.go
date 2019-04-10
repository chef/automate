// +build !windows !darwin

// The following functions are an attempt at determining file access for specific
// users without requiring programs to Setuid or Setgid to a user to use the
// the faccess system call. One major caveat here is that the implementation
// only works on a file permissions level and does not factor Linux ACLs that
// may be present on some filesystems.

package fileutils

import (
	"os"
	"os/user"
	"path/filepath"
	"strconv"
	"syscall"

	"github.com/pkg/errors"
)

// Readable returns whether or not a path is readable by a given user
func Readable(uname string, path string) (bool, error) {
	uid, gids, err := uidGidsFor(uname)
	if err != nil {
		return false, err
	}

	stats, err := pathStatsFor(path)
	if err != nil {
		return false, err
	}

	return readableBy(uid, gids, stats), nil
}

// Writable returns whether or not a path is writable by a given user
func Writable(uname string, path string) (bool, error) {
	uid, gids, err := uidGidsFor(uname)
	if err != nil {
		return false, err
	}

	stats, err := pathStatsFor(path)
	if err != nil {
		return false, err
	}

	return writableBy(uid, gids, stats), nil
}

// Executable returns whether or not a path is executable by a given user. In
// addition to verifying that the executable bit is set for the base of the path,
// it traverses the path in reverse and verifies that the user has the exec
// bit through the entire path.
func Executable(uname string, path string) (bool, error) {
	uid, gids, err := uidGidsFor(uname)
	if err != nil {
		return false, err
	}

	stats, err := pathStatsFor(path)
	if err != nil {
		return false, err
	}

	return executableBy(uid, gids, stats), nil
}

// ReadWritable returns whether or not a path is RW by a given user
func ReadWritable(uname string, path string) (bool, error) {
	uid, gids, err := uidGidsFor(uname)
	if err != nil {
		return false, err
	}

	stats, err := pathStatsFor(path)
	if err != nil {
		return false, err
	}

	return readWritableBy(uid, gids, stats), nil
}

// ReadWriteExecutable returns whether or not a path is RWE by a given user
func ReadWriteExecutable(uname string, path string) (bool, error) {
	uid, gids, err := uidGidsFor(uname)
	if err != nil {
		return false, err
	}

	stats, err := pathStatsFor(path)
	if err != nil {
		return false, err
	}

	return readWriteExecutableBy(uid, gids, stats), nil
}

// MakeReadWriteExecutable takes a user and path and attempts to modify file
// permissions to make it RWE in the least intrusive way. There are many ways
// to do this but what I've deemed the least intrusive generally is:
// If the base path isn't owned by the user or share group membership with the user
// we'll change the files ownership to the user instead of making it RWE for everybody.
// Next, we'll update the owner or group bits with RWE. Finally, we'll ensure
// that the user has exec bits through the entire path either via ownership,
// group membership, or everybody.
// NOTE: it's best to assume that the UID running this function is root.
func MakeReadWriteExecutable(uname, path string) error {
	var (
		uid   uint32
		gids  []uint32
		stats PathStats
		base  PathStat
		err   error
	)

	uid, gids, err = uidGidsFor(uname)
	if err != nil {
		return errors.Wrap(err, "error determining group ids")
	}

	stats, err = pathStatsFor(path)
	if err != nil {
		return errors.Wrap(err, "error loading file stats for for path")
	}

	if len(stats) == 0 {
		return errors.New("no file stats for path")
	}

	if readWritableBy(uid, gids, stats) {
		return nil
	}

	// Make sure the owner or group of the base is our uname
	base = stats[0]
	if uid != base.stat.Uid && !sharesGid(base.stat.Gid, gids) {
		err = os.Chown(base.path, int(uid), int(base.stat.Gid))
		if err != nil {
			return errors.Wrap(err, "failed to change owner")
		}

		base.stat, err = statFor(base.path)
		if err != nil {
			return errors.Wrap(err, "failed to reload file stat")
		}
	}

	// Make sure the our uname has exec through owner or group
	err = os.Chmod(base.path, rweModeFor(uid, gids, base.stat))
	if err != nil {
		return errors.Wrapf(err, "failed to make path (%s) read/write/exec", base.path)
	}

	// Set exec on each directory in the path
	for _, stat := range stats[1:] {
		err = os.Chmod(stat.path, execModeFor(uid, gids, stat.stat))
		if err != nil {
			return errors.Wrapf(err, "failed to make path (%s) exec", stat.path)
		}
	}

	return nil
}

// PathStats is a slice representing the stat results of path in reverse order
// starting with the base at index 0 all the way to / on the slice edge.
type PathStats []PathStat

// PathStat contains a path and the corresponding syscall.Stat_t for the path.
type PathStat struct {
	path string
	stat *syscall.Stat_t
}

func readableBy(uid uint32, gids []uint32, stats PathStats) bool {
	if len(stats) == 0 {
		return false
	}

	if !hasReadBit(uid, gids, stats[0].stat) {
		return false
	}

	if len(stats[1:]) > 0 {
		return executableBy(uid, gids, stats[1:])
	}

	return true
}

func writableBy(uid uint32, gids []uint32, stats PathStats) bool {
	if len(stats) == 0 {
		return false
	}

	if !hasWriteBit(uid, gids, stats[0].stat) {
		return false
	}

	if len(stats[1:]) > 0 {
		return executableBy(uid, gids, stats[1:])
	}

	return true
}

func executableBy(uid uint32, gids []uint32, stats PathStats) bool {
	if len(stats) == 0 {
		return false
	}

	for _, stat := range stats {
		if !hasExecBit(uid, gids, stat.stat) {
			return false
		}
	}

	return true
}

func readWritableBy(uid uint32, gids []uint32, stats PathStats) bool {
	return readableBy(uid, gids, stats) && writableBy(uid, gids, stats)
}

func readWriteExecutableBy(uid uint32, gids []uint32, stats PathStats) bool {
	return readWritableBy(uid, gids, stats) && executableBy(uid, gids, stats)
}

// uidGidsFor takes a username and path and return the found UID, GIDs
// and any error encountered.
func uidGidsFor(uname string) (uint32, []uint32, error) {
	var (
		u     *user.User
		uid   uint32
		uidi  int
		gidi  int
		gids  = []uint32{}
		gidss []string
		err   error
	)

	u, err = user.Lookup(uname)
	if err != nil {
		return uid, gids, err
	}

	uidi, err = strconv.Atoi(u.Uid)
	if err != nil {
		return uid, gids, err
	}
	uid = uint32(uidi)

	// WARNING: GroupIds() might not be implemented on linux if cgo is disabled.
	// Instead of failing if it returns an error we'll fall back to the primary
	// group only.
	gidss, err = u.GroupIds()
	if err != nil {
		g, err := strconv.Atoi(u.Gid)
		if err != nil {
			return uid, gids, err
		}

		gids = append(gids, uint32(g))
	} else {
		for _, g := range gidss {
			gidi, err = strconv.Atoi(g)
			if err != nil {
				return uid, gids, err
			}
			gids = append(gids, uint32(gidi))
		}
	}

	return uid, gids, nil
}

// rweModeFor takes a UID, GIDs, and Stat_t and returns an updated os.FileMode
// for the Stat_t that is RWE.
func rweModeFor(uid uint32, gids []uint32, stat *syscall.Stat_t) os.FileMode {
	if stat.Uid == uid {
		return os.FileMode(stat.Mode | 0700)
	}

	for _, gid := range gids {
		if stat.Gid == gid {
			return os.FileMode(stat.Mode | 0070)
		}
	}

	return os.FileMode(stat.Mode | 0007)
}

// execModeFor takes a UID, GIDs, and Stat_t and returns an updated os.FileMode
// for the Stat_t that is executable.
func execModeFor(uid uint32, gids []uint32, stat *syscall.Stat_t) os.FileMode {
	if stat.Uid == uid {
		return os.FileMode(stat.Mode | 0100)
	}

	for _, gid := range gids {
		if stat.Gid == gid {
			return os.FileMode(stat.Mode | 0010)
		}
	}

	return os.FileMode(stat.Mode | 0001)
}

func statFor(path string) (*syscall.Stat_t, error) {
	stat := &syscall.Stat_t{}

	return stat, syscall.Stat(path, stat)
}

// modeHasBits takes an os.FileMode and checks it for the bit represented by
// another os.FileMode.
func modeHasBits(mode, expected uint32) bool {
	return (mode & expected) == expected
}

func pathStatsFor(path string) (PathStats, error) {
	var (
		stat  *syscall.Stat_t
		stats = PathStats{}
		err   error
	)

	for {
		stat, err = statFor(path)
		if err != nil {
			return stats, errors.Wrapf(err, "failed to get stat for path: %s", path)
		}

		stats = append(stats, PathStat{path: path, stat: stat})

		if path == "/" {
			break
		}

		path = filepath.Dir(path)
	}

	return stats, nil
}

func sharesGid(gid uint32, gids []uint32) bool {
	for _, g := range gids {
		if g == gid {
			return true
		}
	}

	return false
}

func hasExecBit(uid uint32, gids []uint32, stat *syscall.Stat_t) bool {
	if uid == 0 {
		return true
	}

	if uid == stat.Uid {
		// The file is owned by our user so check for the owner write bit
		if modeHasBits(stat.Mode, 0100) {
			return true
		}
	}

	for _, gid := range gids {
		if gid == stat.Gid {
			// The file and user are in the same group so check the group exec bit
			if modeHasBits(stat.Mode, 0010) {
				return true
			}
		}
	}

	// The file is not owned by the user or in the same group so we'll see
	// if writable by everybody
	return modeHasBits(stat.Mode, 0001)
}

func hasWriteBit(uid uint32, gids []uint32, stat *syscall.Stat_t) bool {
	if uid == 0 {
		return true
	}

	if uid == stat.Uid {
		// The file is owned by our user so check for the owner write bit
		if modeHasBits(stat.Mode, 0200) {
			return true
		}
	}

	for _, gid := range gids {
		if gid == stat.Gid {
			// The file and user are in the same group so check the group write bit
			if modeHasBits(stat.Mode, 0020) {
				return true
			}
		}
	}

	// The file is not owned by the user or in the same group so we'll see
	// if writable by everybody
	return modeHasBits(stat.Mode, 0002)
}

func hasReadBit(uid uint32, gids []uint32, stat *syscall.Stat_t) bool {
	if uid == 0 {
		return true
	}

	if uid == stat.Uid {
		// The file is owned by our user so check for the owner read bit
		if modeHasBits(stat.Mode, 0400) {
			return true
		}
	}

	for _, gid := range gids {
		if gid == stat.Gid {
			// The file and user are in the same group so check the group read bit
			if modeHasBits(stat.Mode, 0040) {
				return true
			}
		}
	}

	// The file is not owned by the user or in the same group so we'll see
	// if readable by everybody
	return modeHasBits(stat.Mode, 0004)
}
