// +build linux

package sys

import (
	"bytes"
	"io/ioutil"
	"syscall"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/proc"
)

const (
	procMountsPath  = "/proc/mounts"
	procMeminfoPath = "/proc/meminfo"
)

// SetUmask sets the process umask. Returns the old umask.
func SetUmask(newUmask int) int {
	return syscall.Umask(newUmask)
}

// AllMounts returns a slice of the mounted paths (strings) on the
// machine. It parse /proc/mounts, currently extracting the "mntonname"
// only.
func AllMounts() ([]string, error) {
	data, err := ioutil.ReadFile(procMountsPath)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to read %s", procMountsPath)
	}
	return proc.ParseProcMounts(data)
}

// SpaceAvailForPath returns the space in KB available for the given
// path.
func SpaceAvailForPath(path string) (uint64, error) {
	stat := &syscall.Statfs_t{}
	err := syscall.Statfs(path, stat)
	if err != nil {
		return 0, err
	}
	return (uint64(stat.Frsize) * stat.Bavail) / 1024, nil
}

// SystemMemoryKB returns the total system memory in KB
func SystemMemoryKB() (int, error) {
	data, err := ioutil.ReadFile(procMeminfoPath)
	if err != nil {
		return 0, errors.Wrapf(err, "failed to read %q", procMeminfoPath)
	}
	return proc.ParseMemInfoMemTotal(bytes.NewReader(data))
}

// SysProcAttrWithCred returns a SysProcAttr with the credentials set
// to the given uid and gid
func SysProcAttrWithCred(uid uint32, gid uint32) *syscall.SysProcAttr {
	return &syscall.SysProcAttr{
		Credential: &syscall.Credential{
			Uid: uid,
			Gid: gid},
	}
}
