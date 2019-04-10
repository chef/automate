// +build windows

package sys

import "syscall"

// SetUmask sets the process umask on non-Windows platforms. On
// Windows, it does nothing.
func SetUmask(newUmask int) int {
	return 0
}

// AllMounts returns a slice of the mounted paths (strings) on the
// machine. On Windows, do nothing.
func AllMounts() ([]string, error) {
	return nil, nil
}

// SpaceAvailForPath returns the space in KB available for the given
// path.On Windows, do nothing.
func SpaceAvailForPath(path string) (uint64, error) {
	return 0, nil
}

// SystemMemoryKB returns the total memory in KB. Unimplemented on
// Windows.
func SystemMemoryKB() (int, error) {
	return 0, nil
}

// SysProcAttrWithCred returns a SysProcAttr that does nothing on Windows.
func SysProcAttrWithCred(uid uint32, gid uint32) *syscall.SysProcAttr {
	return &syscall.SysProcAttr{}
}
