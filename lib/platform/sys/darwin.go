// +build darwin

package sys

import "syscall"

// SetUmask sets the process umask. Returns the old umask.
func SetUmask(newUmask int) int {
	return syscall.Umask(newUmask)
}

// AllMounts returns a slice of the mounted paths (strings) on the
// machine. If we ever need more info, we could return a more generic
// struct
func AllMounts() ([]string, error) {
	flags := 2 // MNT_NOWAIT.  Why? I have no idea but this is what OS X does.
	n, err := syscall.Getfsstat(nil, flags)
	if err != nil {
		return nil, err
	}
	data := make([]syscall.Statfs_t, n)
	ret := make([]string, n)
	_, err = syscall.Getfsstat(data, flags)
	if err != nil {
		return nil, err
	}

	for i, m := range data {
		ret[i] = mntOnNameToString(m.Mntonname[:])
	}
	return ret, nil
}

// SpaceAvailForPath returns the space in KB available for the given
// path.
func SpaceAvailForPath(path string) (uint64, error) {
	stat := &syscall.Statfs_t{}
	err := syscall.Statfs(path, stat)
	if err != nil {
		return 0, err
	}
	return (uint64(stat.Bsize) * stat.Bavail) / 1024, nil
}

// SystemMemoryKB returns the total memory in KB. Unimplemented on
// OSX.
func SystemMemoryKB() (int, error) {
	return 0, nil
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

func mntOnNameToString(s []int8) string {
	ret := make([]byte, 0, len(s))
	for _, v := range s {
		if v == 0 {
			break
		}
		ret = append(ret, byte(v))
	}
	return string(ret)
}
