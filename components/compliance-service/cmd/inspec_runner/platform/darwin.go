// +build darwin

package platform

import "syscall"

func Setuid(uid int) error {
	return syscall.Setreuid(uid, uid)
}

func Setgid(gid int) error {
	return syscall.Setregid(gid, gid)
}
