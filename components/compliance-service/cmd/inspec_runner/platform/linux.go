// +build linux

package platform

import "syscall"

func Setuid(uid int) error {
	return syscall.Setresuid(uid, uid, uid)
}

func Setgid(gid int) error {
	return syscall.Setresgid(gid, gid, gid)
}
