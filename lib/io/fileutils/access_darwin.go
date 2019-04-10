// +build darwin

// Right now we don't have an implementation for Darwin. If we ever actually
// need cross compilation support we can shared almost everything with linux
// and darwin except that Stat_t uses uint16 rather than uint32 on linux.

package fileutils

import (
	"github.com/pkg/errors"
)

// Readable returns whether or not a path is readable by a given user
func Readable(uname string, path string) (bool, error) {
	return false, errors.New("unimplemented on darwin")
}

// Writable returns whether or not a path is writable by a given user
func Writable(uname string, path string) (bool, error) {
	return false, errors.New("unimplemented on darwin")
}

// Executable returns whether or not a path is executable by a given user. In
// addition to verifying that the executable bit is set for the base of the path,
// it traverses the path in reverse and verifies that the user has the exec
// bit through the entire path.
func Executable(uname string, path string) (bool, error) {
	return false, errors.New("unimplemented on darwin")
}

// ReadWritable returns whether or not a path is RW by a given user
func ReadWritable(uname string, path string) (bool, error) {
	return false, errors.New("unimplemented on darwin")
}

// ReadWriteExecutable returns whether or not a path is RWE by a given user
func ReadWriteExecutable(uname string, path string) (bool, error) {
	return false, errors.New("unimplemented on darwin")
}

// MakeReadWriteExecutable takes a user and path and attempts to modify file
// permissions in the least intrusive way to make the base of the path RWE.
func MakeReadWriteExecutable(uname, path string) error {
	return errors.New("unimplemented on darwin")
}
