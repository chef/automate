package a1upgrade

import (
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/sys"
)

// IsEmptyDir returns true if an only if the given path is a directory
// (or symlink to a directory) and is empty. An error is returned if
// Stat() or ReadDir() fails.
func IsEmptyDir(dir string) (bool, error) {
	// Stat follows symlinks
	stat, err := os.Stat(dir)
	if os.IsNotExist(err) {
		return false, nil
	}

	if err != nil {
		return false, err
	}

	if stat.IsDir() {
		dentries, err := ioutil.ReadDir(dir)
		if err != nil {
			return false, err
		}

		if len(dentries) == 0 {
			return true, nil
		}
	}

	return false, nil
}

// IsSameFilesystem returns true if both of the paths exist on the
// same filesystem.
func IsSameFilesystem(path1 string, path2 string) (bool, error) {
	mnt1, err := sys.MountFor(path1)
	if err != nil {
		return false, errors.Wrapf(err, "error looking up mount for path %q", path1)
	}
	mnt2, err := sys.MountFor(path2)
	if err != nil {
		return false, errors.Wrapf(err, "error looking up mount for path %q", path2)
	}

	return mnt2 == mnt1, nil
}
