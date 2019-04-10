// Package platform encapsulates platform-specific behavior. Only
// put generic code in this file, per-platform code should go in the
// appropriately named per-platform files in this directory.
package sys

import (
	"strings"

	"github.com/pkg/errors"
)

// DefaultFilePerms is our default creation mode for files. By
// default we only allow the user to operate on a file.
const DefaultFilePerms = 0600

// DefaultDirPerms is our default creation mode for directories. By
// default we only allow the user to operate on the directory.
const DefaultDirPerms = 0700

// HabDefaultDataDirPerms is the mode used by habitat for data
// directories. We're using it when precreating them.
const HabDefaultDataDirPerms = 0770

func findMountEntry(path string, mountList []string) string {
	for _, m := range mountList {
		if path == m {
			return m
		}
	}
	return ""
}

// MountFor returns the closest match in the mount table for the given
// path.
func MountFor(path string) (string, error) {
	if !strings.HasPrefix(path, "/") {
		return "", errors.New("absolute path expected")
	}

	mounts, err := AllMounts()
	if err != nil {
		return "", errors.Wrap(err, "failed to get mount table")
	}

	parts := strings.Split(path, "/")
	for len(parts) > 0 {
		testPath := strings.Join(parts, "/")
		entry := findMountEntry(testPath, mounts)
		if entry != "" {
			return entry, nil
		}
		parts = parts[:len(parts)-1]
	}

	return "/", nil
}
