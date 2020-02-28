package fileutils

import (
	"os"
	"path/filepath"

	"github.com/pkg/errors"
)

type copyOpts struct {
	recursive bool
	depth     int
	overwrite bool
}

// Opt is a functional option
type Opt func(*copyOpts)

// NoRecursive will not recusively traverse directories if the function supports it
func NoRecursive() Opt {
	return func(c *copyOpts) {
		c.recursive = false
	}
}

// Depth determines how far to recursively traverse if the function supports it
func Depth(depth int) Opt {
	return func(c *copyOpts) {
		c.depth = depth
	}
}

// Overwrite will overwrite the file and/or directory and files
func Overwrite() Opt {
	return func(c *copyOpts) {
		c.overwrite = true
	}
}

// CopyFile copies the file at the src into the dst path
func CopyFile(srcPath, dstPath string, options ...Opt) error {
	opts := &copyOpts{}

	for _, o := range options {
		o(opts)
	}

	return copyFile(srcPath, dstPath, opts)
}

func copyFile(srcPath, dstPath string, opts *copyOpts) error {
	srcPath = filepath.Clean(srcPath)
	dstPath = filepath.Clean(dstPath)

	src, err := os.Open(srcPath)
	if err != nil {
		return err
	}
	defer src.Close() // nolint errcheck

	srcInfo, err := os.Stat(srcPath)
	if err != nil {
		return err
	}

	_, err = os.Stat(dstPath)
	if err != nil {
		if !os.IsNotExist(err) {
			return err
		}
	} else {
		if !opts.overwrite {
			return errors.Errorf("%s already exists", dstPath)
		}
	}

	return AtomicWrite(dstPath, src, WithAtomicWriteFileMode(srcInfo.Mode()))
}
