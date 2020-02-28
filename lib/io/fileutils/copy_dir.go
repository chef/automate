package fileutils

import (
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/pkg/errors"
)

// CopyDir copies all files that are not symlinks in a source directory to another.
func CopyDir(srcDir string, dstDir string, options ...Opt) error {
	opts := &copyOpts{
		recursive: true,
		depth:     -1,
	}

	for _, o := range options {
		o(opts)
	}

	return copyDir(srcDir, dstDir, opts)
}

func copyDir(srcDir string, dstDir string, opts *copyOpts) error {
	srcDir = filepath.Clean(srcDir)
	dstDir = filepath.Clean(dstDir)

	srcDirInfo, err := os.Stat(srcDir)
	if err != nil {
		return err
	}

	if !srcDirInfo.IsDir() {
		return errors.Errorf("%s is not a directory", srcDir)
	}

	dstDirInfo, err := os.Stat(dstDir)
	if err != nil {
		if os.IsNotExist(err) {
			err = os.MkdirAll(dstDir, srcDirInfo.Mode())
			if err != nil {
				return err
			}
		} else {
			return err
		}
	} else {
		if dstDirInfo.IsDir() {
			if !opts.overwrite {
				return errors.Errorf("%s already exists", dstDir)
			}
			err = os.Chmod(dstDir, srcDirInfo.Mode())
			if err != nil {
				return err
			}
		} else {
			return errors.Errorf("%s is not a directory", dstDir)
		}
	}

	names, err := ioutil.ReadDir(srcDir)
	if err != nil {
		return err
	}

	for _, name := range names {
		srcPath := filepath.Join(srcDir, name.Name())
		dstPath := filepath.Join(dstDir, name.Name())

		if name.IsDir() {
			optsCopy := *opts
			err = recursiveCopy(srcPath, dstPath, &optsCopy)
			if err != nil {
				return err
			}

			continue
		}

		if name.Mode()&os.ModeSymlink != 0 {
			continue
		}

		optsCopy := *opts
		err = copyFile(srcPath, dstPath, &optsCopy)
		if err != nil {
			return err
		}
	}

	return nil
}

func recursiveCopy(srcDir string, dstDir string, opts *copyOpts) error {
	if !opts.recursive || opts.depth == 0 {
		return nil
	}

	if opts.depth > 0 {
		opts.depth--
	}

	return copyDir(srcDir, dstDir, opts)
}
