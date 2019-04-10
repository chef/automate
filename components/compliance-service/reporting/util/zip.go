// Copyright 2015 Dominik Richter. All rights reserved.

package util

import (
	"archive/zip"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"

	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/sirupsen/logrus"
)

// Zip2Path extracts a zip file on disk to a destination folder on disk.
func Zip2Path(zipPath string, extractPath string) error {
	reader, err := zip.OpenReader(zipPath)
	if err != nil {
		return err
	}
	defer reader.Close() // nolint: errcheck

	for _, curFile := range reader.File {
		rc, err := curFile.Open()
		if err != nil {
			return err
		}
		dstPath, err := constructFilepath(extractPath, curFile.Name)
		if err != nil {
			return err
		}
		mode := curFile.FileInfo().Mode()
		if mode.IsDir() {
			err = os.MkdirAll(dstPath, os.ModePerm)
			if err != nil {
				return err
			}
		} else if mode.IsRegular() { // exclusive: dirs are not regular
			f, err := os.Create(dstPath)
			if err != nil {
				return err
			}
			_, err = io.Copy(f, rc)
			cerr := rc.Close()
			ferr := f.Close()
			if err != nil {
				return err
			}
			if cerr != nil {
				return cerr
			}
			if ferr != nil {
				return ferr
			}
		}
		// ignore everything that is not dir or regular
	}
	return nil
}

// ConvertZipToTarGz extracts the profile to a tmp dir and archives the file as a tar.gz.
func ConvertZipToTarGz(zipPath string, tarPath string) error {
	// should we make this user specific
	tmpPath, err := ioutil.TempDir("", "inspec-upload")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmpPath) // nolint: errcheck

	err = os.Chmod(tmpPath, os.ModePerm)
	if err != nil {
		return err
	}

	// extract the file to the folder
	logrus.Debugf("Extract profile to %s", tmpPath)
	err = Zip2Path(zipPath, tmpPath)
	if err != nil {
		return err
	}

	// if no inspec.yml, make tmpPath go one level deeper (https://github.com/chef/automate/issues/3733)
	// we don't assume anything will be more than one level deep
	pathToTest := path.Join(tmpPath, "inspec.yml")
	if _, err := os.Stat(pathToTest); os.IsNotExist(err) {
		// read dir to find name of folder to run inspec archive on
		files, err := ioutil.ReadDir(tmpPath)
		if err != nil {
			return err
		}
		// we should only ever have one folder here
		if len(files) != 1 {
			return fmt.Errorf("something went wrong, too many folders found")
		}
		folder := files[0].Name()
		tmpPath = path.Join(tmpPath, folder)
		// ensure we have access to the folder
		if err := chModAll(tmpPath); err != nil {
			return err
		}
	}

	err = inspec.Archive(tmpPath, tarPath)
	if err != nil {
		return err
	}
	return nil
}

func chModAll(path string) error {
	return filepath.Walk(path, func(name string, info os.FileInfo, err error) error {
		if err == nil {
			err = os.Chmod(name, 0777)
		}
		return err
	})
}

// clean neutralizes path traversal ("/../../foo" -> "foo") ("../bar" -> "bar")
func clean(dirtyPath string) (string, error) {
	x := path.Join("/", dirtyPath)
	if len(x) == 1 {
		return "", fmt.Errorf("invalid path to cleanup: `%s`", dirtyPath)
	}
	return x[1:], nil // drop leading "/"
}

func constructFilepath(extractPath, filename string) (string, error) {
	cleanPath, err := clean(filename)
	if err != nil {
		return "", errors.New("Failed to clean filename from uploaded tar: " + err.Error())
	}
	return path.Join(extractPath, cleanPath), nil
}
