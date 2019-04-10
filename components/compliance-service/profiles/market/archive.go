package market

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

// return the list of all namespaces
func Namespaces(profilePath string) ([]string, error) {
	namespaces := []string{}

	files, err := ioutil.ReadDir(profilePath)
	if err != nil {
		return namespaces, errors.Wrap(err, "ListProfiles ReadDir error")
	}

	for _, entry := range files {
		filename := filepath.Join(profilePath, entry.Name())
		fileinfo, err := os.Stat(filename)
		if err != nil {
			logrus.Errorf("Namespaces stat error: %s", err)
			continue
		}

		if fileinfo.Mode().IsDir() && !strings.HasPrefix(entry.Name(), ".") {
			namespaces = append(namespaces, entry.Name())
		}
	}

	return namespaces, nil
}

func Archive(profilePath, namespace string, profile string) error {
	// ensure the archive directory is available
	archivePath := filepath.Join(profilePath, ".archive", namespace)
	err := os.MkdirAll(archivePath, os.ModePerm)
	if err != nil {
		return err
	}

	fileinfo, err := os.Stat(profile)
	if err != nil {
		return err
	}

	// move profile
	targetPath := filepath.Join(archivePath, fileinfo.Name())
	logrus.Infof("Archive profile %s to %s", profile, targetPath)
	return os.Rename(profile, targetPath)
}
