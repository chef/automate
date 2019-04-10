package a1upgrade

import (
	"io/ioutil"
	"strings"

	"github.com/pkg/errors"
)

// A1VersionManifestPath is the path to the text-formatted omnibus version manifest for A1
var A1VersionManifestPath = "/opt/delivery/version-manifest.txt"

// VersionStringFromA1Manifest returns the version string from the Automate 1 manifest
func VersionStringFromA1Manifest() (string, error) {
	versionData, err := ioutil.ReadFile(A1VersionManifestPath)
	if err != nil {
		return "", errors.Wrap(err, "unable to read Automate 1 manifest file")
	}

	lines := strings.Split(string(versionData), "\n")
	fields := strings.Fields(lines[0])
	if len(fields) < 2 {
		return "", errors.Errorf("unable to find version in first line of Automate 1 manifest. (first line is %q)", lines[0])
	}
	return fields[1], nil
}
