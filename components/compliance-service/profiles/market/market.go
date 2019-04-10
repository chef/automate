package market

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"time"

	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/sirupsen/logrus"
)

func ListProfiles(profile_path string) ([]string, error) {
	profiles := make([]string, 0)
	dir, err := filepath.Abs(profile_path)
	if err != nil {
		return profiles, fmt.Errorf("ListProfiles filepath.Abs error: %s", err)
	}

	// parse directories
	files, err := ioutil.ReadDir(dir)
	if err != nil {
		return profiles, fmt.Errorf("ListProfiles ReadDir error: %s", err)
	}

	for _, file := range files {
		filename := filepath.Join(dir, file.Name())
		fileinfo, err := os.Stat(filename)
		if err != nil {
			logrus.Errorf("ListProfiles stat error: %s", err)
			continue
		}

		var validFiletype = regexp.MustCompile(`^.*\.tar\.gz$`)
		valid := validFiletype.MatchString(fileinfo.Name())

		if valid && !fileinfo.Mode().IsDir() {
			profiles = append(profiles, filename)
		} else {
			logrus.Debugf("Ignore filename %s", filename)
		}
	}

	return profiles, nil
}

// TempFileWithSuffix provides you with a unique file name with suffix,
// the file needs to be created afterwards
func TempFileWithSuffix(suffix string) (string, error) {
	var tmpWithSuffix = ""

	// create a temp file to get a unique name
	tmpfile, err := ioutil.TempFile("", "inspec-upload")
	if err != nil {
		return tmpWithSuffix, err
	}
	defer os.Remove(tmpfile.Name()) // nolint: errcheck

	// Go's own TempFile cannot deal with suffix
	tmpWithSuffix = tmpfile.Name() + suffix
	return tmpWithSuffix, nil
}

func TempUpload(body io.ReadCloser, suffix string) (string, error) {
	tmpWithSuffix, err := TempFileWithSuffix(suffix)
	if err != nil {
		return tmpWithSuffix, err
	}

	logrus.Debug("temporary store upload at: " + tmpWithSuffix)
	outFile, err := os.Create(tmpWithSuffix)
	if err != nil {
		return tmpWithSuffix, err
	}
	defer outFile.Close() // nolint: errcheck

	_, err = io.Copy(outFile, body)
	if err != nil {
		return tmpWithSuffix, err
	}
	return tmpWithSuffix, nil
}

func CheckProfile(tmpWithSuffix string) (inspec.CheckResult, error) {
	defer util.TimeTrack(time.Now(), "CheckProfile")

	checkResult, err := inspec.Check(tmpWithSuffix)
	if err != nil {
		logrus.Error("CheckProfile InSpec Check failed for " + tmpWithSuffix)
		return checkResult, err
	}

	return checkResult, nil
}
