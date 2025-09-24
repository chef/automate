package upgradeinspectorv5

import (
	"fmt"
	"io"
	"net/http"

	"github.com/chef/automate/components/automate-deployment/pkg/majorupgradechecklist"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
)

type UpgradeV5Utils interface {
	IsExternalOpenSearch(timeout int64) bool
	IsExternalPG() bool
	ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetMaintenanceStatus(timeout int64) (bool, error)
	SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error)
	GetServicesStatus() (bool, error)
}

const (
	downTimeError = "There will be a downtime while upgrading. Please prepare for down time and run the upgrade"

	backupError = "Please take a backup and restart the upgrade process."

	diskSpaceError = `Please ensure to have %.2f GB free disk space`

	postChecklistIntimationError = "Post upgrade steps need to be run, after this upgrade completed."

	reindexingError = "Please do reindexing and restart the upgrade process."

	NEW_S3_URL = "https://s3.amazonaws.com"
)

type UpgradeV5UtilsImp struct{}

func NewUpgradeV5Utils() UpgradeV5Utils {
	return &UpgradeV5UtilsImp{}
}

func (cu *UpgradeV5UtilsImp) GetMaintenanceStatus(timeout int64) (bool, error) {
	return majorupgrade_utils.GetMaintenanceStatus(timeout)
}

func (cu *UpgradeV5UtilsImp) SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error) {
	return majorupgrade_utils.SetMaintenanceMode(timeout, status)
}

func (cu *UpgradeV5UtilsImp) IsExternalOpenSearch(timeout int64) bool {
	return majorupgrade_utils.IsExternalOpenSearch(timeout)
}

func (cu *UpgradeV5UtilsImp) IsExternalPG() bool {
	return majorupgradechecklist.IsExternalPG()
}

func (cu *UpgradeV5UtilsImp) ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error) {
	method := methodType

	client := &http.Client{}
	req, err := http.NewRequest(method, url, requestBody) // nosemgrep

	if err != nil {
		return nil, err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return nil, err
	}
	defer res.Body.Close()
	body, err := io.ReadAll(res.Body) // nosemgrep
	if err != nil {
		return nil, err
	}

	if res.StatusCode != http.StatusOK {
		return nil, errors.Errorf("Request failed with status %d\n%s\n", res.StatusCode, string(body))
	}
	return body, nil
}

func (cu *UpgradeV5UtilsImp) GetServicesStatus() (bool, error) {
	return majorupgrade_utils.EnsureStatus()
}
