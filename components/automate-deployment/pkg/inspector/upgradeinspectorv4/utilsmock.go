package upgradeinspectorv4

import (
	"io"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type MockUpgradeV4UtilsImp struct {
	IsExternalElasticSearchFunc func() bool
	ExecRequestFunc             func(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetESBasePathFunc           func(timeout int64) string
	GetBackupS3URLFunc          func() (string, error)
	PatchS3backupURLFunc        func(w *cli.Writer) (stdOut, stdErr string, err error)
}

func (utl *MockUpgradeV4UtilsImp) IsExternalElasticSearch() bool {
	return utl.IsExternalElasticSearchFunc()
}

func (utl *MockUpgradeV4UtilsImp) ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error) {
	return utl.ExecRequestFunc(url, methodType, requestBody)
}
func (utl *MockUpgradeV4UtilsImp) GetESBasePath(timeout int64) string {
	return utl.GetESBasePathFunc(timeout)
}
func (utl *MockUpgradeV4UtilsImp) GetBackupS3URL() (string, error) {
	return utl.GetBackupS3URLFunc()
}
func (utl *MockUpgradeV4UtilsImp) PatchS3backupURL(w *cli.Writer) (stdOut, stdErr string, err error) {
	return utl.PatchS3backupURLFunc(w)
}
