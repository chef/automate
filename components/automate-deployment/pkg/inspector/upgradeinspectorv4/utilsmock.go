package upgradeinspectorv4

import (
	"io"
)

type MockUpgradeV4UtilsImp struct {
	IsExternalElasticSearchFunc func(timeout int64) bool
	ExecRequestFunc             func(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetESBasePathFunc           func(timeout int64) string
	GetBackupS3URLFunc          func(timeout int64) (string, error)
	PatchS3backupURLFunc        func(timeout int64) (stdOut, stdErr string, err error)
	GetMaintenanceStatusFunc    func(timeout int64) (bool, error)
	SetMaintenanceModeFunc      func(timeout int64, status bool) (stdOut, stdErr string, err error)
	GetElasticsearchPIDFunc     func() (string, error)
	WriteToFileFunc             func(filepath string, data []byte) error
	GetServicesStatusFunc       func() (bool, error)
}

func (utl *MockUpgradeV4UtilsImp) IsExternalElasticSearch(timeout int64) bool {
	return utl.IsExternalElasticSearchFunc(timeout)
}
func (utl *MockUpgradeV4UtilsImp) ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error) {
	return utl.ExecRequestFunc(url, methodType, requestBody)
}
func (utl *MockUpgradeV4UtilsImp) GetESBasePath(timeout int64) string {
	return utl.GetESBasePathFunc(timeout)
}
func (utl *MockUpgradeV4UtilsImp) GetBackupS3URL(timeout int64) (string, error) {
	return utl.GetBackupS3URLFunc(timeout)
}
func (utl *MockUpgradeV4UtilsImp) PatchS3backupURL(timeout int64) (stdOut, stdErr string, err error) {
	return utl.PatchS3backupURLFunc(timeout)
}
func (utl *MockUpgradeV4UtilsImp) GetMaintenanceStatus(timeout int64) (bool, error) {
	return utl.GetMaintenanceStatusFunc(timeout)
}
func (utl *MockUpgradeV4UtilsImp) SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error) {
	return utl.SetMaintenanceModeFunc(timeout, status)
}
func (utl *MockUpgradeV4UtilsImp) WriteToFile(filepath string, data []byte) error {
	return utl.WriteToFileFunc(filepath, data)
}
func (utl *MockUpgradeV4UtilsImp) GetServicesStatus() (bool, error) {
	return utl.GetServicesStatusFunc()
}
