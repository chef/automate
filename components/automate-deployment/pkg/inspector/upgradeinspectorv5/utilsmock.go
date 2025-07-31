package upgradeinspectorv5

import (
	"io"
)

type MockUpgradeV5UtilsImp struct {
	IsExternalOpenSearchFunc func(timeout int64) bool
	IsExternalPGFunc         func() bool
	ExecRequestFunc          func(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetMaintenanceStatusFunc func(timeout int64) (bool, error)
	SetMaintenanceModeFunc   func(timeout int64, status bool) (stdOut, stdErr string, err error)
	GetServicesStatusFunc    func() (bool, error)
}

func (utl *MockUpgradeV5UtilsImp) IsExternalOpenSearch(timeout int64) bool {
	return utl.IsExternalOpenSearchFunc(timeout)
}
func (utl *MockUpgradeV5UtilsImp) IsExternalPG() bool {
	return utl.IsExternalPGFunc()
}
func (utl *MockUpgradeV5UtilsImp) ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error) {
	return utl.ExecRequestFunc(url, methodType, requestBody)
}
func (utl *MockUpgradeV5UtilsImp) GetMaintenanceStatus(timeout int64) (bool, error) {
	return utl.GetMaintenanceStatusFunc(timeout)
}
func (utl *MockUpgradeV5UtilsImp) SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error) {
	return utl.SetMaintenanceModeFunc(timeout, status)
}
func (utl *MockUpgradeV5UtilsImp) GetServicesStatus() (bool, error) {
	return utl.GetServicesStatusFunc()
}
