package upgradeinspectorv4

import "io"

type MockUpgradeV4UtilsImp struct {
	IsExternalElasticSearchFunc func() bool
	ExecRequestFunc             func(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetESBasePathFunc           func(timeout int64) string
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
