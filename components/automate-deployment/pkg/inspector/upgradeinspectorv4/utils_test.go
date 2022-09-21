package upgradeinspectorv4

type MockUpgradeV4UtilsImp struct {
	IsExternal bool
}

func (utl *MockUpgradeV4UtilsImp) IsExternalElasticSearch() bool {
	return utl.IsExternal
}
