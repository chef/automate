package migratorv4

type MockMigratorV4UtilsImpl struct {
	GetEsTotalShardSettingsFunc func(path string) (int32, error)
	PatchOpensearchConfigFunc   func(es *ESSettings) (string, string, error)
	IsExternalElasticSearchFunc func(timeout int64) bool
	StopAutomateFunc            func() error
	StartAutomateFunc           func() error
	ReadV4ChecklistFunc         func(id, path string) (bool, error)
	UpdatePostChecklistFileFunc func(id, path string) error
	ExecuteCommandFunc          func(command string, args []string, workingDir string) error
	GetServicesStatusFunc       func() (bool, error)
	CreateMigrateJsonFunc       func() error
	GetAutomateFQDNFunc         func(timeout int64) string
}

func (mui *MockMigratorV4UtilsImpl) GetEsTotalShardSettings(path string) (int32, error) {
	return mui.GetEsTotalShardSettingsFunc(path)
}
func (mui *MockMigratorV4UtilsImpl) PatchOpensearchConfig(es *ESSettings) (string, string, error) {
	return mui.PatchOpensearchConfigFunc(es)
}
func (mui *MockMigratorV4UtilsImpl) IsExternalElasticSearch(timeout int64) bool {
	return mui.IsExternalElasticSearchFunc(timeout)
}
func (mui *MockMigratorV4UtilsImpl) StopAutomate() error {
	return mui.StopAutomateFunc()
}
func (mui *MockMigratorV4UtilsImpl) StartAutomate() error {
	return mui.StartAutomateFunc()
}
func (mui *MockMigratorV4UtilsImpl) ReadV4Checklist(id, path string) (bool, error) {
	return mui.ReadV4ChecklistFunc(id, path)
}
func (mui *MockMigratorV4UtilsImpl) UpdatePostChecklistFile(id, path string) error {
	return mui.UpdatePostChecklistFileFunc(id, path)
}
func (mui *MockMigratorV4UtilsImpl) ExecuteCommand(command string, args []string, workingDir string) error {
	return mui.ExecuteCommandFunc(command, args, workingDir)
}
func (mui *MockMigratorV4UtilsImpl) GetServicesStatus() (bool, error) {
	return mui.GetServicesStatusFunc()
}
func (mui *MockMigratorV4UtilsImpl) CreateMigrateJson() error {
	return mui.CreateMigrateJsonFunc()
}
func (mui *MockMigratorV4UtilsImpl) GetAutomateFQDN(timeout int64) string {
	return mui.GetAutomateFQDNFunc(timeout)
}
