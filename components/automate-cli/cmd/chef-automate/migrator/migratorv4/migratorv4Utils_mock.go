package migratorV4

type MockMigratorV4UtilsImpl struct {
	GetEsTotalShardSettingsFunc func() (int32, error)
	PatchOpensearchConfigFunc   func(es *ESSettings) (string, string, error)
	IsExternalElasticSearchFunc func(timeout int64) bool
	StopAutomateFunc            func() error
	StartAutomateFunc           func() error
	GetHabRootPathFunc          func(habrootcmd string) string
	ReadV4ChecklistFunc         func(id string) (bool, error)
	UpdatePostChecklistFileFunc func(id string) error
	ExecuteCommandFunc          func(command string, args []string, workingDir string) error
	GetServicesStatusFunc       func() (bool, error)
	CreateMigrateJsonFunc       func() error
}

func (mui *MockMigratorV4UtilsImpl) GetEsTotalShardSettings() (int32, error) {
	return mui.GetEsTotalShardSettingsFunc()
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
func (mui *MockMigratorV4UtilsImpl) GetHabRootPath(habrootcmd string) string {
	return mui.GetHabRootPathFunc(habrootcmd)
}
func (mui *MockMigratorV4UtilsImpl) ReadV4Checklist(id string) (bool, error) {
	return mui.ReadV4ChecklistFunc(id)
}
func (mui *MockMigratorV4UtilsImpl) UpdatePostChecklistFile(id string) error {
	return mui.UpdatePostChecklistFileFunc(id)
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
