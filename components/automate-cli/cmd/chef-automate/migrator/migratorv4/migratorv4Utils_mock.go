package migratorV4

type MockMigratorV4UtilsImpl struct {
	CreateMigrationMetadataFunc func() error
	ReadMigrationMetadataFunc   func() error
	UpdateMigrationMetadataFunc func() error
	GetEsTotalShardSettingsFunc func() (int32, error)
	PatchOpensearchConfigFunc   func(es *ESSettings) (string, string, error)
	IsExternalElasticSearchFunc func(timeout int64) bool
	StopAutomateFunc            func() error
	StartAutomateFunc           func() error
	GetHabRootPathFunc          func(habrootcmd string) string
	ExecShCommandFunc           func(script string) error
	ReadV4ChecklistFunc         func() (bool, error)
	UpdatePostChecklistFileFunc func() error
	ExecuteCommandFunc          func(command string, args []string, workingDir string) error
}

func (mui *MockMigratorV4UtilsImpl) CreateMigrationMetadata() error {
	return mui.CreateMigrationMetadataFunc()
}
func (mui *MockMigratorV4UtilsImpl) ReadMigrationMetadata() error {
	return mui.ReadMigrationMetadataFunc()
}
func (mui *MockMigratorV4UtilsImpl) UpdateMigrationMetadata() error {
	return mui.UpdateMigrationMetadataFunc()
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
func (mui *MockMigratorV4UtilsImpl) ExecShCommand(script string) error {
	return mui.ExecShCommandFunc(script)
}
func (mui *MockMigratorV4UtilsImpl) ReadV4Checklist() (bool, error) {
	return mui.ReadV4ChecklistFunc()
}
func (mui *MockMigratorV4UtilsImpl) UpdatePostChecklistFile() error {
	return mui.UpdatePostChecklistFileFunc()
}
func (mui *MockMigratorV4UtilsImpl) ExecuteCommand(command string, args []string, workingDir string) error {
	return mui.ExecuteCommandFunc(command, args, workingDir)
}
