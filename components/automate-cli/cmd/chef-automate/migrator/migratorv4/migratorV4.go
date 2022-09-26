package migratorV4

type MigratorV4 struct {
	*MigrateV4Flags
}

type MigrateV4Flags struct {
	AutoAcceptFlag bool
	ForceExecute   bool
}

func NewMigratorV4(autoAccept, forceExecute bool) *MigratorV4 {
	return &MigratorV4{
		MigrateV4Flags: &MigrateV4Flags{
			AutoAcceptFlag: autoAccept,
			ForceExecute:   forceExecute,
		},
	}
}

func (m *MigratorV4) PreMigration() error {
	// todo: dataFlag should not be empty
	
	return nil
}

func (m *MigratorV4) Run() error {

	return nil
}

func (m *MigratorV4) PostMigration() error {
	return nil
}
