package migratorv4

type MigratorV4 struct {
}

func NewMigratorV4() *MigratorV4 {
	return &MigratorV4{}
}

func (m *MigratorV4) PreMigration() error {
	return nil
}

func (m *MigratorV4) Run() error {
	return nil
}

func (m *MigratorV4) PostMigration() error {
	return nil
}
