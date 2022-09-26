package migrator

type Migrator interface {
	AskForConfirmation()
	AddMigrationSteps(migrationSteps MigrationSteps)
	Execute() error
}

type MigrationSteps interface {
	Run() error
	Skip() error
}
