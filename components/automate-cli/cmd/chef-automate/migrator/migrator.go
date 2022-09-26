package migrator

type Migrator interface {
	AskForConfirmation()
	AddMigrationSteps(migrationSteps MigrationSteps)
	Execute() error
	EnsureStateBeforeExit()
}

type MigrationSteps interface {
	Run() error
	Skip() error
}

type RollbackSteps interface {
	MigrationSteps
	ExitHandler()
}
