package migrator

type Migrator interface {
	AskForConfirmation() (bool, error)
	AddMigrationSteps(migrationSteps MigrationSteps)
	ExecuteMigrationSteps() error
	ExecuteDeferredSteps() error
	PrintMigrationErrors() error
}

type MigrationSteps interface {
	Run() error
	Skip() error
	ErrorHandler()
}

type PostMigrationSteps interface {
	MigrationSteps
	DefferedHandler()
}
