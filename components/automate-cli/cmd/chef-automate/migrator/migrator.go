package migrator

type Migrator interface {
	AskForConfirmation() error
	IsExecutedCheck() error
	AddMigrationSteps(migrationSteps MigrationSteps)
	ExecuteMigrationSteps() error
	SaveExecutedStatus() error
	ExecuteDeferredSteps() error
	PrintMigrationErrors()
	RunMigrationFlow()
}

type MigrationSteps interface {
	Run() error
	ErrorHandler()
}

type PostMigrationSteps interface {
	MigrationSteps
	DefferedHandler() error
}
