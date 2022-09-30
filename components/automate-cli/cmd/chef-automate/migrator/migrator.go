package migrator

type Migrator interface {
	AskForConfirmation(skipConfirmation bool) error
	IsExecutedCheck() error
	AddMigrationSteps(migrationSteps MigrationSteps)
	ExecuteMigrationSteps() error
	SaveExecutedStatus() error
	ExecuteDeferredSteps() error
	PrintMigrationErrors()
	RunMigrationFlow(skipConfirmation bool)
	ClearData() error
	RunSuccess() error
}

type MigrationSteps interface {
	Run() error
	ErrorHandler()
}

type PostMigrationSteps interface {
	MigrationSteps
	DefferedHandler() error
}

type SuccessfulMigrationSteps interface {
	MigrationSteps
	OnSuccess() error
}
