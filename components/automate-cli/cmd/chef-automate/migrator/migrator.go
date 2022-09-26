package migrator

type Migrator interface {
	PreMigration() error
	Run() error
	PostMigration() error
}
