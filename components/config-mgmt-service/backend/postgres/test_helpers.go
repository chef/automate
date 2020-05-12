package postgres

func (db *Postgres) DestructiveReMigrateForTest() error {
	_, err := db.db.Exec("DROP TABLE IF EXISTS schema_migrations;")
	if err != nil {
		return err
	}
	_, err = db.db.Exec("DROP TABLE IF EXISTS rollouts;")
	if err != nil {
		return err
	}
	err = db.Migrate()
	if err != nil {
		return err
	}
	return nil
}
