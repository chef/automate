package postgres

import (
	"github.com/chef/automate/components/config-mgmt-service/config"
)

func DestructiveReMigrateForTest(cfg *config.Postgres) error {
	var err error
	pg := New(cfg)
	err = pg.Connect()
	if err != nil {
		return err
	}
	err = pg.Ping()
	if err != nil {
		return err
	}
	err = pg.DestructiveReMigrateForTest()
	if err != nil {
		return err
	}
	return pg.Close()
}

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
