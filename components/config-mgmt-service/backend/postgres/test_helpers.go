package postgres

import (
	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
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
	logger := logger.NewLogrusStandardLogger()
	if err := migrator.DestructiveMigrateForTests(db.URI, db.SchemaPath, logger, true); err != nil {
		log.WithError(err).WithFields(log.Fields{
			"uri":         db.URI,
			"schema_path": db.SchemaPath,
		}).Error("Failed migrating database")
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}
	return nil
}
