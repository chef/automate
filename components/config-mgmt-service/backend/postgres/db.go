package postgres

import (
	"database/sql"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/config-mgmt-service/config"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type Postgres struct {
	*config.Postgres
	db     *sql.DB
	mapper *gorp.DbMap
}

func Open(config *config.Postgres) (*Postgres, error) {
	pg := New(config)

	err := pg.Connect()
	if err != nil {
		return nil, err
	}
	err = pg.Ping()
	if err != nil {
		return nil, err
	}
	err = pg.Migrate()
	if err != nil {
		return nil, err
	}

	return pg, nil
}

func New(config *config.Postgres) *Postgres {
	return &Postgres{Postgres: config}
}

func (db *Postgres) Connect() error {
	log.WithFields(log.Fields{
		"uri": db.URI,
	}).Info("Connecting to PostgreSQL")

	dbMap, err := libdb.PGOpen(db.URI)
	if err != nil {
		return errors.Wrapf(err, "Failed to open database with uri: %s", db.URI)
	}
	db.db = dbMap

	if db.MaxIdleConns > 0 {
		dbMap.SetMaxIdleConns(db.MaxIdleConns)
	}
	if db.MaxOpenConns > 0 {
		dbMap.SetMaxOpenConns(db.MaxOpenConns)
	}

	db.mapper = &gorp.DbMap{Db: dbMap, Dialect: gorp.PostgresDialect{}}

	return nil
}

func (db *Postgres) Migrate() error {
	logger := logger.NewLogrusStandardLogger()
	log.SetLevel(log.DebugLevel)
	if err := migrator.Migrate(db.URI, db.SchemaPath, logger, true); err != nil {
		log.WithError(err).WithFields(log.Fields{
			"uri":         db.URI,
			"schema_path": db.SchemaPath,
		}).Error("Failed migrating database")
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}
	return nil
}

// ping will verify if the database mapped with gorp is available
func (db *Postgres) Ping() error {
	return db.db.Ping()
}
