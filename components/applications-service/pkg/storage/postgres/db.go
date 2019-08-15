package postgres

import (
	gosql "database/sql"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

// Postgres is a wrapping struct that will hold the database mapping object
// from the underlying db/sql implementation (gorp) plus our service config
// specifically for storage.
//
// Additionally this struct implements our storage.Client interface
type Postgres struct {
	*gorp.DbMap
	*config.Postgres
}

// New creates a new Postgres client
func New(dbConf *config.Postgres) (*Postgres, error) {
	pg := &Postgres{Postgres: dbConf}

	log.WithFields(log.Fields{
		"uri": pg.URI,
	}).Debug("Connecting to PostgreSQL")
	err := pg.connect()
	if err != nil {
		return pg, err
	}

	log.WithFields(log.Fields{
		"uri":    pg.URI,
		"schema": pg.SchemaPath,
	}).Debug("Initializing database")
	err = pg.initDB()
	return pg, err
}

// ping will verify if the database mapped with gorp is available
func (db *Postgres) ping() error {
	return db.Db.Ping()
}

// connect opens a connection to the database
func (db *Postgres) connect() error {
	dbMap, err := gosql.Open("postgres", db.URI)
	if err != nil {
		return errors.Wrapf(err, "Failed to open database with uri: %s", db.URI)
	}

	// Configure the database mapping object
	db.DbMap = &gorp.DbMap{Db: dbMap, Dialect: gorp.PostgresDialect{}}

	// Verify database
	err = db.ping()
	if err != nil {
		return errors.Wrapf(err, "Failed to ping database with uri: %s", db.URI)
	}

	return nil
}

// initDB initializes the database
func (db *Postgres) initDB() error {
	// Create the schema
	// @afiune Can we rename this library?
	// @sr Just do it ;)
	if err := migrator.Migrate(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}

	return nil
}
