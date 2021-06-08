package postgres

import (
	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/user-settings-service/pkg/config"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

// Postgres is a wrapping struct that will hold the database mapping object
// from the underlying db/sql implementation (gorp) plus our service config
// specifically for storage.
//
// Additionally this struct implements our storage.Client interface
type DB struct {
	*gorp.DbMap
	*config.Postgres
}

// ConnectAndMigrate creates a new Postgres client, connects to the database server and runs
// the migrations
func ConnectAndMigrate(dbConf *config.Postgres) (*DB, error) {
	pg, err := Connect(dbConf)
	if err != nil {
		return nil, err
	}
	log.WithFields(log.Fields{
		"uri":    pg.URI,
		"schema": pg.SchemaPath,
	}).Debug("Initializing database")
	err = pg.initDB()
	return pg, err
}

// Connect creates a new Postgres client, connects to the database server
func Connect(dbConf *config.Postgres) (*DB, error) {
	pg := &DB{Postgres: dbConf}

	log.WithFields(log.Fields{
		"uri": pg.URI,
	}).Debug("Connecting to PostgreSQL")
	err := pg.connect()
	return pg, err
}

// DestructiveMigrateForTests will:
// * Drop the database to give you a clean slate
// * Run all your migrations
// * Forcibly run all the migrations again to verify that they are idempotent.
//
// Obviously you don't want this for production, but you should use it instead
// of the plain Migrate function in your tests if you can.
func (db *DB) DestructiveMigrateForTests() error {
	if err := migrator.DestructiveMigrateForTests(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}
	return nil
}

// ping will verify if the database mapped with gorp is available
func (db *DB) ping() error {
	return db.Db.Ping()
}

// connect opens a connection to the database
func (db *DB) connect() error {
	dbMap, err := libdb.PGOpen(db.URI)
	if err != nil {
		return errors.Wrapf(err, "Failed to open database with uri: %s", db.URI)
	}

	if db.MaxIdleConns > 0 {
		dbMap.SetMaxIdleConns(db.MaxIdleConns)
	}
	if db.MaxOpenConns > 0 {
		dbMap.SetMaxOpenConns(db.MaxOpenConns)
	}
	// Configure the database mapping object
	db.DbMap = &gorp.DbMap{Db: dbMap, Dialect: gorp.PostgresDialect{}}

	// Verify database
	err = db.ping()
	if err != nil {
		return errors.Wrapf(err, "Failed to ping database with uri: %s", db.URI)
	}
	db.TraceOn("gorp", log.New())
	return nil
}

// initDB initializes the database
func (db *DB) initDB() error {
	// Create the schema
	// @afiune Can we rename this library?
	// @sr Just do it ;)
	if err := migrator.Migrate(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}

	return nil
}
