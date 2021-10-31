package postgres

import (
	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	libdb "github.com/chef/automate/lib/db"
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

type DBTrans struct {
	*gorp.Transaction
}

// New creates a new Postgres client, connects to the database server and runs
// the migrations
func ConnectAndMigrate(dbConf *config.Postgres) (*Postgres, error) {
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

func Connect(dbConf *config.Postgres) (*Postgres, error) {
	pg := &Postgres{Postgres: dbConf}

	log.WithFields(log.Fields{
		"uri": pg.URI,
	}).Debug("Connecting to PostgreSQL")
	err := pg.connect()
	return pg, err
}

func (db *Postgres) DestructiveMigrateForTests() error {
	if err := migrator.DestructiveMigrateForTests(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}
	return nil
}

// ping will verify if the database mapped with gorp is available
func (db *Postgres) ping() error {
	return db.Db.Ping()
}

// connect opens a connection to the database
func (db *Postgres) connect() error {
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

	db.DbMap.AddTableWithName(storage.Telemetry{}, "telemetry").SetKeys(false, "id")
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

// Transact wraps your calls in a transaction. If the call should fail with an error it will
// perform a rollback. Otherwise the transaction will be committed.
func Transact(pg *Postgres, txFunc func(*DBTrans) error) error {
	trans, err := pg.DbMap.Begin()
	if err != nil {
		return errors.Wrap(err, "Unable to start transaction.")
	}
	tx := DBTrans{
		Transaction: trans,
	}
	defer func() {
		if err != nil {
			tx.Rollback() // nolint: errcheck
		} else {
			err = tx.Commit()
			if err != nil {
				tx.Rollback() // nolint: errcheck
				err = errors.Wrap(err, "Transaction failed and will be rolled back.")
			}
		}
	}()

	err = txFunc(&tx)
	return err
}
