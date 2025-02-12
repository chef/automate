package storage

import (
	"database/sql"
	"time"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/config"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type DB struct {
	*gorp.DbMap
}

// ReindexRequest represents a row in the reindex_requests table
type ReindexRequest struct {
	RequestID   int       `db:"request_id"`
	Status      string    `db:"status"`
	CreatedAt   time.Time `db:"created_at"`
	LastUpdated time.Time `db:"last_updated"`
}

// ReindexRequestDetailed represents a row in the reindex_request_detailed table
type ReindexRequestDetailed struct {
	ID          int       `db:"id"`
	RequestID   int       `db:"request_id"`
	Index       string    `db:"index"`
	FromVersion string    `db:"from_version"`
	ToVersion   string    `db:"to_version"`
	Stage       string    `db:"stage"`
	OsTaskID    string    `db:"os_task_id"`
	Heartbeat   time.Time `db:"heartbeat"`
	HavingAlias bool      `db:"having_alias"`
	AliasList   string    `db:"alias_list"`
	CreatedAt   time.Time `db:"created_at"`
	UpdatedAt   time.Time `db:"updated_at"`
}

// ConnectAndMigrate creates a new PostgreSQL connection, connects to the database server, and runs migrations
func ConnectAndMigrate(dbConf *config.Storage) (*DB, error) {
	dbConn, err := connect(dbConf)
	if err != nil {
		return nil, err
	}
	log.WithFields(log.Fields{
		"uri":    dbConf.URI,
		"schema": dbConf.SchemaPath,
	}).Debug("Initializing database")

	err = runMigrations(dbConf)
	if err != nil {
		return nil, errors.Wrap(err, "Migration failed")
	}

	db := &DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}
	return db, nil
}

// connect opens a connection to the database
func connect(dbConf *config.Storage) (*sql.DB, error) {
	log.WithFields(log.Fields{
		"uri": dbConf.URI,
	}).Debug("Connecting to PostgreSQL")

	dbconn, err := libdb.PGOpen(dbConf.URI)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to open database with uri: %s", dbConf.URI)
	}

	if dbConf.MaxIdleConns > 0 {
		dbconn.SetMaxIdleConns(dbConf.MaxIdleConns)
	}
	if dbConf.MaxOpenConns > 0 {
		dbconn.SetMaxOpenConns(dbConf.MaxOpenConns)
	}

	// ping database
	err = dbconn.Ping()
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to ping database with uri: %s", dbConf.URI)
	}

	return dbconn, nil
}

func runMigrations(dbConf *config.Storage) error {
	if err := migrator.Migrate(dbConf.URI, dbConf.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", dbConf.SchemaPath)
	}
	return nil
}
