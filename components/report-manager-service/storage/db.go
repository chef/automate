package storage

import (
	"database/sql"
	"fmt"
	"time"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/report-manager-service/config"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type DB struct {
	*gorp.DbMap
}

// ConnectAndMigrate creates a new Postgres connection, connects to the database server and runs
// the migrations
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

	db := &DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	return db, err
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

func (db *DB) InsertTask(id, requestorID, status string, createdTime, updatedTime time.Time) error {
	_, err := db.Exec(insertTask, id, requestorID, status, createdTime, updatedTime)
	if err != nil {
		err = fmt.Errorf("error in executing the insert task: %w", err)
	}
	return err
}

func (db *DB) UpdateTask(id, status, msg string, updatedTime time.Time) error {
	_, err := db.Exec(updateTask, status, msg, updatedTime, id)
	if err != nil {
		err = fmt.Errorf("error in executing the update task: %w", err)
	}
	return err
}

const insertTask = `
INSERT INTO custom_report_requests(id, requestor, status,created_at,updated_at)
VALUES ($1, $2, $3, $4, $5);
`
const updateTask = `
UPDATE custom_report_requests SET status = $1, message = $2, updated_at = $3 WHERE id = $4;
`
