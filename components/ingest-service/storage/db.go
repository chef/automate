package storage

import (
	"database/sql"
	"time"

	"github.com/chef/automate/components/ingest-service/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
)

// DB struct encapsulating database operations

type DB struct {
	DbMap *gorp.DbMap
	Conn  *sql.DB
}

// ReindexRequest represents a record in the reindex_requests table
type ReindexRequest struct {
	RequestID   int       `db:"request_id"`
	Status      string    `db:"status"`
	CreatedAt   time.Time `db:"created_at"`
	LastUpdated time.Time `db:"last_updated"`
}

// ReindexRequestDetails represents a record in the reindex_request_detailed table
type ReindexRequestDetails struct {
	ID          int        `db:"id"`
	RequestID   int        `db:"request_id"`
	Index       string     `db:"index"`
	FromVersion string     `db:"from_version"`
	ToVersion   string     `db:"to_version"`
	Stage       string     `db:"stage"`
	OSTaskID    *string    `db:"os_task_id"`
	Heartbeat   *time.Time `db:"heartbeat"`
	HavingAlias bool       `db:"having_alias"`
	AliasList   *string    `db:"alias_list"`
	CreatedAt   time.Time  `db:"created_at"`
	UpdatedAt   time.Time  `db:"updated_at"`
}

// InsertDummyData inserts a new dummy record into the reindex_requests table
func (db *DB) InsertDummyData(req *ReindexRequest) error {
	query := `
		INSERT INTO reindex_requests (status, created_at, last_updated)
		VALUES ($1, $2, $3)
		RETURNING request_id;
	`
	err := db.Conn.QueryRow(query, req.Status, time.Now(), time.Now()).Scan(&req.RequestID)
	if err != nil {
		return errors.Wrap(err, "failed to insert dummy data into reindex_requests")
	}
	return nil
}

// InsertDummyDetailsData inserts a new dummy record into the reindex_request_detailed table
func (db *DB) InsertDummyDetailsData(req *ReindexRequestDetails) error {
	query := `
		INSERT INTO reindex_request_detailed (request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);
	`
	_, err := db.Conn.Exec(query, req.RequestID, req.Index, req.FromVersion, req.ToVersion, req.Stage, req.OSTaskID, req.Heartbeat, req.HavingAlias, req.AliasList, time.Now(), time.Now())
	if err != nil {
		return errors.Wrap(err, "failed to insert dummy data into reindex_request_detailed")
	}
	return nil
}

// RunMigrations runs database migration scripts

func RunMigrations(dbConf *config.Storage) error {
	if err := migrator.Migrate(dbConf.URI, dbConf.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", dbConf.SchemaPath)
	}

	return nil
}
