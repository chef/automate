package storage

import (
	"database/sql"
	"fmt"
	"math/rand"
	"time"

	"github.com/chef/automate/components/ingest-service/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
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

// RunMigrations runs database migration scripts

func RunMigrations(dbConf *config.Storage) error {
	if err := migrator.Migrate(dbConf.URI, dbConf.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", dbConf.SchemaPath)
	}

	return nil
}

// CheckIfReindexInProgress checks if an active reindex request exists

func (db *DB) CheckIfReindexInProgress() (bool, error) {
	var count int
	query := `SELECT COUNT(*) FROM reindex_requests WHERE status = 'running';`
	err := db.Conn.QueryRow(query).Scan(&count)
	if err != nil {
		return false, fmt.Errorf("error checking reindex status: %w", err)
	}
	return count > 0, nil

}

// CreateReindexRequest inserts a new reindex request

func (db *DB) CreateReindexRequest() (int, error) {
	// Check if a reindex is already running
	inProgress, err := db.CheckIfReindexInProgress()
	if err != nil {
		return 0, err
	}

	if inProgress {
		return 0, fmt.Errorf("another reindex process is already running")
	}

	requestID := rand.Intn(10000) // Generate a random request ID
	status := "running"
	currentTime := time.Now()
	query := `
    INSERT INTO reindex_requests (request_id, status, created_at, last_updated)
    VALUES ($1, $2, $3, $4);
    `
	_, err = db.Conn.Exec(query, requestID, status, currentTime, currentTime)

	if err != nil {
		return 0, fmt.Errorf("error inserting reindex request: %w", err)
	}

	log.Infof("Reindex request created with ID: %d", requestID)

	return requestID, nil

}

// InsertReindexRequest inserts index details for a given request ID

func (db *DB) InsertReindexRequest(requestID int) error {
	indexes := []string{"index_1", "index_2", "index_3"}

	for _, index := range indexes {
		query := `
        INSERT INTO reindex_request_detailed
        (id, request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at)
        VALUES ($1, $2, $3, '1.0', '2.0', 'running', NULL, NULL, FALSE, NULL, $4, $4);
        `
		id := rand.Intn(10000) // Generate a random ID
		_, err := db.Conn.Exec(query, id, requestID, index, time.Now())

		if err != nil {
			return fmt.Errorf("error inserting reindex details: %w", err)
		}

	}

	log.Infof("Reindex request details inserted for request ID: %d", requestID)

	return nil

}

// InsertDummyData inserts a dummy reindex request and its details
func (db *DB) InsertDummyData() error {
	requestID, err := db.CreateReindexRequest()

	if err != nil {
		return err
	}

	err = db.InsertReindexRequest(requestID)

	if err != nil {
		return err
	}

	log.Info("Dummy data inserted successfully!")

	return nil

}

// GetReindexStatus retrieves the status of a reindex request

func (db *DB) GetReindexStatus(requestID int) (string, error) {
	var status string
	query := `SELECT status FROM reindex_requests WHERE request_id = $1;`
	err := db.Conn.QueryRow(query, requestID).Scan(&status)

	if err != nil {
		if err == sql.ErrNoRows {
			return "", fmt.Errorf("no reindex request found with ID: %d", requestID)
		}
		return "", fmt.Errorf("error fetching reindex status: %w", err)
	}
	log.Infof("Reindex request ID %d has status: %s", requestID, status)
	return status, nil

}

// FetchDummyData retrieves dummy data for verification

func (db *DB) FetchDummyData() error {
	query := `
    SELECT r.request_id, r.status, d.index, d.stage
    FROM reindex_requests r
    JOIN reindex_request_detailed d ON r.request_id = d.request_id;
    `
	rows, err := db.Conn.Query(query)

	if err != nil {
		return fmt.Errorf("error fetching dummy data: %w", err)
	}

	defer rows.Close()

	for rows.Next() {
		var requestID int
		var status, index, stage string
		err := rows.Scan(&requestID, &status, &index, &stage)

		if err != nil {
			return fmt.Errorf("error scanning dummy data: %w", err)
		}
		log.Infof("Dummy Data - Request ID: %d, Status: %s, Index: %s, Stage: %s", requestID, status, index, stage)

	}

	return nil

}

// UpdateReindexStatus updates the status of a reindex request

func (db *DB) UpdateReindexStatus(requestID int, newStatus string) error {

	query := `UPDATE reindex_requests SET status = $1, last_updated = $2 WHERE request_id = $3;`

	_, err := db.Conn.Exec(query, newStatus, time.Now(), requestID)

	if err != nil {
		return fmt.Errorf("error updating reindex status: %w", err)
	}

	log.Infof("Reindex request ID %d updated to status: %s", requestID, newStatus)

	return nil

}
