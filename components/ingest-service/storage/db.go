package storage

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/chef/automate/components/ingest-service/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
)

type DB struct {
	*gorp.DbMap
}

// ReindexRequest represents the reindex_requests table
type ReindexRequest struct {
	RequestID   int       `db:"request_id"`
	Status      string    `db:"status"`
	CreatedAt   time.Time `db:"created_at"`
	LastUpdated time.Time `db:"last_updated"`
}

// ReindexRequestDetailed represents the reindex_request_detailed table
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

func RunMigrations(dbConf *config.Storage) error {
	if err := migrator.Migrate(dbConf.URI, dbConf.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", dbConf.SchemaPath)
	}
	return nil
}

// CRUD Operations

// Create a new reindex request
func (db *DB) InsertReindexRequest(requestID int, status string, currentTime time.Time) error {
	_, err := db.Exec(insertReindexRequest, requestID, status, currentTime, currentTime)
	return err
}

// Update an existing reindex request
func (db *DB) UpdateReindexRequest(requestID int, status string, currentTime time.Time) error {
	_, err := db.Exec(updateReindexRequest, status, currentTime, requestID)
	return err
}

// Insert reindex request detailed entry
func (db *DB) InsertReindexRequestDetailed(detail ReindexRequestDetailed, currentTime time.Time) error {
	_, err := db.Exec(insertReindexRequestDetailed, detail.RequestID, detail.Index, detail.FromVersion, detail.ToVersion, detail.Stage, detail.OsTaskID, detail.Heartbeat, detail.HavingAlias, detail.AliasList, currentTime, currentTime)
	return err
}

// Delete a reindex request
func (db *DB) DeleteReindexRequest(requestID int) error {
	_, err := db.Exec(deleteReindexRequest, requestID)
	return err
}

// Delete a reindex request detail
func (db *DB) DeleteReindexRequestDetail(id int) error {
	_, err := db.Exec(deleteReindexRequestDetail, id)
	return err
}

// Get reindex request status
func (db *DB) GetReindexStatus(requestID int) ([]*ReindexRequest, []*ReindexRequestDetailed, string, error) {
	// Fetch the latest reindex request
	var request []*ReindexRequest
	_, err := db.Select(&request, getLatestReindexRequest, requestID)
	if err != nil {
		return nil, nil, "", errors.Wrap(err, "error fetching reindex request status from db")
	}

	// Handle case where no records are found
	if len(request) == 0 {
		return nil, nil, "", errors.New("no reindex request found for the given requestID")
	}

	// Fetch the latest reindex request details for each index
	var details []*ReindexRequestDetailed
	_, err = db.Select(&details, getLatestReindexRequestDetails, requestID)
	if err != nil {
		return nil, nil, "", errors.Wrap(err, "error fetching reindex request details from db")
	}

	// Determine the overall status based on the stages of the individual indexes
	overallStatus := "completed"
	for _, detail := range details {
		if detail.Stage == "failed" {
			overallStatus = "failed"
			break
		} else if detail.Stage == "running" && overallStatus != "failed" {
			overallStatus = "running"
		}
	}

	// Prepare the status response
	status := map[string]interface{}{
		"overall_status": overallStatus, // overall status of the reindex request
		"indexes":        []map[string]string{},
	}

	for _, detail := range details {
		status["indexes"] = append(status["indexes"].([]map[string]string), map[string]string{
			"index": detail.Index,
			"stage": detail.Stage,
		})
	}

	// Marshal the status to JSON
	statusJSON, err := json.Marshal(status)
	if err != nil {
		return nil, nil, "", errors.Wrap(err, "error marshalling reindex status to JSON")
	}
	fmt.Println("********Generated JSON Response:", string(statusJSON))
	return request, details, string(statusJSON), nil
}

// SQL Queries
const insertReindexRequest = `
INSERT INTO reindex_requests(request_id, status, created_at, last_updated)
VALUES ($1, $2, $3, $4);`

const updateReindexRequest = `
UPDATE reindex_requests SET status = $1, last_updated = $2 WHERE request_id = $3;`

const getLatestReindexRequest = `
SELECT request_id, status, created_at, last_updated 
FROM reindex_requests 
WHERE request_id = $1 
ORDER BY last_updated DESC 
LIMIT 1;`

const insertReindexRequestDetailed = `
INSERT INTO reindex_request_detailed(request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);`

const getLatestReindexRequestDetails = `
SELECT DISTINCT ON (index) id, request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at 
FROM reindex_request_detailed 
WHERE request_id = $1 
ORDER BY index, updated_at DESC;`

const deleteReindexRequest = `
DELETE FROM reindex_requests WHERE request_id = $1;`

const deleteReindexRequestDetail = `
DELETE FROM reindex_request_detailed WHERE id = $1;`
