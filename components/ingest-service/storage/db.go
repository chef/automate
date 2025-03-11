package storage

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"math/rand"
	"sort"
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
	ID          int           `db:"id"`
	RequestID   int           `db:"request_id"`
	Index       string        `db:"index"`
	FromVersion string        `db:"from_version"`
	ToVersion   string        `db:"to_version"`
	Stage       []StageDetail `db:"stage"`
	OsTaskID    string        `db:"os_task_id"`
	Heartbeat   time.Time     `db:"heartbeat"`
	HavingAlias bool          `db:"having_alias"`
	AliasList   string        `db:"alias_list"`
	CreatedAt   time.Time     `db:"created_at"`
	UpdatedAt   time.Time     `db:"updated_at"`
}

type StageDetail struct {
	Stage     string    `json:"stage"`
	Status    string    `json:"status"`
	UpdatedAt time.Time `json:"updated_at"`
}

// StatusResponse represents the response structure for reindex status
type StatusResponse struct {
	RequestID int                 `json:"request_id"`
	Status    string              `json:"status"`
	Indexes   []IndexStatusDetail `json:"indexes"`
}

type IndexStatusDetail struct {
	Index  string `json:"index"`
	Stage  string `json:"stage"`
	Status string `json:"status"`
}

func RunMigrations(dbConf *config.Storage) error {
	if err := migrator.Migrate(dbConf.URI, dbConf.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", dbConf.SchemaPath)
	}
	return nil
}

// Create a new reindex request with a random request_id
func (db *DB) InsertReindexRequest(status string, currentTime time.Time) (int, error) {
	requestID := rand.Int()
	_, err := db.Exec(insertReindexRequest, requestID, status, currentTime, currentTime)
	return requestID, err
}

// Update an existing reindex request
func (db *DB) UpdateReindexRequest(requestID int, status string, currentTime time.Time) error {
	_, err := db.Exec(updateReindexRequest, status, currentTime, requestID)
	return err
}

// Insert reindex request detailed entry
func (db *DB) InsertReindexRequestDetailed(detail ReindexRequestDetailed, currentTime time.Time) error {
	var existingStages []StageDetail
	var stageJSON string

	// Fetch latest stage from DB
	err := db.SelectOne(&stageJSON, `SELECT stage FROM reindex_request_detailed WHERE request_id = $1 AND index = $2 ORDER BY updated_at DESC LIMIT 1`, detail.RequestID, detail.Index)
	if err != nil && err != sql.ErrNoRows {
		return errors.Wrap(err, "error fetching existing stages")
	}

	// Unmarshal existing stages if present
	if stageJSON != "" {
		if err := json.Unmarshal([]byte(stageJSON), &existingStages); err != nil {
			return errors.Wrap(err, "error unmarshalling existing stage JSON")
		}
	}

	// Append new stage properly
	existingStages = append(existingStages, detail.Stage...)

	// Convert back to JSON
	stageJSONBytes, err := json.Marshal(existingStages)
	if err != nil {
		return errors.Wrap(err, "error marshalling stage to JSON")
	}

	// Insert or update the reindex request detailed entry
	_, err = db.Exec(insertReindexRequestDetailed, detail.RequestID, detail.Index, detail.FromVersion, detail.ToVersion, stageJSONBytes, detail.OsTaskID, detail.Heartbeat, detail.HavingAlias, detail.AliasList, currentTime, currentTime)
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

// Get the latest reindex request status for a given request
func (db *DB) GetReindexStatus(requestID int) (*StatusResponse, error) {
	var request ReindexRequest
	err := db.SelectOne(&request, getLatestReindexRequest, requestID)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, errors.New("no reindex request found for the given requestID")
		}
		return nil, errors.Wrap(err, "error fetching reindex request status from db")
	}

	var details []ReindexRequestDetailed
	rows, err := db.Query(getLatestReindexRequestDetails, requestID)
	if err != nil {
		return nil, errors.Wrap(err, "error fetching reindex request details from db")
	}
	defer rows.Close()

	for rows.Next() {
		var detail ReindexRequestDetailed
		var stageJSON string
		err := rows.Scan(&detail.ID, &detail.RequestID, &detail.Index, &detail.FromVersion, &detail.ToVersion, &stageJSON, &detail.OsTaskID, &detail.Heartbeat, &detail.HavingAlias, &detail.AliasList, &detail.CreatedAt, &detail.UpdatedAt)
		if err != nil {
			return nil, errors.Wrap(err, "error scanning reindex request detail row")
		}

		err = json.Unmarshal([]byte(stageJSON), &detail.Stage)
		if err != nil {
			return nil, errors.Wrap(err, "error unmarshalling stage JSON")
		}

		details = append(details, detail)
	}

	// If no details are found, return a JSON response with an empty indexes array
	if len(details) == 0 {
		return &StatusResponse{
			RequestID: request.RequestID,
			Status:    request.Status,
			Indexes:   []IndexStatusDetail{},
		}, nil
	}

	// Determine the overall status based on the latest stage of individual indexes
	overallStatus := "completed"
	indexes := []IndexStatusDetail{}

	for _, detail := range details {
		if len(detail.Stage) == 0 {
			continue
		}

		// Sort the stages by UpdatedAt
		sort.Slice(detail.Stage, func(i, j int) bool {
			return detail.Stage[i].UpdatedAt.After(detail.Stage[j].UpdatedAt)
		})

		latestStage := detail.Stage[0]

		if latestStage.Status == "failed" {
			overallStatus = "failed"
			break
		} else if latestStage.Status == "running" && overallStatus != "failed" {
			overallStatus = "running"
		}

		indexes = append(indexes, IndexStatusDetail{
			Index:  detail.Index,
			Stage:  latestStage.Stage,
			Status: latestStage.Status,
		})
	}

	statusResponse := &StatusResponse{
		RequestID: request.RequestID,
		Status:    overallStatus,
		Indexes:   indexes,
	}

	fmt.Println("********Generated Response:", statusResponse)

	return statusResponse, nil
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
SELECT id, request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at 
FROM reindex_request_detailed 
WHERE request_id = $1 
ORDER BY updated_at DESC;`

const deleteReindexRequest = `
DELETE FROM reindex_requests WHERE request_id = $1;`

const deleteReindexRequestDetail = `
DELETE FROM reindex_request_detailed WHERE id = $1;`
