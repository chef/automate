package storage

import (
	"encoding/json"
	"fmt"
	"testing"
	"time"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/stretchr/testify/assert"
)

func TestInsertReindexRequestSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	createdAt := time.Now()

	query := `INSERT INTO reindex_requests(status, created_at, last_updated) VALUES ($1, $2, $3) RETURNING id;`
	mock.ExpectQuery(query).WithArgs("running", createdAt, createdAt).WillReturnRows(sqlmock.NewRows([]string{"id"}).AddRow(1))

	requestID, err := db.InsertReindexRequest("running", createdAt)
	assert.NoError(t, err)
	assert.NotZero(t, requestID)
}

func TestInsertReindexRequestFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	createdAt := time.Now()

	query := `INSERT INTO reindex_requests(status, created_at, last_updated) VALUES ($1, $2, $3) RETURNING id;`
	mock.ExpectQuery(query).WithArgs("running", createdAt, createdAt).WillReturnError(fmt.Errorf("insert error"))

	_, err = db.InsertReindexRequest("running", createdAt)
	assert.Equal(t, "failed to insert reindex request: insert error", err.Error())
}

func TestUpdateReindexRequestSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	updatedAt := time.Now()

	query := `UPDATE reindex_requests SET status = $1, last_updated = $2 WHERE id = $3;`
	mock.ExpectExec(query).WithArgs("completed", updatedAt, 1).WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.UpdateReindexRequest(1, "completed", updatedAt)
	assert.NoError(t, err)
}

func TestUpdateReindexRequestFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	updatedAt := time.Now()

	query := `UPDATE reindex_requests SET status = $1, last_updated = $2 WHERE id = $3;`
	mock.ExpectExec(query).WithArgs("completed", updatedAt, 1).WillReturnError(fmt.Errorf("update error"))

	err = db.UpdateReindexRequest(1, "completed", updatedAt)
	assert.Equal(t, "update error", err.Error())
}

func TestInsertReindexRequestDetailedSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	detail := ReindexRequestDetailed{
		RequestID:   1,
		Index:       "index1",
		FromVersion: "1.0",
		ToVersion:   "2.0",
		Stage:       []StageDetail{{Stage: "stage1", Status: "running", UpdatedAt: time.Now()}},
		OsTaskID:    "task1",
		Heartbeat:   time.Now(),
		HavingAlias: true,
		AliasList:   "alias1,alias2",
		CreatedAt:   time.Now(),
		UpdatedAt:   time.Now(),
	}

	stageJSON, err := json.Marshal(detail.Stage)
	assert.NoError(t, err)

	// Mock the query to fetch existing stages
	selectQuery := `SELECT stage FROM reindex_request_detailed WHERE request_id = $1 AND index = $2 ORDER BY updated_at DESC LIMIT 1`
	mock.ExpectQuery(selectQuery).WithArgs(detail.RequestID, detail.Index).
		WillReturnRows(sqlmock.NewRows([]string{"stage"}).AddRow(`[]`))

	// Mock the insert query
	query := `INSERT INTO reindex_request_detailed(request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);`
	mock.ExpectExec(query).WithArgs(detail.RequestID, detail.Index, detail.FromVersion, detail.ToVersion, stageJSON, detail.OsTaskID, sqlmock.AnyArg(), detail.HavingAlias, detail.AliasList, sqlmock.AnyArg(), sqlmock.AnyArg()).WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.InsertReindexRequestDetailed(detail, detail.CreatedAt)
	assert.NoError(t, err)
}

func TestInsertReindexRequestDetailedFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	detail := ReindexRequestDetailed{
		RequestID:   1,
		Index:       "index1",
		FromVersion: "1.0",
		ToVersion:   "2.0",
		Stage:       []StageDetail{{Stage: "stage1", Status: "running", UpdatedAt: time.Now()}},
		OsTaskID:    "task1",
		Heartbeat:   time.Now(),
		HavingAlias: true,
		AliasList:   "alias1,alias2",
		CreatedAt:   time.Now(),
		UpdatedAt:   time.Now(),
	}

	stageJSON, err := json.Marshal(detail.Stage)
	assert.NoError(t, err)

	// Mock the query to fetch existing stages
	selectQuery := `SELECT stage FROM reindex_request_detailed WHERE request_id = $1 AND index = $2 ORDER BY updated_at DESC LIMIT 1`
	mock.ExpectQuery(selectQuery).WithArgs(detail.RequestID, detail.Index).
		WillReturnRows(sqlmock.NewRows([]string{"stage"}).AddRow(`[]`))

	// Mock the insert query to return an error
	query := `INSERT INTO reindex_request_detailed(request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);`
	mock.ExpectExec(query).WithArgs(detail.RequestID, detail.Index, detail.FromVersion, detail.ToVersion, stageJSON, detail.OsTaskID, sqlmock.AnyArg(), detail.HavingAlias, detail.AliasList, sqlmock.AnyArg(), sqlmock.AnyArg()).WillReturnError(fmt.Errorf("insert error"))

	err = db.InsertReindexRequestDetailed(detail, detail.CreatedAt)
	assert.Equal(t, "insert error", err.Error())
}

func TestDeleteReindexRequestSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	query := `DELETE FROM reindex_requests WHERE id = $1;`
	mock.ExpectExec(query).WithArgs(1).WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.DeleteReindexRequest(1)
	assert.NoError(t, err)
}

func TestDeleteReindexRequestFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	query := `DELETE FROM reindex_requests WHERE id = $1;`
	mock.ExpectExec(query).WithArgs(1).WillReturnError(fmt.Errorf("delete error"))

	err = db.DeleteReindexRequest(1)
	assert.Equal(t, "delete error", err.Error())
}

func TestDeleteReindexRequestDetailSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	query := `DELETE FROM reindex_request_detailed WHERE id = $1;`
	mock.ExpectExec(query).WithArgs(1).WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.DeleteReindexRequestDetail(1)
	assert.NoError(t, err)
}

func TestDeleteReindexRequestDetailFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	query := `DELETE FROM reindex_request_detailed WHERE id = $1;`
	mock.ExpectExec(query).WithArgs(1).WillReturnError(fmt.Errorf("delete error"))

	err = db.DeleteReindexRequestDetail(1)
	assert.Equal(t, "delete error", err.Error())
}

func TestGetReindexStatusSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	requestID := 1
	createdAt := time.Now()
	updatedAt := time.Now()
	heartbeat := time.Now()

	// Mock the reindex_requests table query
	requestQuery := `SELECT id, status, created_at, last_updated FROM reindex_requests WHERE id = $1 ORDER BY last_updated DESC LIMIT 1;`
	requestColumns := []string{"id", "status", "created_at", "last_updated"}
	mock.ExpectQuery(requestQuery).WithArgs(requestID).
		WillReturnRows(sqlmock.NewRows(requestColumns).AddRow(requestID, "completed", createdAt, updatedAt))

	// Mock the reindex_request_detailed table query
	detailQuery := `SELECT id, request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at FROM reindex_request_detailed WHERE request_id = $1 ORDER BY updated_at DESC;`
	detailColumns := []string{"id", "request_id", "index", "from_version", "to_version", "stage", "os_task_id", "heartbeat", "having_alias", "alias_list", "created_at", "updated_at"}
	mock.ExpectQuery(detailQuery).WithArgs(requestID).
		WillReturnRows(sqlmock.NewRows(detailColumns).
			AddRow(1, requestID, "index1", "1.0", "2.0", `[{"stage":"stage1","status":"running","updated_at":"`+updatedAt.Format(time.RFC3339)+`"}]`, "task1", heartbeat, true, "alias1,alias2", createdAt, updatedAt).
			AddRow(2, requestID, "index2", "2.0", "3.0", `[{"stage":"stage1","status":"completed","updated_at":"`+updatedAt.Format(time.RFC3339)+`"}]`, "task2", heartbeat, false, "", createdAt, updatedAt))

	// Call the function
	statusResponse, err := db.GetReindexStatus(requestID)
	assert.NoError(t, err)

	// Log the JSON response
	statusJSON, err := json.Marshal(statusResponse)
	assert.NoError(t, err)
	t.Log("Generated JSON Response:", string(statusJSON))

	// Verify the JSON response
	expectedJSON := `{"request_id":1,"status":"running","indexes":[{"index":"index1","stage":"stage1","status":"running"},{"index":"index2","stage":"stage1","status":"completed"}]}`
	assert.JSONEq(t, expectedJSON, string(statusJSON))
}

func TestGetReindexStatusFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	requestID := 1

	// Mock the reindex_requests table query to return an error
	requestQuery := `SELECT id, status, created_at, last_updated FROM reindex_requests WHERE id = $1 ORDER BY last_updated DESC LIMIT 1;`
	mock.ExpectQuery(requestQuery).WithArgs(requestID).WillReturnError(fmt.Errorf("query error"))

	// Call the function
	_, err = db.GetReindexStatus(requestID)

	// Assertions
	assert.Error(t, err)
	assert.Equal(t, "error fetching reindex request status from db: query error", err.Error())
}

func TestGetReindexStatusNoDetails(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	requestID := 1
	createdAt := time.Now()
	updatedAt := time.Now()

	// Mock the reindex_requests table query
	requestQuery := `SELECT id, status, created_at, last_updated FROM reindex_requests WHERE id = $1 ORDER BY last_updated DESC LIMIT 1;`
	requestColumns := []string{"id", "status", "created_at", "last_updated"}
	mock.ExpectQuery(requestQuery).WithArgs(requestID).
		WillReturnRows(sqlmock.NewRows(requestColumns).AddRow(requestID, "completed", createdAt, updatedAt))

	// Mock the reindex_request_detailed table query to return no rows
	detailQuery := `SELECT id, request_id, index, from_version, to_version, stage, os_task_id, heartbeat, having_alias, alias_list, created_at, updated_at FROM reindex_request_detailed WHERE request_id = $1 ORDER BY updated_at DESC;`
	detailColumns := []string{"id", "request_id", "index", "from_version", "to_version", "stage", "os_task_id", "heartbeat", "having_alias", "alias_list", "created_at", "updated_at"}
	mock.ExpectQuery(detailQuery).WithArgs(requestID).
		WillReturnRows(sqlmock.NewRows(detailColumns))

	// Call the function
	statusResponse, err := db.GetReindexStatus(requestID)

	// Assertions
	assert.NoError(t, err)

	// Verify the JSON response
	statusJSON, err := json.Marshal(statusResponse)
	assert.NoError(t, err)
	expectedJSON := `{"request_id":1,"status":"completed","indexes":[]}`
	assert.JSONEq(t, expectedJSON, string(statusJSON))
}

func TestUpdateAliasesForIndex(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}
	time := time.Now()
	query := `UPDATE reindex_request_detailed SET having_alias = $1, alias_list = $2, updated_at = $3 WHERE request_id = $4 AND index = $5;`
	mock.ExpectExec(query).WithArgs(true, "test,test2", time, 1, "reindexing").WillReturnError(fmt.Errorf("update error"))

	err = db.UpdateAliasesForIndex("reindexing", true, []string{"test", "test2"}, 1, time)
	assert.EqualError(t, err, "update error")
}

func TestUpdateAliasesForIndexSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}
	time := time.Now()

	query := `UPDATE reindex_request_detailed SET having_alias = $1, alias_list = $2, updated_at = $3 WHERE request_id = $4 AND index = $5;`
	mock.ExpectExec(query).WithArgs(true, "test1,test2", time, 1, "reindexing").WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.UpdateAliasesForIndex("reindexing", true, []string{"test1", "test2"}, 1, time)
	assert.NoError(t, err)
}

func TestUpdateAliasesForIndexNoAliases(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}
	time := time.Now()

	query := `UPDATE reindex_request_detailed SET having_alias = $1, alias_list = $2, updated_at = $3 WHERE request_id = $4 AND index = $5;`
	mock.ExpectExec(query).WithArgs(false, "", time, 1, "reindexing").WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.UpdateAliasesForIndex("reindexing", false, []string{""}, 1, time)
	assert.NoError(t, err)
}

func TestUpdateTaskIDForReindexRequestSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	currentTime := time.Now()
	requestID := 1
	indexName := "index1"
	taskID := "task123"

	query := `
        UPDATE reindex_request_detailed
        SET os_task_id = $1, updated_at = $2
        WHERE request_id = $3 AND "index" = $4
    `
	mock.ExpectExec(query).
		WithArgs(taskID, currentTime, requestID, indexName).
		WillReturnResult(sqlmock.NewResult(1, 1)) // Simulates 1 row affected

	err = db.UpdateTaskIDForReindexRequest(requestID, indexName, taskID, currentTime)
	assert.NoError(t, err)
}

func TestUpdateTaskIDForReindexRequestNoRowsFound(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	currentTime := time.Now()
	requestID := 1
	indexName := "index1"
	taskID := "task123"

	query := `
        UPDATE reindex_request_detailed
        SET os_task_id = $1, updated_at = $2
        WHERE request_id = $3 AND "index" = $4
    `
	mock.ExpectExec(query).
		WithArgs(taskID, currentTime, requestID, indexName).
		WillReturnResult(sqlmock.NewResult(0, 0)) // Simulates no rows affected

	err = db.UpdateTaskIDForReindexRequest(requestID, indexName, taskID, currentTime)
	assert.Error(t, err)
	assert.Equal(t, fmt.Sprintf("no matching record found for request_id: %d, index: %s", requestID, indexName), err.Error())
}

func TestUpdateTaskIDForReindexRequestQueryFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	currentTime := time.Now()
	requestID := 1
	indexName := "index1"
	taskID := "task123"

	query := `
        UPDATE reindex_request_detailed
        SET os_task_id = $1, updated_at = $2
        WHERE request_id = $3 AND "index" = $4
    `
	mock.ExpectExec(query).
		WithArgs(taskID, currentTime, requestID, indexName).
		WillReturnError(fmt.Errorf("database error")) // Simulates a DB failure

	err = db.UpdateTaskIDForReindexRequest(requestID, indexName, taskID, currentTime)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "failed to update task ID")
}

func TestDB_CreateOrUpdateStageAndStatusForIndex(t *testing.T) {

	t.Run("Sucessful update", func(t *testing.T) {
		dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
		assert.NoError(t, err)
		defer dbConn.Close()

		db := DB{
			DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
		}

		requestID := 1
		index := "test_index"
		stage := "stage1"
		status := "completed"
		updateTime := time.Now()

		// Mock the query to fetch existing stages
		stageJSON := `[{"stage":"stage1","status":"running","updated_at":"2025-03-28T12:00:00Z"}]`
		mock.ExpectQuery(`Select stage from reindex_request_detailed where request_id = $1 and index = $2`).
			WithArgs(requestID, index).
			WillReturnRows(sqlmock.NewRows([]string{"stage"}).AddRow(stageJSON))

		// Mock the update query
		mock.ExpectExec(`update reindex_request_detailed set stage = $1 where request_id = $2 and index = $3`).
			WithArgs(sqlmock.AnyArg(), requestID, index).
			WillReturnResult(sqlmock.NewResult(1, 1))

		err = db.CreateOrUpdateStageAndStatusForIndex(requestID, index, stage, status, updateTime)
		assert.NoError(t, err)
	})

	t.Run("Error in getting data already presnet", func(t *testing.T) {
		dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
		assert.NoError(t, err)
		defer dbConn.Close()

		db := DB{
			DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
		}

		requestID := 1
		index := "test_index"
		stage := "stage1"
		status := "completed"
		updateTime := time.Now()

		// Mock the query to fetch existing stages (simulate query failure)
		mock.ExpectQuery(`Select stage from reindex_request_detailed where request_id = $1 and index = $2`).
			WithArgs(requestID, index).
			WillReturnError(assert.AnError)

		err = db.CreateOrUpdateStageAndStatusForIndex(requestID, index, stage, status, updateTime)
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "Unable to get the stage for requestId")
	})

	t.Run("Error in Updating the data", func(t *testing.T) {
		dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
		assert.NoError(t, err)
		defer dbConn.Close()

		db := DB{
			DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
		}

		requestID := 1
		index := "test_index"
		stage := "stage1"
		status := "completed"
		updateTime := time.Now()

		// Mock the query to fetch existing stages
		stageJSON := `[{"stage":"stage1","status":"running","updated_at":"2025-03-28T12:00:00Z"}]`
		mock.ExpectQuery(`Select stage from reindex_request_detailed where request_id = $1 and index = $2`).
			WithArgs(requestID, index).
			WillReturnRows(sqlmock.NewRows([]string{"stage"}).AddRow(stageJSON))

		// Mock the update query (simulate update failure)
		mock.ExpectExec(`update reindex_request_detailed set stage = $1 where request_id = $2 and index = $3`).
			WithArgs(sqlmock.AnyArg(), requestID, index).
			WillReturnError(assert.AnError)

		err = db.CreateOrUpdateStageAndStatusForIndex(requestID, index, stage, status, updateTime)
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "Unable to executed update query")
	})

}

func Test_getUpdatedStageDetails(t *testing.T) {
	updateTime := time.Now()
	type args struct {
		stageDetails []*StageDetail
		stage        string
		status       string
		updateTime   time.Time
	}
	tests := []struct {
		name               string
		args               args
		resultStageDetails []*StageDetail
	}{
		{
			name: "Adding new stage if empty",
			args: args{
				stageDetails: []*StageDetail{},
				stage:        "stage1",
				status:       "running",
				updateTime:   updateTime,
			},
			resultStageDetails: []*StageDetail{
				{
					Stage:     "stage1",
					Status:    "running",
					UpdatedAt: updateTime,
				},
			},
		},
		{
			name: "Update a stage if present in the stage details",
			args: args{
				stageDetails: []*StageDetail{{
					Stage:     "stage1",
					Status:    "running",
					UpdatedAt: updateTime,
				}},
				stage:      "stage1",
				status:     "completed",
				updateTime: updateTime,
			},
			resultStageDetails: []*StageDetail{
				{
					Stage:     "stage1",
					Status:    "completed",
					UpdatedAt: updateTime,
				},
			},
		},
		{
			name: "Add a stage if not present",
			args: args{
				stageDetails: []*StageDetail{{
					Stage:     "stage1",
					Status:    "running",
					UpdatedAt: updateTime,
				}},
				stage:      "stage2",
				status:     "completed",
				updateTime: updateTime,
			},
			resultStageDetails: []*StageDetail{
				{
					Stage:     "stage1",
					Status:    "running",
					UpdatedAt: updateTime,
				},
				{
					Stage:     "stage2",
					Status:    "completed",
					UpdatedAt: updateTime,
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			stageDetails := getUpdatedStageDetails(tt.args.stageDetails, tt.args.stage, tt.args.status, tt.args.updateTime)
			assert.Equal(t, tt.resultStageDetails, stageDetails)

		})
	}
}
