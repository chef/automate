package storage

import (
	"errors"
	"testing"
	"time"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/go-gorp/gorp"
	"github.com/stretchr/testify/assert"
)

func setupMockDB(t *testing.T) (*DB, sqlmock.Sqlmock) {
	mockDB, mock, err := sqlmock.New()
	assert.NoError(t, err)
	dbMap := &DB{DbMap: &gorp.DbMap{Db: mockDB}}
	return dbMap, mock
}

// Test InsertReindexRequest
func TestInsertReindexRequest_Success(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("INSERT INTO reindex_requests").WillReturnResult(sqlmock.NewResult(1, 1))
	err := db.InsertReindexRequest(1, "running")
	assert.NoError(t, err)
}

func TestInsertReindexRequest_Failure(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("INSERT INTO reindex_requests").WillReturnError(errors.New("db error"))
	err := db.InsertReindexRequest(1, "running")
	assert.Error(t, err)
}

// Test InsertReindexRequestDetailed
func TestInsertReindexRequestDetailed_Success(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("INSERT INTO reindex_request_detailed").WillReturnResult(sqlmock.NewResult(1, 1))
	detail := ReindexRequestDetailed{RequestID: 1, Index: "test_index", Stage: "processing"}
	err := db.InsertReindexRequestDetailed(detail)
	assert.NoError(t, err)
}

func TestInsertReindexRequestDetailed_Failure(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("INSERT INTO reindex_request_detailed").WillReturnError(errors.New("db error"))
	detail := ReindexRequestDetailed{RequestID: 1, Index: "test_index", Stage: "processing"}
	err := db.InsertReindexRequestDetailed(detail)
	assert.Error(t, err)
}

// Test UpdateReindexRequest
func TestUpdateReindexRequest_Success(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("UPDATE reindex_requests").WillReturnResult(sqlmock.NewResult(1, 1))
	err := db.UpdateReindexRequest(1, "completed")
	assert.NoError(t, err)
}

func TestUpdateReindexRequest_Failure(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("UPDATE reindex_requests").WillReturnError(errors.New("db error"))
	err := db.UpdateReindexRequest(1, "completed")
	assert.Error(t, err)
}

// Test DeleteReindexRequest
func TestDeleteReindexRequest_Success(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("DELETE FROM reindex_requests").WillReturnResult(sqlmock.NewResult(1, 1))
	err := db.DeleteReindexRequest(1)
	assert.NoError(t, err)
}

func TestDeleteReindexRequest_Failure(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectExec("DELETE FROM reindex_requests").WillReturnError(errors.New("db error"))
	err := db.DeleteReindexRequest(1)
	assert.Error(t, err)
}

// Test GetReindexStatus
func TestGetReindexStatus_Success(t *testing.T) {
	db, mock := setupMockDB(t)
	rowsRequest := sqlmock.NewRows([]string{"request_id", "status", "created_at", "last_updated"}).AddRow(1, "running", time.Now(), time.Now())
	rowsDetails := sqlmock.NewRows([]string{"id", "request_id", "index", "stage"}).AddRow(1, 1, "test_index", "processing")

	mock.ExpectQuery("SELECT request_id, status").WillReturnRows(rowsRequest)
	mock.ExpectQuery("SELECT id, request_id, index, stage").WillReturnRows(rowsDetails)

	request, details, statusJSON, err := db.GetReindexStatus(1)
	assert.NoError(t, err)
	assert.NotNil(t, request)
	assert.NotNil(t, details)
	assert.NotEmpty(t, statusJSON)
}

func TestGetReindexStatus_NoRequestFound(t *testing.T) {
	db, mock := setupMockDB(t)
	mock.ExpectQuery("SELECT request_id, status").WillReturnRows(sqlmock.NewRows([]string{"request_id", "status"}))

	_, _, _, err := db.GetReindexStatus(1)
	assert.Error(t, err)
}

func TestGetReindexStatus_NoDetailsFound(t *testing.T) {
	db, mock := setupMockDB(t)
	rowsRequest := sqlmock.NewRows([]string{"request_id", "status", "created_at", "last_updated"}).AddRow(1, "running", time.Now(), time.Now())
	mock.ExpectQuery("SELECT request_id, status").WillReturnRows(rowsRequest)
	mock.ExpectQuery("SELECT id, request_id, index, stage").WillReturnRows(sqlmock.NewRows([]string{"id", "request_id", "index", "stage"}))

	request, details, statusJSON, err := db.GetReindexStatus(1)
	assert.NoError(t, err)
	assert.NotNil(t, request)
	assert.Empty(t, details)
	assert.NotEmpty(t, statusJSON)
}
