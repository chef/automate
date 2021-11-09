package storage_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/go-gorp/gorp"
	"github.com/stretchr/testify/assert"
)

func TestInsertTaskSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := &storage.DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	createdTime := time.Now()

	query := `INSERT INTO custom_report_requests(id, requestor, status,created_at,updated_at) VALUES ($1, $2, $3, $4, $5);`
	mock.ExpectExec(query).WithArgs("id", "test", "running", createdTime, createdTime).WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.InsertTask("id", "test", "running", createdTime, createdTime)
	assert.NoError(t, err)
}

func TestInsertTaskFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := &storage.DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	createdTime := time.Now()

	query := `INSERT INTO custom_report_requests(id, requestor, status,created_at,updated_at) VALUES ($1, $2, $3, $4, $5);`
	mock.ExpectExec(query).WithArgs("id", "test", "running", createdTime, createdTime).WillReturnError(fmt.Errorf("insert error"))

	err = db.InsertTask("id", "test", "running", createdTime, createdTime)
	assert.Equal(t, "error in executing the insert task: insert error", err.Error())
}

func TestUpdateTaskSuccess(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := &storage.DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	updatedTime := time.Now()

	query := `UPDATE custom_report_requests SET status = $1, message = $2, custom_report_size = $3, updated_at = $4 WHERE id = $5;`
	mock.ExpectExec(query).WithArgs("status", "message", 1024, updatedTime, "id").WillReturnResult(sqlmock.NewResult(1, 1))

	err = db.UpdateTask("id", "status", "message", updatedTime, 1024)
	assert.NoError(t, err)
}

func TestUpdateTaskFailure(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := &storage.DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	updatedTime := time.Now()

	query := `UPDATE custom_report_requests SET status = $1, message = $2, custom_report_size = $3, updated_at = $4 WHERE id = $5;`
	mock.ExpectExec(query).WithArgs("status", "message", 0, updatedTime, "id").WillReturnError(fmt.Errorf("update error"))

	err = db.UpdateTask("id", "status", "message", updatedTime, 0)
	assert.Equal(t, "error in executing the update task: update error", err.Error())
}
