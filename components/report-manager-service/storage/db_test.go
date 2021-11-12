package storage_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/go-gorp/gorp"
	"github.com/golang/protobuf/ptypes"
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

func TestGetAllStatus(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := &storage.DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}
	endedAt := time.Now()
	createdAt := endedAt.Add(-10 * time.Minute)
	endTime := endedAt.Add(-24 * time.Hour)

	endedAtProto, err := ptypes.TimestampProto(endedAt)
	assert.NoError(t, err)

	createdAtProto, err := ptypes.TimestampProto(createdAt)
	assert.NoError(t, err)

	query := `SELECT id, status, message, custom_report_size, created_at, updated_at FROM custom_report_requests WHERE requestor = $1 AND created_at >= $2 ORDER BY created_at DESC;`

	t.Run("TestGetAllStatus_Success", func(t *testing.T) {
		columns := []string{"id", "status", "message", "custom_report_size", "created_at", "updated_at"}

		mock.ExpectQuery(query).WithArgs("reqID", endTime).
			WillReturnRows(sqlmock.NewRows(columns).AddRow("1", "success", "", 1024*1024, createdAt, endedAt).
				AddRow("2", "failed", "error in running task", 0, createdAt, endedAt).AddRow("3", "running", "", 0, createdAt, createdAt))

		resp, err := db.GetAllStatus("reqID", endTime)
		assert.NoError(t, err)
		assert.Equal(t, 3, len(resp.Data))

		assert.Equal(t, "1", resp.Data[0].AcknowledgementId)
		assert.Equal(t, "success", resp.Data[0].Status)
		assert.Equal(t, int64(1048576), resp.Data[0].ReportSize)
		assert.Equal(t, createdAtProto, resp.Data[0].CreatedAt)
		assert.Equal(t, endedAtProto, resp.Data[0].EndedAt)
		assert.Equal(t, "failed", resp.Data[1].Status)
		assert.Equal(t, "error in running task", resp.Data[1].ErrMessage)
		assert.Equal(t, "running", resp.Data[2].Status)
	})

	t.Run("TestGetAllStatus_Failed", func(t *testing.T) {
		mock.ExpectQuery(query).WithArgs("reqID", endTime).
			WillReturnError(fmt.Errorf("error from db"))

		_, err := db.GetAllStatus("reqID", endTime)
		assert.Error(t, err)
		assert.Equal(t, "error in fetching the report request status from db: error from db", err.Error())
	})
}
