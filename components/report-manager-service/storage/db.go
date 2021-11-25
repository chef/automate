package storage

import (
	"database/sql"
	"fmt"
	"time"

	"github.com/go-gorp/gorp"
	"github.com/golang/protobuf/ptypes"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/config"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type DB struct {
	*gorp.DbMap
}

// customReportRequestStatus used to read custom report request status from DB
type customReportRequestStatus struct {
	ID         string    `db:"id"`
	Status     string    `db:"status"`
	Message    string    `db:"message"`
	ReportSize int64     `db:"custom_report_size"`
	StartTime  time.Time `db:"created_at"`
	EndTime    time.Time `db:"updated_at"`
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

func (db *DB) InsertTask(id, requestorID, status, reportType string, createdTime, updatedTime time.Time) error {
	_, err := db.Exec(insertTask, id, requestorID, status, reportType, createdTime, updatedTime)
	if err != nil {
		err = fmt.Errorf("error in executing the insert task: %w", err)
	}
	return err
}

func (db *DB) UpdateTask(id, status, msg, preSignedURL string, updatedTime time.Time, objSize int64) error {
	_, err := db.Exec(updateTask, status, msg, objSize, preSignedURL, updatedTime, id)
	if err != nil {
		err = fmt.Errorf("error in executing the update task: %w", err)
	}
	return err
}

func (db *DB) GetAllStatus(id string, endTime time.Time) (*report_manager.AllStatusResponse, error) {
	var dbResp []*customReportRequestStatus
	resp := report_manager.AllStatusResponse{}
	_, err := db.Select(&dbResp, getStatus, id, endTime)
	if err != nil {
		return nil, errors.Wrap(err, "error in fetching the report request status from db")
	}
	for _, reportStatus := range dbResp {
		createdAt, err := ptypes.TimestampProto(reportStatus.StartTime)
		if err != nil {
			return nil, errors.Wrapf(err, "error in converting the created at with value %s to timestamppb.Timestamp", reportStatus.StartTime)
		}
		endedAt, err := ptypes.TimestampProto(reportStatus.EndTime)
		if err != nil {
			return nil, errors.Wrapf(err, "error in converting the ended at with value %s to timestamppb.Timestamp", reportStatus.EndTime)
		}
		resp.Data = append(resp.Data, &report_manager.StatusResponse{
			AcknowledgementId: reportStatus.ID,
			Status:            reportStatus.Status,
			ReportSize:        reportStatus.ReportSize,
			ErrMessage:        reportStatus.Message,
			CreatedAt:         createdAt,
			EndedAt:           endedAt,
		})
	}
	return &resp, nil
}

// PreSignedURLResponse used to read custom report's presigned URL and other details from DB
type PreSignedURLResponse struct {
	PresignedURL string `db:"custom_report_url"`
	ReportType   string `db:"custom_report_type"`
	ReportSize   int64  `db:"custom_report_size"`
}

func (db *DB) GetPreSignedURL(id, requestorID string) (*report_manager.GetPresignedURLResponse, error) {
	var dbResp []*PreSignedURLResponse
	resp := report_manager.GetPresignedURLResponse{}
	_, err := db.Select(&dbResp, getPreSignedURL, id, requestorID)
	if err != nil {
		return nil, errors.Wrap(err, "error in fetching the presigned url from db")
	}
	if len(dbResp) > 1 {
		return nil, fmt.Errorf("multiple reports are available for given id and requestor")
	}
	for _, item := range dbResp {
		resp.Url = item.PresignedURL
		resp.ReportType = item.ReportType
		resp.ReportSize = item.ReportSize
	}
	return &resp, nil
}

const insertTask = `
INSERT INTO custom_report_requests(id, requestor, status, custom_report_type, created_at, updated_at)
VALUES ($1, $2, $3, $4, $5, $6);
`
const updateTask = `
UPDATE custom_report_requests SET status = $1, message = $2, custom_report_size = $3, custom_report_url = $4, updated_at = $5 WHERE id = $6;
`

const getStatus = `
SELECT id, status, message, custom_report_size, created_at, updated_at FROM custom_report_requests WHERE requestor = $1 AND created_at >= $2 ORDER BY created_at DESC;
`
const getPreSignedURL string = `
SELECT custom_report_url, custom_report_type, custom_report_size FROM custom_report_requests where id = $1 and requestor = $2;
`
