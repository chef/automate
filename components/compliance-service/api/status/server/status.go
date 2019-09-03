package status

import (
	"context"
	"time"

	"github.com/golang/protobuf/ptypes"
	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/api/status"
)

// List of labels used to differentiate between migrations that can run independent of each other
const (
	MigrationLabelESa1   = "ElasticSearch_A1"
	MigrationLabelESa2v1 = "ElasticSearch_A2_v1"
	MigrationLabelESa2v2 = "ElasticSearch_A2_v2"
	MigrationLabelESa2v3 = "ElasticSearch_A2_v3"
	MigrationLabelPG     = "PostgreSQL"
	MigrationLabelPRO    = "Profiles"
	//MigrationLabelFEEDS = "ElasticSearch_Feeds_1"
)

const maxMigrations = 6        // Total migrations should match the number of constants above
const totalMigrationSteps = 26 // Max number of migration LogEntry items we can have across all migrations

// Special message sent by the services to flag the end of a migration either failed or successful
const MigrationFailedMsg = "FAILED"
const MigrationCompletedMsg = "COMPLETED"

// Server struct
type Server struct {
	MigrationStatus  *status.MigrationStatus
	MigrationChannel chan status.LogEntry
}

// New creates a new instance of Server
func New() *Server {
	migrationChannel := make(chan status.LogEntry)
	migrationStatus := &status.MigrationStatus{
		Completed: 0,
		Total:     totalMigrationSteps,
		Status:    status.MigrationStatus_RUNNING,
	}
	thisServer := &Server{
		MigrationStatus:  migrationStatus,
		MigrationChannel: migrationChannel,
	}
	go listenForMigrationUpdates(migrationChannel, thisServer)
	return thisServer
}

// GetMigrationStatus returns the migration status of the service
func (srv *Server) GetMigrationStatus(ctx context.Context, empty *pb.Empty) (*status.MigrationStatus, error) {
	return srv.MigrationStatus, nil
}

// migrationsStatus uses the migration labelMap to return
// completed=true if all migrations (labels) have finished successfully
// failed=true if at least one migration (label) has failed
func migrationsStatus(labelMap map[string]string) (bool, bool) {
	anyFailed := false
	completeMigration := 0
	for _, value := range labelMap {
		if value == MigrationCompletedMsg || value == MigrationFailedMsg {
			completeMigration += 1
		}

		if value == MigrationFailedMsg {
			anyFailed = true
		}
	}
	return completeMigration == maxMigrations, anyFailed
}

// listenForMigrationUpdates uses a LogEntry channel to listen for migration updates
// coming from the various goroutines that are migrating at the same time.
// It exits when all migrations (labels) have concluded
func listenForMigrationUpdates(ch chan status.LogEntry, statusSrv *Server) {
	// a map that stores the most recent migration message per migration
	labelMap := make(map[string]string, 0)
	for {
		entry := <-ch
		labelMap[entry.Label] = entry.Text
		completed, failed := migrationsStatus(labelMap)
		if entry.Text == MigrationFailedMsg || entry.Text == MigrationCompletedMsg {
			entry.Text = entry.Label + " ended with status " + entry.Text
			statusSrv.MigrationStatus.Logs = append(statusSrv.MigrationStatus.Logs, &entry)
		} else {
			statusSrv.MigrationStatus.Logs = append(statusSrv.MigrationStatus.Logs, &entry)
		}
		statusSrv.MigrationStatus.Completed = int64(len(statusSrv.MigrationStatus.Logs))
		logrus.Infof("Migrations running (%d/%d) %s: %s", statusSrv.MigrationStatus.Completed, statusSrv.MigrationStatus.Total, entry.Label, entry.Text)
		if failed {
			// Set the overall status to failed but
			// continue to collect other migration data
			// until we've heard back from everything.
			statusSrv.MigrationStatus.Status = status.MigrationStatus_FAILED
		}

		if completed {
			close(ch)
			break
		}
	}

	statusSrv.MigrationStatus.Completed = statusSrv.MigrationStatus.Total
	if statusSrv.MigrationStatus.Status == status.MigrationStatus_FAILED {
		logrus.Info("Migrations failed")
	} else {
		statusSrv.MigrationStatus.Status = status.MigrationStatus_FINISHED
		logrus.Info("Migrations complete")
	}
}

// AddMigrationUpdate is the entrypoint for sending updates about running migrations or finishing them
// It receives the update and places it into the LogEntry channel
func AddMigrationUpdate(migServer *Server, logLabel string, logText string) {
	timeNow, err := ptypes.TimestampProto(time.Now())
	if err != nil {
		logrus.Errorf("AddMigrationUpdate encountered error while getting time: %s", err.Error())
		return
	}
	migServer.MigrationChannel <- status.LogEntry{
		Label:     logLabel,
		Text:      logText,
		Timestamp: timeNow,
	}
}
