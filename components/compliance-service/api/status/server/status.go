package status

import (
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/workflow"

	"github.com/golang/protobuf/ptypes"
	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"

	"github.com/chef/automate/components/compliance-service/api/status"
)

// Server struct
type Server struct {
	MigrationStatus  *status.MigrationStatus
	MigrationChannel chan status.LogEntry
}

// New creates a new instance of Server
func New(workflowManager *workflow.WorkflowManager) *Server {
	migrationChannel := make(chan status.LogEntry)
	migrationStatus := &status.MigrationStatus{
		Completed: 0,
		Total:     status.TotalMigrationSteps,
		Status:    status.MigrationStatus_RUNNING,
	}
	thisServer := &Server{
		MigrationStatus:  migrationStatus,
		MigrationChannel: migrationChannel,
	}
	go listenForMigrationUpdates(migrationChannel, workflowManager, thisServer)
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
		if value == status.MigrationCompletedMsg || value == status.MigrationFailedMsg {
			completeMigration += 1
		}

		if value == status.MigrationFailedMsg {
			anyFailed = true
		}
	}
	return completeMigration == status.MaxMigrations, anyFailed
}

func addEntry(entry status.LogEntry, statusSrv *Server, labelMap map[string]string) bool {
	logrus.Info("HERE")
	labelMap[entry.Label] = entry.Text
	completed, failed := migrationsStatus(labelMap)
	if entry.Text == status.MigrationFailedMsg || entry.Text == status.MigrationCompletedMsg {
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
	return completed
}

// listenForMigrationUpdates uses a LogEntry channel to listen for migration updates
// coming from the various goroutines that are migrating at the same time.
// It exits when all migrations (labels) have concluded
func listenForMigrationUpdates(ch chan status.LogEntry, workflowManager *workflow.WorkflowManager, statusSrv *Server) {
	// a map that stores the most recent migration message per migration
	labelMap := make(map[string]string, 0)
OUTER:
	for {
		select {
		case entry := <-ch:
			completed := addEntry(entry, statusSrv, labelMap)
			if completed {
				break OUTER
			}
		case <-time.After(5 * time.Second):
			entries, err := relaxting.GetMigrationLogs(context.TODO(), workflowManager)
			if err != nil {
				logrus.WithError(err).Error("failed to get migration logs")
				continue
			}
			for _, entry := range entries {
				if labelMap[entry.Label] == entry.Text {
					logrus.Info("Doing some skipping")
					continue
				}
				logrus.Info("Not skipping")
				ts, _ := ptypes.TimestampProto(entry.Timestamp)
				labelMap[entry.Label] = entry.Text
				completed := addEntry(status.LogEntry{
					Label:     entry.Label,
					Text:      entry.Text,
					Timestamp: ts,
				}, statusSrv, labelMap)
				if completed {
					break OUTER
				}
			}
		}

	}
	close(ch)

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
