package server

import (
	"context"
	"fmt"
	"strings"
	"time"

	pb "github.com/golang/protobuf/ptypes/empty"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/api/interservice/ingest"
	compliance "github.com/chef/automate/components/compliance-service/api/status"
)

// Amount of time to wait for a single service to report its status
const a1MigrationStatusTimeout = 10 * time.Second

// Amount of time to wait between re-polling services for their
// migration status.
const a1MigrationStatusPollInterval = 2 * time.Second

func (s *server) A1UpgradeStatus(_ *api.A1UpgradeStatusRequest, stream api.Deployment_A1UpgradeStatusServer) error {
	ingestConn, err := s.connFactory.Dial("ingest-service", s.AddressForService("ingest-service"), grpc.WithBlock())
	if err != nil {
		return status.Errorf(codes.Unavailable, "unable to initialize connection to ingest-service: %s", err.Error())
	}
	defer ingestConn.Close() // nolint: errcheck

	ingestClient := ingest.NewIngestStatusClient(ingestConn)

	complianceConn, err := s.connFactory.Dial("compliance-service", s.AddressForService("compliance-service"), grpc.WithBlock())
	if err != nil {
		return status.Errorf(codes.Unavailable, "unable to initialize connection to compliance-service: %s", err.Error())
	}
	defer complianceConn.Close() // nolint: errcheck

	complianceClient := compliance.NewComplianceStatusClient(complianceConn)

	for {
		resp := gatherA1MigrationStatuses(ingestClient, complianceClient)
		err = stream.Send(resp)
		if err != nil {
			return err
		}
		if resp.OverallStatus != api.A1UpgradeStatusResponse_IN_PROGRESS {
			break
		}
		time.Sleep(a1MigrationStatusPollInterval)
	}

	return nil
}

// We have a finite number of phase 2 migrations.  For now, rather
// than being generic, we just explicitly gather each one we know
// about.
func gatherA1MigrationStatuses(ingest ingest.IngestStatusClient, compliance compliance.ComplianceStatusClient) *api.A1UpgradeStatusResponse {
	resp := &api.A1UpgradeStatusResponse{}

	resp.ServiceStatuses = make([]*api.A1UpgradeStatusResponse_ServiceMigrationStatus, 2)

	ingestStatus := getIngestMigrationStatus(ingest)
	complianceStatus := getComplianceMigrationStatus(compliance)

	resp.ServiceStatuses[0] = ingestStatus
	resp.ServiceStatuses[1] = complianceStatus

	resp.OverallStatus = overallStatus(ingestStatus, complianceStatus)

	return resp
}

func overallStatus(statuses ...*api.A1UpgradeStatusResponse_ServiceMigrationStatus) api.A1UpgradeStatusResponse_MigrationStatus {
	anyFailed := false
	anyUnknown := false
	allTerminal := true
	for _, s := range statuses {
		switch s.Status {
		case api.A1UpgradeStatusResponse_UNKNOWN:
			anyUnknown = true
		case api.A1UpgradeStatusResponse_IN_PROGRESS:
			allTerminal = false
		case api.A1UpgradeStatusResponse_COMPLETE:
			continue
		case api.A1UpgradeStatusResponse_FAILED:
			anyFailed = true
		}
	}

	if !allTerminal {
		return api.A1UpgradeStatusResponse_IN_PROGRESS
	}

	if allTerminal && anyFailed {
		return api.A1UpgradeStatusResponse_FAILED
	}

	if allTerminal && anyUnknown {
		return api.A1UpgradeStatusResponse_UNKNOWN
	}
	return api.A1UpgradeStatusResponse_COMPLETE
}

func getIngestMigrationStatus(ingestClient ingest.IngestStatusClient) *api.A1UpgradeStatusResponse_ServiceMigrationStatus {
	logrus.Debug("Checking migration status of ingest service")

	ctx, cancel := context.WithTimeout(context.Background(), a1MigrationStatusTimeout)
	defer cancel()

	ingestResp, err := ingestClient.GetMigrationStatus(ctx, &ingest.MigrationStatusRequest{})
	if err != nil {
		logrus.WithError(err).Error("A1 Migration status check for ingest-service failed")
		return &api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			ServiceName: "ingest-service",
			Status:      api.A1UpgradeStatusResponse_UNKNOWN,
			Info:        err.Error(),
		}
	}

	logrus.WithFields(logrus.Fields{
		"status":    ingestResp.Status,
		"finished":  ingestResp.Finished,
		"total":     ingestResp.Total,
		"completed": ingestResp.Completed,
	}).Debug("A1 migration status check of ingest-service succeeded")

	return ingestResponseToServiceMigrationStatus(ingestResp)
}

func ingestResponseToServiceMigrationStatus(ingestResp *ingest.MigrationStatus) *api.A1UpgradeStatusResponse_ServiceMigrationStatus {
	ret := &api.A1UpgradeStatusResponse_ServiceMigrationStatus{
		ServiceName: "ingest-service",
	}

	finished := ingestResp.Finished
	if ingestResp.Total != 0 {
		ret.Progress = int32(float64(ingestResp.Completed) / float64(ingestResp.Total) * 100)
	} else {
		finished = true
		ret.Progress = 100
	}

	if !finished {
		ret.Status = api.A1UpgradeStatusResponse_IN_PROGRESS
		ret.Info = fmt.Sprintf("(%d/%d) %s", ingestResp.Completed, ingestResp.Total, ingestResp.Status)
		return ret
	}

	if strings.HasPrefix(ingestResp.Status, "Error:") {
		ret.Status = api.A1UpgradeStatusResponse_FAILED
		ret.Info = ingestResp.Status
		return ret
	}

	ret.Status = api.A1UpgradeStatusResponse_COMPLETE
	ret.Info = ingestResp.Status
	return ret
}

func getComplianceMigrationStatus(client compliance.ComplianceStatusClient) *api.A1UpgradeStatusResponse_ServiceMigrationStatus {
	logrus.Debug("Checking migration status of compliance service")

	ctx, cancel := context.WithTimeout(context.Background(), a1MigrationStatusTimeout)
	defer cancel()

	complianceResp, err := client.GetMigrationStatus(ctx, &pb.Empty{})
	if err != nil {
		logrus.WithError(err).Error("A1 Migration status check for migration-service failed")
		return &api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			ServiceName: "compliance-service",
			Status:      api.A1UpgradeStatusResponse_UNKNOWN,
			Info:        err.Error(),
		}
	}

	logrus.WithFields(logrus.Fields{
		"status":    complianceResp.Status,
		"total":     complianceResp.Total,
		"completed": complianceResp.Completed,
	}).Debug("A1 migration status check of compliance-service succeeded")

	return complianceResponseToServiceMigrationStatus(complianceResp)
}

// TODO(ssd) 2018-05-11: The eventual goal is to normalize all three
// of these types to match the one we developed in
// compliance-service. For now, we smoosh things to the lowest common
// denominator.
func complianceResponseToServiceMigrationStatus(complianceResp *compliance.MigrationStatus) *api.A1UpgradeStatusResponse_ServiceMigrationStatus {
	ret := &api.A1UpgradeStatusResponse_ServiceMigrationStatus{
		ServiceName: "compliance-service",
	}

	if complianceResp.Total != 0 {
		ret.Progress = int32(float64(complianceResp.Completed) / float64(complianceResp.Total) * 100)
	} else {
		ret.Progress = 100
	}

	var lastLog string
	if len(complianceResp.Logs) > 0 {
		lastLog = complianceResp.Logs[len(complianceResp.Logs)-1].Text
	}

	switch complianceResp.Status {
	case compliance.MigrationStatus_UNKNOWN:
		ret.Status = api.A1UpgradeStatusResponse_UNKNOWN
		ret.Info = lastLog
	case compliance.MigrationStatus_RUNNING:
		ret.Status = api.A1UpgradeStatusResponse_IN_PROGRESS
		ret.Info = fmt.Sprintf("(%d/%d) %s", complianceResp.Completed, complianceResp.Total, lastLog)
	case compliance.MigrationStatus_FINISHED:
		ret.Status = api.A1UpgradeStatusResponse_COMPLETE
	case compliance.MigrationStatus_FAILED:
		ret.Status = api.A1UpgradeStatusResponse_FAILED
	case compliance.MigrationStatus_SKIPPED:
		ret.Status = api.A1UpgradeStatusResponse_COMPLETE
	}

	return ret
}
