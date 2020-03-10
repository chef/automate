package ingesttest

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/jsonpb"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
)

func SendReportToGRPC(file string, threadCount int, reportsPerThread int) error {
	fileData, err := os.Open(file)
	if err != nil {
		logrus.Errorf("ingest_call.go - Unable to read file: %s", file)
		return err
	}
	defer fileData.Close()

	connFactory := helpers.SecureConnFactory()
	reportEndpoint := fmt.Sprintf("%s:%d", "127.0.0.1", 10121)
	logrus.Infof("Handing over report to ingest service %s", reportEndpoint)
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Minute)
	defer cancel()
	conn, err := connFactory.DialContext(ctx, "compliance-service", reportEndpoint, grpc.WithBlock())
	if err != nil {
		logrus.Errorf("ingest_call.go - Unable grpc dial: %s", err)
		return err
	}
	logrus.Infof("Connection to GRPC successful, sending report(s) now...")

	defer conn.Close()

	var iReport compliance.Report
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	if err = unmarshaler.Unmarshal(fileData, &iReport); err != nil {
		logrus.Errorf("ingest_call.go - reportToGRPC was unable to unmarshal the report output into a compliance.Report struct: %s", err.Error())
		return err
	}

	client := ingest.NewComplianceIngesterClient(conn)
	if client == nil {
		logrus.Errorf("ingest_call.go - reportToGRPC got nil for NewComplianceIngesterClient: %s", err)
		return err
	}
	logrus.Info("-------------------------------------------------------")
	deadline, _ := ctx.Deadline()

	onesie := true
	if threadCount > 1 || reportsPerThread > 1 {
		onesie = false
	}

	logrus.Infof("Ingest context deadline=%s", deadline.UTC().Format(time.RFC3339))
	threadsProgress := make([]int, threadCount)
	for i := 1; i <= threadCount; i++ {
		go SendThreadedReports(iReport, onesie, i, reportsPerThread, threadsProgress, client, ctx)
	}

	// Waiting for all threads to send the reports
	for completed := false; completed == false; {
		isCompleted := true
		for i := 0; i < threadCount; i++ {
			if threadsProgress[i] < reportsPerThread {
				isCompleted = false
				break
			}
		}
		completed = isCompleted
		time.Sleep(time.Second)
	}
	return nil
}

// SendThreadedReports is sequentially sending `reportsPerThread` reports on behalf of thread `threadNr`
func SendThreadedReports(iReport compliance.Report, onesie bool, threadNr int, reportsPerThread int, threadsProgress []int, client ingest.ComplianceIngesterClient, ctx context.Context) {
	for j := 1; j <= reportsPerThread; j++ {
		SendUniqueReport(iReport, onesie, threadNr, j, client, ctx)
		threadsProgress[threadNr-1] = j
	}
}

// SendUniqueReport is sending one report with ReportUuid and EndTime modified
// It measures the execution time and reports success of failure
func SendUniqueReport(iReport compliance.Report, onesie bool, threadNr int, reportNr int, client ingest.ComplianceIngesterClient, ctx context.Context) {
	// If the report is ingested more than once, we change the report id and time
	// We also change the node id to avoid ES ingest 409 conflicts
	if !onesie {
		iReport.ReportUuid = uuid.Must(uuid.NewV4()).String()
		iReport.NodeUuid = uuid.Must(uuid.NewV4()).String()
		iReport.EndTime = time.Now().UTC().Format(time.RFC3339)
	}
	start := time.Now()
	_, err := client.ProcessComplianceReport(ctx, &iReport)
	elapsed := time.Since(start)
	if err != nil {
		logrus.Errorf("ingest_call.go - Failed to send report %s, took %s, error: %s", iReport.ReportUuid, elapsed.Truncate(time.Millisecond), err)
		return
	}
	logrus.Infof("Thread %d, successfully ingested report %d (%s) in %s", threadNr, reportNr, iReport.ReportUuid, elapsed.Truncate(time.Millisecond))
}
