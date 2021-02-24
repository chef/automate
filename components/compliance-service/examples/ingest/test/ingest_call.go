package ingesttest

import (
	"context"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/jsonpb"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
	"path/filepath"
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

	json_file := filepath.Base(file)
	// Using the report filename convention to change the end_time of the report based on the current date and time
	// This is needed to test the last 24 hours functionality of Automate when an end_time is not specified for API calls
	if strings.HasPrefix(json_file, "NOW") {
		split_file := strings.Split(json_file, "_")
		if len(split_file) < 6 {
			logrus.Fatalf("Dynamic report file (%s) is invalid. starting with NOW must have at least 5 underscores. Example: NOW_MINUS_1440_PLUS_0060_osx(2)-omega-pro1(p)-passed.json", json_file)
		}
		new_end_date := time.Now()
		if split_file[1] == "MINUS" {
			first_minutes, err := strconv.Atoi(split_file[2])
			if err != nil {
				logrus.Fatalf("Can't convert '%s' to integer, aborting report %s", split_file[2], json_file)
			}
			second_minutes, err := strconv.Atoi(split_file[4])
			if err != nil {
				logrus.Fatalf("Can't convert '%s' to integer, aborting report %s", split_file[4], json_file)
			}
			new_end_date = new_end_date.Add(-time.Duration(first_minutes) * time.Minute)
			if split_file[3] == "MINUS" {
				new_end_date = new_end_date.Add(-time.Duration(second_minutes) * time.Minute)
			} else if split_file[3] == "PLUS" {
				if first_minutes == 1440 && second_minutes == 1 {
					// Need less than a minute, otherwise some tests run before this minute expires, creating flaky tests
					new_end_date = new_end_date.Add(15 * time.Second)
				} else {
					new_end_date = new_end_date.Add(time.Duration(second_minutes) * time.Minute)
				}
			} else {
				logrus.Fatalf("Don't understand operand %s when processing dynamic report %s. Only MINUS and PLUS are supported.", split_file[3], json_file)
			}
			iReport.EndTime = new_end_date.UTC().Format(time.RFC3339)
			logrus.Infof("[%s] Ingesting dynamic report (%s) with updated end_time (%s)", time.Now().UTC().Format(time.RFC3339), iReport.ReportUuid, iReport.EndTime)
		} else {
			logrus.Fatalf("'%s' not supported after NOW, only 'MINUS', aborting file %s", split_file[1], json_file)
		}
	}

	client := ingest.NewComplianceIngesterServiceClient(conn)
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
func SendThreadedReports(iReport compliance.Report, onesie bool, threadNr int, reportsPerThread int, threadsProgress []int, client ingest.ComplianceIngesterServiceClient, ctx context.Context) {
	for j := 1; j <= reportsPerThread; j++ {
		SendUniqueReport(iReport, onesie, threadNr, j, client, ctx)
		threadsProgress[threadNr-1] = j
	}
}

// SendUniqueReport is sending one report with ReportUuid and EndTime modified
// It measures the execution time and reports success of failure
func SendUniqueReport(iReport compliance.Report, onesie bool, threadNr int, reportNr int, client ingest.ComplianceIngesterServiceClient, ctx context.Context) {
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
