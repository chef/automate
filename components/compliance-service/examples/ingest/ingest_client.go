package main

import (
	"flag"

	ingesttest "github.com/chef/automate/components/compliance-service/examples/ingest/test"
	"github.com/sirupsen/logrus"
)

const (
	address = "localhost:10121"
)

func main() {
	filePtr := flag.String("file", "", "File to upload")
	threadCount := flag.Int("threads", 1, "Number of threads to use when sending the reports")
	reportsPerThread := flag.Int("reports-per-thread", 1, "Number of reports to send for each thread")

	flag.Parse()
	logrus.Infof("Using sample %s to send %d reports over %d threads", *filePtr, *reportsPerThread*(*threadCount), *threadCount)

	if err := ingesttest.SendReportToGRPC(*filePtr, *threadCount, *reportsPerThread); err != nil {
		logrus.Fatal(err)
	}
}
