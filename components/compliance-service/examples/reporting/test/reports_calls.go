package reportingtest

import (
	"context"
	"io"
	"log"

	rs "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
)

const (
	address = "localhost:10121"
)

func Export(query *rs.Query) (*rs.ExportData, error) {
	// Set up a connection to the server.
	connFactory := helpers.SecureConnFactory()
	conn, err := connFactory.Dial("compliance-service", address)
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)

	var tdata *rs.ExportData
	stream2, err2 := reporting.Export(context.Background(), query)
	if err2 != nil {
		log.Fatalf("could not export: %v", err2)
	}
	for {
		data, err := stream2.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatalf("%v.Export(_) = _, %v", data, err)
		}
		tdata = data
	}
	return tdata, nil
}
