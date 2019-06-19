package reportingtest

import (
	"context"
	"encoding/csv"
	"encoding/json"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	rs "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
	"github.com/chef/automate/lib/grpc/grpctest"
)

const (
	address = "localhost:10121"
)

func getClientConn() (*grpc.ClientConn, error) {
	// Set up a connection to the server.
	connFactory := helpers.SecureConnFactory()
	return connFactory.Dial("compliance-service", address)
}

func TestJSONExportWithEndTime(t *testing.T) {
	// get reporting client
	conn, err := getClientConn()
	require.NoError(t, err)

	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)
	require.NoError(t, err)

	// test with only end_time
	query := rs.Query{
		Type: "json",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
		},
	}
	stream, err := reporting.Export(context.Background(), &query)
	require.NoError(t, err)

	data := make([]byte, 0)
	//the gateway wraps the response in [] because it's a json array
	//since we are not using the gateway in this test, we need to do that wrapping
	//here's the '[' (open wrapper)
	data = append([]byte("["), data...)
	for {
		tdata, err := stream.Recv()
		if err != nil && err == io.EOF {
			data = append(data, tdata.GetContent()...)
			break
		}

		require.NoError(t, err)
		data = append(data, tdata.GetContent()...)
	}
	//and here's the ']' (close wrapper)
	data = append(data, []byte("]")...)

	var reports []rs.Report
	err = json.Unmarshal(data, &reports)
	require.NoError(t, err)

	report := reports[0]
	assert.Equal(t, 3, len(report.GetProfiles()))
	assert.Equal(t, "bb93e1b2-36d6-439e-ac70-cccccccccc04", report.GetId())
	assert.Equal(t, "10.3.4.5", report.Ipaddress)
	assert.Equal(t, "web-cent.example.com", report.Fqdn)
}

func TestJSONExportWithProfileFilter(t *testing.T) {
	// get reporting client
	conn, err := getClientConn()
	require.NoError(t, err)

	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)
	require.NoError(t, err)

	// test with a single profile filter
	profileFilterQuery := rs.Query{
		Type: "json",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
			{Type: "profile_id", Values: []string{"09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"}},
		},
	}

	stream, err := reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	data := make([]byte, 0)
	//the gateway wraps the response in [] because it's a json array
	//since we are not using the gateway in this test, we need to do that wrapping
	//here's the '[' (open wrapper)
	data = append([]byte("["), data...)
	for {
		tdata, err := stream.Recv()
		if err != nil && err == io.EOF {
			data = append(data, tdata.GetContent()...)
			break
		}

		require.NoError(t, err)
		data = append(data, tdata.GetContent()...)
	}
	//and here's the ']' (close wrapper)
	data = append(data, []byte("]")...)

	var reports []rs.Report
	err = json.Unmarshal(data, &reports)
	require.NoError(t, err)

	report := reports[0]
	assert.Equal(t, 1, len(report.GetProfiles()))
	assert.Equal(t, "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988", report.GetProfiles()[0].GetSha256())
	assert.Equal(t, "bb93e1b2-36d6-439e-ac70-cccccccccc04", report.GetId())
}

func TestJSONExportWithTwoProfileFiltersReturnsError(t *testing.T) {
	// get reporting client
	conn, err := getClientConn()
	require.NoError(t, err)

	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)
	require.NoError(t, err)

	// test with a two profile_id filters
	profileFilterQuery := rs.Query{
		Type: "json",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
			{Type: "profile_id", Values: []string{"09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"}},
			{Type: "profile_id", Values: []string{"41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9"}},
		},
	}

	stream, err := reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	_, err = stream.Recv()

	assert.Equal(t, "rpc error: code = InvalidArgument desc = Invalid: Only one 'profile_id' filter is allowed", err.Error())

	// test with a two profile_name filters
	profileFilterQuery = rs.Query{
		Type: "json",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
			{Type: "profile_name", Values: []string{"apache-baseline"}},
			{Type: "profile_name", Values: []string{"fake-baseline"}},
		},
	}

	stream, err = reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	_, err = stream.Recv()

	assert.Equal(t, "rpc error: code = InvalidArgument desc = Invalid: Only one 'profile_name' filter is allowed", err.Error())

	// test with a profile_id and profile_name filter
	profileFilterQuery = rs.Query{
		Type: "json",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
			{Type: "profile_id", Values: []string{"09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"}},
			{Type: "profile_name", Values: []string{"fake-baseline"}},
		},
	}

	stream, err = reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	_, err = stream.Recv()

	assert.Equal(t, "rpc error: code = InvalidArgument desc = Invalid: Cannot specify both 'profile_name' and 'profile_id' filters", err.Error())
}

func TestCSVExportWithEndTime(t *testing.T) {
	// get reporting client
	conn, err := getClientConn()
	require.NoError(t, err)

	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)
	require.NoError(t, err)

	// test with a single profile filter
	profileFilterQuery := rs.Query{
		Type: "csv",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
		},
	}

	stream, err := reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	data := make([]byte, 0)
	for {
		tdata, err := stream.Recv()
		if err != nil && err == io.EOF {
			data = append(data, tdata.GetContent()...)
			break
		}

		require.NoError(t, err)
		data = append(data, tdata.GetContent()...)
	}

	// translate byte array to string, then read with csv package
	r := csv.NewReader(strings.NewReader(string(data)))

	records, err := r.ReadAll()
	require.NoError(t, err)
	require.Equal(t, 20, len(records))

	assert.Equal(t, []string{"centos-beta", "2018-03-04T09:18:41Z", "centos", "5.11", "DevSec Prod beta", "10.3.4.5", "web-cent.example.com", "nginx-baseline", "DevSec Nginx Baseline", "2.1.0", "Test-suite for best-practice nginx hardening", "nginx-01", "Running worker process as non-privileged user", "1.00", "passed", "0.000", "Worked like a charm baby!", "", ""}, records[1])
}

func TestCSVExportWithEndTimeAndMissingFields(t *testing.T) {
	// get reporting client
	conn, err := getClientConn()
	require.NoError(t, err)

	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)
	require.NoError(t, err)

	// test with a single profile filter
	profileFilterQuery := rs.Query{
		Type: "csv",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T23:18:41Z"}},
			{Type: "node_id", Values: []string{"a0ddd774-cbbb-49be-8730-49c92f3fc2a0"}},
		},
	}

	stream, err := reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	data := make([]byte, 0)
	for {
		tdata, err := stream.Recv()
		if err != nil && err == io.EOF {
			data = append(data, tdata.GetContent()...)
			break
		}

		require.NoError(t, err)
		data = append(data, tdata.GetContent()...)
	}

	// translate byte array to string, then read with csv package
	r := csv.NewReader(strings.NewReader(string(data)))

	records, err := r.ReadAll()
	require.NoError(t, err)
	require.Equal(t, 15, len(records))

	assert.Equal(t, []string{"windows(1)-zeta-apache(s)-skipped", "2018-03-04T10:18:41Z", "windows", "7", "DevSec Prod Zeta", "", "", "apache-baseline", "DevSec Apache Baseline", "2.0.1", "Test-suite for best-practice apache hardening", "apache-01", "Apache should be running", "1.00", "skipped", "0.000", "Operating System Detection", "", "Skipped control due to only_if condition."}, records[1])
}

func TestJSONExportWithControlFilter(t *testing.T) {
	// get reporting client
	conn, err := getClientConn()
	require.NoError(t, err)

	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)
	require.NoError(t, err)

	// test with a single control filter
	profileFilterQuery := rs.Query{
		Type: "json",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
			{Type: "control", Values: []string{"nginx-02"}},
			{Type: "profile_id", Values: []string{"09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"}},
		},
	}

	stream, err := reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	data := make([]byte, 0)
	//the gateway wraps the response in [] because it's a json array
	//since we are not using the gateway in this test, we need to do that wrapping
	//here's the '[' (open wrapper)
	data = append([]byte("["), data...)
	for {
		tdata, err := stream.Recv()
		if err != nil && err == io.EOF {
			data = append(data, tdata.GetContent()...)
			break
		}

		require.NoError(t, err)
		data = append(data, tdata.GetContent()...)
	}
	//and here's the ']' (close wrapper)
	data = append(data, []byte("]")...)

	var reports []rs.Report
	err = json.Unmarshal(data, &reports)
	require.NoError(t, err)

	report := reports[0]
	assert.Equal(t, "bb93e1b2-36d6-439e-ac70-cccccccccc04", report.GetId())
	assert.Equal(t, 1, len(report.GetProfiles()))
	assert.Equal(t, "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988", report.GetProfiles()[0].GetSha256())

	assert.Equal(t, 1, len(report.GetProfiles()[0].GetControls()))
	assert.Equal(t, "nginx-02", report.GetProfiles()[0].GetControls()[0].GetId())
}

func TestJSONExportWithTwoControlFiltersReturnsError(t *testing.T) {
	// get reporting client
	conn, err := getClientConn()
	require.NoError(t, err)

	defer conn.Close()

	reporting := rs.NewReportingServiceClient(conn)
	require.NoError(t, err)

	// test with a two control filters
	profileFilterQuery := rs.Query{
		Type: "json",
		Filters: []*rs.ListFilter{
			{Type: "start_time", Values: []string{"2018-03-04T00:00:00Z"}},
			{Type: "end_time", Values: []string{"2018-03-04T09:18:41Z"}},
			{Type: "control", Values: []string{"nginx-01"}},
			{Type: "control", Values: []string{"nginx-02"}},
		},
	}

	stream, err := reporting.Export(context.Background(), &profileFilterQuery)
	require.NoError(t, err)

	_, err = stream.Recv()

	grpctest.AssertCode(t, codes.InvalidArgument, err)
	assert.Equal(t, "rpc error: code = InvalidArgument desc = Invalid: Only one 'control' filter is allowed", err.Error())
}
