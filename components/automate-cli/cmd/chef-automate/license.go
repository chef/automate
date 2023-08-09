// Copyright © 2017 Chef Software

package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/spf13/cobra"
	grpc_codes "google.golang.org/grpc/codes"
	grpc_status "google.golang.org/grpc/status"

	"github.com/chef/automate/api/external/compliance/reporting"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	generator "github.com/chef/automate/components/automate-cli/pkg/reportgenerator"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	tslib "github.com/chef/automate/lib/grpc/timestamp"
)

var licenseCmd = &cobra.Command{
	Use:               "license COMMAND",
	Short:             "Chef Automate license management",
	PersistentPreRunE: preLicenseCmd,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var licenseStatusCmd = &cobra.Command{
	Use:               "status",
	Short:             "Retrieve Chef Automate license status",
	RunE:              runLicenseStatusCmd,
	PersistentPreRunE: preLicenseCmd,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var licenseApplyLong = `Apply Chef Automate license token.
	- <LICENSE> must be valid encoded license string
`

var licenseApplyCmd = &cobra.Command{
	Use:               "apply LICENSE",
	Short:             "Apply Chef Automate license",
	Long:              licenseApplyLong,
	RunE:              runLicenseApplyCmd,
	PersistentPreRunE: preLicenseCmd,
	Args:              cobra.ExactArgs(1),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var licenseUsageCmd = &cobra.Command{
	Use:    "usage",
	Short:  "Display node usage info for billing.",
	Long:   "Show usage for billing.",
	RunE:   runLicenseUsageCmd,
	Args:   cobra.RangeArgs(0, 1),
	Hidden: true,
}

var uniqueNodeCounterCmd = &cobra.Command{
	Use:   "uniqNodeRunReport",
	Short: "Generates the unique count of reported Infra Client nodes on hourly basis between the time duration",
	RunE: func(cmd *cobra.Command, args []string) error {
		validateArgs()
		startTime, _ := convertStringToTime(CommandFlags.StartTime)
		endTime, _ := convertStringToTime(CommandFlags.EndTime)
		generator.GenerateNodeCount(CommandFlags.ESHostname, CommandFlags.ESPort, CommandFlags.ESUserID, CommandFlags.ESPassword, startTime, endTime, CommandFlags.FileName)
		return nil
	},
	PersistentPreRunE: getPreLicenseReportCmd("nodecount"),
}

var nodeUsageCommand = &cobra.Command{
	Use:   "nodeRunReport",
	Short: "Generates daily Infra Client Run reports for a span of time duration",
	RunE: func(cmd *cobra.Command, args []string) error {
		validateArgs()
		startTime, _ := convertStringToTime(CommandFlags.StartTime)
		endTime, _ := convertStringToTime(CommandFlags.EndTime)
		generator.GenerateNodeRunReport(CommandFlags.ESHostname, CommandFlags.ESPort, CommandFlags.ESUserID, CommandFlags.ESPassword, startTime, endTime, CommandFlags.FileName)
		return nil
	},
	PersistentPreRunE: getPreLicenseReportCmd("nodeinfo"),
}

var complianceUniqueResourceCounterCmd = &cobra.Command{
	Use:   "complianceResourceRunCount",
	Short: "Generates the unique count of reported Compliance scans on hourly basis between the time duration",
	RunE: func(cmd *cobra.Command, args []string) error {
		validateArgs()
		startTime, _ := convertStringToTime(CommandFlags.StartTime)
		endTime, _ := convertStringToTime(CommandFlags.EndTime)
		generator.GenerateComplianceResourceRunCount(CommandFlags.ESHostname, CommandFlags.ESPort, CommandFlags.ESUserID, CommandFlags.ESPassword, startTime, endTime, CommandFlags.FileName)
		return nil
	},
	PersistentPreRunE: getPreLicenseReportCmd("complianceresourcecount"),
}

var complianceResourceUsageCmd = &cobra.Command{
	Use:   "complianceResourceRunReport",
	Short: "Generates daily Compliance scan reports for a span of time duration",
	RunE: func(cmd *cobra.Command, args []string) error {
		validateArgs()
		startTime, _ := convertStringToTime(CommandFlags.StartTime)
		endTime, _ := convertStringToTime(CommandFlags.EndTime)
		generator.GenerateComplianceResourceRunReport(CommandFlags.ESHostname, CommandFlags.ESPort, CommandFlags.ESUserID, CommandFlags.ESPassword, startTime, endTime, CommandFlags.FileName)
		return nil
	},
	PersistentPreRunE: getPreLicenseReportCmd("complianceresourceinfo"),
}

var noLicenseAppliedMsg = `
A license has not been associated with this installation of Chef
Automate. Please contact sales@chef.io to request a Chef Automate
license.

If you already have a license, please apply it by running the
following command:

chef-automate license apply <license>
`

var licenseInfoFmt = `

Licensed to:     %s
License ID:      %s
Expiration Date: %s

`

var startTimeFormat = "start time of the report in yyyy-mm-dd format"
var endTimeFormat = "end time of the report in yyyy-mm-dd format"
var hostnameES = "hostname of the OpenSource host"
var portES = "port of the OpenSource host"
var userNameES = "username of the OpenSource host"
var passwordES = "password of the OpenSource host"
var fileName = "file name for the report Ex: complianceUniqueResourceCount"

type usageResult struct {
	StartTimestamp   string           `json:"start_timestamp"`
	EndTimestamp     string           `json:"end_timestamp"`
	ManagedNodes     []*api.NodeUsage `json:"managed_nodes"`
	ScannedNodes     []*scanNode      `json:"scanned_nodes"`
	ManagedNodeCount int              `json:"managed_node_count"`
	ScannedNodeCount int              `json:"scanned_node_count"`
}

type scanNode struct {
	ID        string `json:"id"`
	Name      string `json:"name"`
	ScanJobID string `json:"scan_job_id"`
	LastSeen  string `json:"last_seen"`
}

func runLicenseStatusCmd(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return nil
	}

	response, err := connection.LicenseStatus(context.Background(), &api.LicenseStatusRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to get license status failed",
		)
	}

	if response.Set {
		status.GlobalResult = response

		// License information for a current license
		writer.Printf(licenseInfoFmt,
			response.CustomerName,
			response.LicenseId,
			time.Unix(response.ExpirationDate.GetSeconds(), 0).UTC().Format(time.RFC3339))

		// Add notice if license is expired
		if time.Now().Unix() > response.ExpirationDate.GetSeconds() {
			return status.New(
				status.LicenseError,
				"This license has expired. Please contact sales@chef.io to renew your Chef Automate license.",
			)
		}

		return nil
	}

	// Information if license is not set
	writer.Print(noLicenseAppliedMsg)
	return nil
}

var licenseCmdFlags = struct {
	forceSet bool
}{}

func runLicenseApplyCmd(cmd *cobra.Command, args []string) error {
	licenseToken, err := maybeFromFile(args[0])
	if err != nil {
		return err
	}

	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	req := &api.LicenseApplyRequest{License: licenseToken, Force: licenseCmdFlags.forceSet}
	response, err := connection.LicenseApply(context.Background(), req)
	if err != nil {
		// return with error message based on gRPC error response code
		// https://godoc.org/google.golang.org/grpc/codes
		switch grpc_status.Code(err) {
		case grpc_codes.DataLoss:
			return status.Wrap(
				err,
				status.LicenseError,
				"The license does not appear to be complete. Please check and try again.",
			)
		case grpc_codes.Unauthenticated:
			return status.Wrap(
				err,
				status.LicenseError,
				"The license cannot be verified by Chef Automate. Please contact support@chef.io for assistance.",
			)
		default:
			return status.Wrap(
				err,
				status.DeploymentServiceCallError,
				"Request to apply license failed",
			)
		}
	}

	// If it was a duplicate of an already applied license
	if !response.Updated && response.Duplicate {
		writer.Println("The license provided is already associated with this installation of Chef Automate.")
	}

	// Update successful
	if response.Updated {
		writer.Println("License updated")
	}

	return nil
}

func runLicenseUsageCmd(cmd *cobra.Command, args []string) error {
	tsFormat := "2006-01-02 15:04:05.999999999 -0700 MST"
	displayEndTimestamp := time.Now().UTC().Format(tsFormat)
	beginning := time.Now().Add(-time.Hour)
	displayStartTimestamp := beginning.UTC().Format(tsFormat)

	hourAgo, err := ptypes.TimestampProto(beginning)
	if err != nil {
		return err
	}

	// We're going to be combining the results from the conf-mgmt client
	// with the results from the scan jobs one.

	managedNodes, err := getConfigMgmtUsageNodes(hourAgo)
	if err != nil {
		return err
	}

	scanNodes, err := getScanInfo(hourAgo)
	if err != nil {
		return err
	}

	countScannedNodes := len(scanNodes)
	countManagedNodes := len(managedNodes)
	status.GlobalResult = usageResult{
		StartTimestamp:   displayStartTimestamp,
		EndTimestamp:     displayEndTimestamp,
		ManagedNodeCount: countManagedNodes,
		ManagedNodes:     managedNodes,
		ScannedNodes:     scanNodes,
		ScannedNodeCount: countScannedNodes,
	}

	writer.Titlef("Usage Summary for %s to %s", displayStartTimestamp, displayEndTimestamp)
	writer.Bodyf("Managed node count: %d\n  Node scan count: %d\n", countManagedNodes, countScannedNodes)
	writer.Body("For detailed output, re-run this command with `--result-json <output-file>`")
	return nil

}

// maybeFromFile provides a bit of user-friendliness by reading the
// token from disk if the passed string happens to be a file on disk
// that exists.
func maybeFromFile(maybeToken string) (string, error) {
	_, err := os.Stat(maybeToken)
	if err == nil {
		writer.Printf("Reading token data from file: %s\n", maybeToken)
		data, err := ioutil.ReadFile(maybeToken)
		if err != nil {
			return "", status.Wrap(
				err,
				status.FileAccessError,
				"Reading token data from file failed",
			)
		}
		strippedData := strings.TrimSpace(string(data))
		return strippedData, nil
	}
	writer.Println("No such file appears on disk, assuming that the argument is your license key content")
	return maybeToken, nil
}

func getConfigMgmtUsageNodes(hourAgo *tspb.Timestamp) ([]*api.NodeUsage, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return nil, err
	}

	usageInfo, err := connection.Usage(context.Background(), &api.UsageRequest{StartTime: hourAgo})
	if err != nil {
		return nil, status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request for usage info failed",
		)
	}

	return usageInfo.Nodes, nil
}

func getScanInfo(hourAgo *tspb.Timestamp) ([]*scanNode, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return nil, err
	}

	report, err := apiClient.ReportingClient().LicenseUsageNodes(ctx, &reporting.TimeQuery{StartTime: hourAgo})
	if err != nil {
		return nil, status.Wrap(err, status.APIError, "Request for scanned nodes failed")
	}

	nodeReports := report.Reports
	nodesScanned := report.Total

	// Form the array of scanned nodes
	scanJobNodes := make([]*scanNode, 0, nodesScanned)
	for _, node := range nodeReports {
		sn := &scanNode{
			ID:        node.NodeId,
			Name:      node.NodeName,
			ScanJobID: node.JobId,
			LastSeen:  tslib.TimestampString(node.EndTime),
		}
		scanJobNodes = append(scanJobNodes, sn)
	}

	return scanJobNodes, nil
}

var CommandFlags = struct {
	StartTime  string
	EndTime    string
	ESHostname string
	ESPort     string
	ESUserID   string
	ESPassword string
	FileName   string
}{}

func validateArgs() {
	if CommandFlags.StartTime == "" {
		fmt.Println("Start Time needs to be mentioned")
		os.Exit(1)
	}
	s, err := convertStringToTime(CommandFlags.StartTime)
	if err != nil {
		fmt.Println("The start time has to be in yyyy-mm-dd format")
		os.Exit(1)
	}
	if CommandFlags.EndTime == "" {
		CommandFlags.EndTime = time.Now().Format(time.RFC3339)
	}
	e, err := convertStringToTime(CommandFlags.EndTime)
	if err != nil {
		fmt.Println("The end time has to be in yyyy-mm-dd format")
		os.Exit(1)
	}

	if e.Before(s) {
		fmt.Println("End time cannot be before start time")
		os.Exit(1)
	}
}

func convertStringToTime(timeStr string) (time.Time, error) {
	// return time.Parse(time.RFC3339, fmt.Sprintf("%sT00:00:00.00Z", timeStr))
	return time.Parse("2006-01-02", timeStr)
}

func init() {
	RootCmd.AddCommand(licenseCmd)
	licenseCmd.AddCommand(licenseStatusCmd)
	licenseCmd.AddCommand(licenseApplyCmd)
	licenseCmd.AddCommand(licenseUsageCmd)
	licenseCmd.AddCommand(uniqueNodeCounterCmd)
	licenseCmd.AddCommand(nodeUsageCommand)
	licenseCmd.AddCommand(complianceUniqueResourceCounterCmd)
	licenseCmd.AddCommand(complianceResourceUsageCmd)
	uniqueNodeCounterCmd.Flags().StringVarP(&CommandFlags.StartTime, "start_time", "s", "", startTimeFormat)
	uniqueNodeCounterCmd.Flags().StringVarP(&CommandFlags.EndTime, "end_time", "e", "", endTimeFormat)
	uniqueNodeCounterCmd.Flags().StringVarP(&CommandFlags.ESHostname, "os_hostname", "n", "localhost", hostnameES)
	uniqueNodeCounterCmd.Flags().StringVarP(&CommandFlags.ESPort, "os_port", "p", "10168", portES)
	uniqueNodeCounterCmd.Flags().StringVarP(&CommandFlags.ESUserID, "os_username", "u", "admin", userNameES)
	uniqueNodeCounterCmd.Flags().StringVarP(&CommandFlags.ESPassword, "os_password", "P", "admin", passwordES)
	uniqueNodeCounterCmd.Flags().StringVarP(&CommandFlags.FileName, "fileName", "f", "", fileName)
	nodeUsageCommand.Flags().StringVarP(&CommandFlags.StartTime, "start_time", "s", "", startTimeFormat)
	nodeUsageCommand.Flags().StringVarP(&CommandFlags.EndTime, "end_time", "e", "", endTimeFormat)
	nodeUsageCommand.Flags().StringVarP(&CommandFlags.ESHostname, "es_hostname", "n", "localhost", hostnameES)
	nodeUsageCommand.Flags().StringVarP(&CommandFlags.ESPort, "es_port", "p", "10168", portES)
	nodeUsageCommand.Flags().StringVarP(&CommandFlags.ESUserID, "os_username", "u", "admin", userNameES)
	nodeUsageCommand.Flags().StringVarP(&CommandFlags.ESPassword, "os_password", "P", "admin", passwordES)
	nodeUsageCommand.Flags().StringVarP(&CommandFlags.FileName, "fileName", "f", "", fileName)
	complianceUniqueResourceCounterCmd.Flags().StringVarP(&CommandFlags.StartTime, "start_time", "s", "", startTimeFormat)
	complianceUniqueResourceCounterCmd.Flags().StringVarP(&CommandFlags.EndTime, "end_time", "e", "", endTimeFormat)
	complianceUniqueResourceCounterCmd.Flags().StringVarP(&CommandFlags.ESHostname, "es_hostname", "n", "localhost", hostnameES)
	complianceUniqueResourceCounterCmd.Flags().StringVarP(&CommandFlags.ESPort, "es_port", "p", "10168", portES)
	complianceUniqueResourceCounterCmd.Flags().StringVarP(&CommandFlags.ESUserID, "os_username", "u", "admin", userNameES)
	complianceUniqueResourceCounterCmd.Flags().StringVarP(&CommandFlags.ESPassword, "os_password", "P", "admin", passwordES)
	complianceUniqueResourceCounterCmd.Flags().StringVarP(&CommandFlags.FileName, "fileName", "f", "", fileName)
	complianceResourceUsageCmd.Flags().StringVarP(&CommandFlags.StartTime, "start_time", "s", "", startTimeFormat)
	complianceResourceUsageCmd.Flags().StringVarP(&CommandFlags.EndTime, "end_time", "e", "", endTimeFormat)
	complianceResourceUsageCmd.Flags().StringVarP(&CommandFlags.ESHostname, "es_hostname", "n", "localhost", hostnameES)
	complianceResourceUsageCmd.Flags().StringVarP(&CommandFlags.ESPort, "es_port", "p", "10168", portES)
	complianceResourceUsageCmd.Flags().StringVarP(&CommandFlags.ESUserID, "os_username", "u", "admin", userNameES)
	complianceResourceUsageCmd.Flags().StringVarP(&CommandFlags.ESPassword, "os_password", "P", "admin", passwordES)
	complianceResourceUsageCmd.Flags().StringVarP(&CommandFlags.FileName, "fileName", "f", "", fileName)
	licenseApplyCmd.Flags().BoolVarP(&licenseCmdFlags.forceSet, "force", "f", false, "Force set license")
}

func preLicenseCmd(cmd *cobra.Command, args []string) error {
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	if isA2HARBFileExist() {
		output, err := RunCmdOnSingleAutomateNode(cmd, args)
		if err != nil {
			return err
		}
		writer.Print(output)
		// NOTE: used os.exit as need to stop next lifecycle method to execute
		os.Exit(0)
	}
	return nil
}

func getPreLicenseReportCmd(fileNamePrefix string) func(*cobra.Command, []string) error {
	commandWithFileName := func(cmd *cobra.Command, args []string) error {
		err := commandPrePersistent(cmd)
		if err != nil {
			return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
		}
		reportFileName := CommandFlags.FileName
		if reportFileName == "" {
			reportFileName = fmt.Sprintf("%s_%s_%s.csv", fileNamePrefix, CommandFlags.StartTime, CommandFlags.EndTime)
			args = append(args, "-f", reportFileName)
			CommandFlags.FileName = reportFileName
		}
		if isA2HARBFileExist() {

			infra, err := getAutomateHAInfraDetails()
			if err != nil {
				fmt.Print(err)
				return err
			}

			err = RunCmdOnSingleAutomateNodeNCopyReport(cmd, args, reportFileName, infra)
			if err != nil {
				return err
			}
			// NOTE: used os.exit as need to stop next lifecycle method to execute
			os.Exit(0)
		}
		return nil
	}
	return commandWithFileName
}
