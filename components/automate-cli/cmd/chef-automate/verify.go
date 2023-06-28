package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	"github.com/chef/automate/components/automate-cli/pkg/verifysystemdcreate"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/reporting"
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/version"
	"github.com/jedib0t/go-pretty/v5/table"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	VERIFY_SERVER_PORT        = "VERIFY_SERVER_PORT"
	BINARY_DESTINATION_FOLDER = "/etc/automate-verify"
	SYSTEMD_PATH              = "/etc/systemd/system"
	REMOTE_NODES              = "remote nodes"
	BASTION                   = "bastion"
	LOCALHOST                 = "localhost"
	SLEEP_DURATION            = 2 * time.Second
)

var (
	statusAPIRoute     = "/status"
	batchCheckAPIRoute = "/api/v1/checks/batch-checks"
	getAPIEndpoint     = func(hostIp string, port string, route string) string {
		return fmt.Sprintf("http://%s:%s%s", hostIp, port, route)
	}
)

type verifyCmdFlags struct {
	config      string
	debug       bool
	prettyPrint bool
}

type verifyCmdFlow struct {
	Client               httputils.HTTPClient
	CreateSystemdService verifysystemdcreate.CreateSystemdService
	SystemdCreateUtils   verifysystemdcreate.SystemdCreateUtils
	Config               *config.HaDeployConfig
	SSHUtil              sshutils.SSHUtil
	Writer               *cli.Writer
	prettyPrint          bool
	automateIPs          []string
	chefServerIPs        []string
	postgresqlIPs        []string
	opensearchIPs        []string
	sshPort              string
	sshKeyFile           string
	sshUserName          string
	deps                 *verifyCmdDeps
}

type verifyCmdDeps struct {
	getAutomateHAInfraDetails func() (*AutomateHAInfraDetails, error)
	PopulateHaCommonConfig    func(configPuller PullConfigs) (haDeployConfig *config.HaDeployConfig, err error)
}

func NewVerifyCmdFlow(client httputils.HTTPClient, createSystemdService verifysystemdcreate.CreateSystemdService, systemdCreateUtils verifysystemdcreate.SystemdCreateUtils, config *config.HaDeployConfig, sshutil sshutils.SSHUtil, writer *cli.Writer, deps *verifyCmdDeps) *verifyCmdFlow {
	return &verifyCmdFlow{
		Client:               client,
		CreateSystemdService: createSystemdService,
		SystemdCreateUtils:   systemdCreateUtils,
		Config:               config,
		SSHUtil:              sshutil,
		Writer:               writer,
		deps:                 deps,
	}
}

type verifyServeCmdFlow struct{}

type verifySystemdCreateFlow struct{}

type StatusResponse struct {
	HostIp       string
	ResponseBody []byte
	Error        error
}

func init() {
	flagsObj := verifyCmdFlags{}

	verifyCmd := &cobra.Command{
		Use:   "verify",
		Short: "Verify Chef Automate setup",
		Long:  "Verify Chef Automate config files and Infrastructure",
		Annotations: map[string]string{
			docs.Compatibility: docs.Compatibility,
		},
		Args:   cobra.RangeArgs(0, 1),
		RunE:   verifyCmdFunc(&flagsObj),
		Hidden: true,
	}

	verifyServeCmd := &cobra.Command{
		Use:   "serve",
		Short: "Start verify server",
		Long:  "Start verify server for running checks",
		Annotations: map[string]string{
			docs.Compatibility: docs.Compatibility,
		},
		Args: cobra.ExactArgs(0),
		RunE: verifyServeCmdFunc(&flagsObj),
	}

	verifySystemdServiceCmd := &cobra.Command{
		Use:   "systemd-service COMMAND",
		Short: "Systemd utilities for verify command",
		Annotations: map[string]string{
			docs.Compatibility: docs.Compatibility,
		},
	}

	verifySystemdServiceCreateCmd := &cobra.Command{
		Use:   "create",
		Short: "Start verify server as systemd service",
		Annotations: map[string]string{
			docs.Compatibility: docs.Compatibility,
		},
		Args: cobra.ExactArgs(0),
		RunE: verifySystemdCreateFunc(&flagsObj),
	}

	verifyCmd.PersistentFlags().StringVarP(
		&flagsObj.config,
		"config",
		"c",
		"",
		"Config file that needs to be verified")

	verifyCmd.Flags().BoolVarP(
		&flagsObj.debug,
		"debug",
		"d",
		false,
		"enable debugging for verify command")

	verifyCmd.Flags().BoolVarP(
		&flagsObj.prettyPrint,
		"pretty-print",
		"p",
		false,
		"pretty-print json response")

	verifyServeCmd.Flags().BoolVarP(
		&flagsObj.debug,
		"debug",
		"d",
		false,
		"enable debugging for verify serve command")

	verifySystemdServiceCreateCmd.Flags().BoolVarP(
		&flagsObj.debug,
		"debug",
		"d",
		false,
		"enable debugging for verify systemd-service create command")

	verifySystemdServiceCmd.AddCommand(verifySystemdServiceCreateCmd)
	verifyCmd.AddCommand(verifySystemdServiceCmd)
	verifyCmd.AddCommand(verifyServeCmd)
	RootCmd.AddCommand(verifyCmd)
}

func verifyServeCmdFunc(flagsObj *verifyCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		c := verifyServeCmdFlow{}
		return c.runVerifyServeCmd(cmd, args, flagsObj.debug)
	}
}

func (v *verifyServeCmdFlow) runVerifyServeCmd(cmd *cobra.Command, args []string, debug bool) error {
	port := getPort()
	writer.Println("Using port " + port)
	vs, err := server.NewVerifyServer(port, debug)
	if err != nil {
		return err
	}
	return vs.Start()
}

func verifySystemdCreateFunc(flagsObj *verifyCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		c := verifySystemdCreateFlow{}
		return c.runVerifySystemdCreateCmd(cmd, args, flagsObj.debug)
	}
}

func (v *verifySystemdCreateFlow) runVerifySystemdCreateCmd(cmd *cobra.Command, args []string, debug bool) error {
	createSystemdServiceWithBinary, err := verifysystemdcreate.NewCreateSystemdService(
		verifysystemdcreate.NewSystemdCreateUtilsImpl(),
		BINARY_DESTINATION_FOLDER,
		SYSTEMD_PATH,
		debug,
		writer,
	)
	if err != nil {
		return err
	}
	return createSystemdServiceWithBinary.Create()
}

func verifyCmdFunc(flagsObj *verifyCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		defaultLevel := "info"
		if flagsObj.debug {
			defaultLevel = "debug"
		}

		log, err := logger.NewLogger("text", defaultLevel)
		if err != nil {
			return err
		}

		createSystemdServiceWithBinary, err := verifysystemdcreate.NewCreateSystemdService(
			verifysystemdcreate.NewSystemdCreateUtilsImpl(),
			BINARY_DESTINATION_FOLDER,
			SYSTEMD_PATH,
			flagsObj.debug,
			writer,
		)
		if err != nil {
			return err
		}
		deps := &verifyCmdDeps{
			getAutomateHAInfraDetails: getAutomateHAInfraDetails,
			PopulateHaCommonConfig:    PopulateHaCommonConfig,
		}
		c := NewVerifyCmdFlow(httputils.NewClient(log), createSystemdServiceWithBinary, verifysystemdcreate.NewSystemdCreateUtilsImpl(), config.NewHaDeployConfig(), sshutils.NewSSHUtilWithCommandExecutor(sshutils.NewSshClient(), log, command.NewExecExecutor()), writer, deps)
		return c.runVerifyCmd(cmd, args, flagsObj)
	}
}

func (v *verifyCmdFlow) runVerifyCmd(cmd *cobra.Command, args []string, flagsObj *verifyCmdFlags) error {
	v.prettyPrint = flagsObj.prettyPrint
	return v.RunVerify(flagsObj.config)
}

func (v *verifyCmdFlow) RunVerify(config string) error {
	var configPath string
	// TODO : config flag is optional for now. Need to handle the default config path
	if len(strings.TrimSpace(config)) > 0 {
		configPath = config
		err := v.Config.ParseAndVerify(configPath)
		if err != nil {
			return err
		}
	} else {
		infra, err := v.deps.getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		sshConfig := &SSHConfig{
			sshUser:    infra.Outputs.SSHUser.Value,
			sshKeyFile: infra.Outputs.SSHKeyFile.Value,
			sshPort:    infra.Outputs.SSHPort.Value,
		}
		sshUtil := NewSSHUtil(sshConfig)
		configPuller := NewPullConfigs(infra, sshUtil)

		config, err := v.deps.PopulateHaCommonConfig(configPuller)
		if err != nil {
			return err
		}
		v.Config = config
		err = v.Config.Verify()
		if err != nil {
			return err
		}
	}

	// Get config required for batch-check API call
	batchCheckConfig := models.NewConfig()
	if v.Config.IsExistingInfra() {
		v.sshPort, v.sshKeyFile, v.sshUserName = v.getSSHConfig(v.Config.Architecture.ExistingInfra)

		v.automateIPs = v.Config.ExistingInfra.Config.AutomatePrivateIps
		v.chefServerIPs = v.Config.ExistingInfra.Config.ChefServerPrivateIps
		v.postgresqlIPs = v.Config.ExistingInfra.Config.PostgresqlPrivateIps
		v.opensearchIPs = v.Config.ExistingInfra.Config.OpensearchPrivateIps
	} else if v.Config.IsAws() {
		v.sshPort, v.sshKeyFile, v.sshUserName = v.getSSHConfig(v.Config.Architecture.Aws)
		configDetails, err := fetchAwsConfigFromTerraform()
		if err != nil {
			return err
		}

		v.automateIPs = configDetails.AutomateIps
		v.chefServerIPs = configDetails.ChefServerIps
		v.postgresqlIPs = configDetails.PostgresqlIps
		v.opensearchIPs = configDetails.OpensearchIps

		if v.Config.Aws.Config.SetupManagedServices {
			batchCheckConfig.ExternalOS = &models.ExternalOS{
				OSRoleArn:                     configDetails.OsSnapshotRoleArn,
				OsSnapshotUserAccessKeyId:     configDetails.OsSnapshotUserID,
				OsSnapshotUserAccessKeySecret: configDetails.OsSnapshotUserSecret,
			}
		}
		// assign ip's to models.Hardware
		batchCheckHardware := batchCheckConfig.Hardware
		batchCheckHardware.AutomateNodeIps = configDetails.AutomateIps
		batchCheckHardware.ChefInfraServerNodeIps = configDetails.ChefServerIps
		batchCheckHardware.PostgresqlNodeIps = configDetails.PostgresqlIps
		batchCheckHardware.OpenSearchNodeIps = configDetails.OpensearchIps
	} else {
		return errors.New("Invalid config")
	}

	err := batchCheckConfig.PopulateWith(v.Config)
	if err != nil {
		return err
	}
	var batchCheckResultBastion, batchCheckResultRemote models.BatchCheckResponse
	var batchCheckResults []models.BatchCheckResult

	// Doing batch-check API call for bastion
	resBastion, err := v.runVerifyServiceForBastion(*batchCheckConfig)
	if err != nil {
		return err
	}

	if err := json.Unmarshal(resBastion, &batchCheckResultBastion); err != nil {
		return fmt.Errorf("failed to unmarshal batch-check API result field for bastion: %v", err)
	}

	batchCheckResults = append(batchCheckResults, batchCheckResultBastion.NodeResult...)

	// If batch-check API failed on bastion
	if !batchCheckResultBastion.Passed {
		v.printResponse(batchCheckResults)
		return fmt.Errorf("batch-check API failed on bastion")
	}

	// Doing batch-check API call for remote nodes
	resRemote, err := v.runVerifyServiceForRemote(*batchCheckConfig)
	if err != nil {
		return err
	}

	if err := json.Unmarshal(resRemote, &batchCheckResultRemote); err != nil {
		return fmt.Errorf("failed to unmarshal batch-check API result field for remote nodes: %v", err)
	}

	// Appending batch-check API result for remote nodes to batch-check API result for bastion
	batchCheckResults = append(batchCheckResults, batchCheckResultRemote.NodeResult...)

	// If batch-check API failed on remote nodes
	if !batchCheckResultRemote.Passed {
		v.printResponse(batchCheckResults)
		return fmt.Errorf("batch-check API failed on remote nodes")
	}

	// Printing response for success case
	v.printResponse(batchCheckResults)

	return nil
}

// Runs automate-verify service on bastion
func (v *verifyCmdFlow) runVerifyServiceForBastion(batchCheckConfig models.Config) ([]byte, error) {
	v.Writer.Println("Checking automate-verify service on bastion")
	//Call status API to check if automate-verify service is running on bastion
	statusAPIEndpoint := getAPIEndpoint(LOCALHOST, getPort(), statusAPIRoute)
	_, responseBody, err := v.Client.MakeRequest(http.MethodGet, statusAPIEndpoint, nil)
	if err != nil {
		v.Writer.Printf("error while checking automate-verify service on bastion: %v\n", err)
		if responseBody != nil {
			v.Writer.Printf("Error response body: %s\n", string(responseBody))
		}
		v.Writer.Println("Adding automate-verify service to systemd and starting the service")

		// create systemd service with current CLI binary
		err = v.createSystemdOnBastion()
		if err != nil {
			return nil, err
		}

		v.Writer.Println("Added automate-verify service to systemd and started the the service")
	} else {
		resultBytes, err := v.getResultFromResponseBody(responseBody)
		if err != nil {
			return nil, err
		}

		var result models.StatusDetails

		if err := json.Unmarshal(resultBytes, &result); err != nil {
			return nil, fmt.Errorf("failed to unmarshal Result field: %v", err)
		}

		// Upgrade if current CLI is latest version than the existing CLI on bastion
		latestCLIVersion := version.BuildTime
		if latestCLIVersion > result.CliVersion {
			v.Writer.Printf("Upgrading from CLI %s to latest %s on bastion\n", result.CliVersion, latestCLIVersion)
			// create systemd service with current CLI binary
			err = v.createSystemdOnBastion()
			if err != nil {
				return nil, err
			}
			v.Writer.Println("Upgrading from CLI completed on bastion")
		}
	}
	v.Writer.Println("Checking automate-verify service on bastion completed")

	// Doing batch-check API call for bastion
	batchCheckBastionReq := models.BatchCheckRequest{
		Checks: constants.GetBastionChecks(),
		Config: &batchCheckConfig,
	}

	return v.makeBatchCheckAPICall(batchCheckBastionReq, BASTION)
}

// Runs automate-verify service on remote nodes
func (v *verifyCmdFlow) runVerifyServiceForRemote(batchCheckConfig models.Config) ([]byte, error) {
	// TODO: Need to check if automate-verify service is already running on remote nodes and upgrade if needed.

	hostIPs := v.getHostIPs(
		v.automateIPs,
		v.chefServerIPs,
		v.postgresqlIPs,
		v.opensearchIPs,
	)

	sshConfig := sshutils.NewSshConfig("", v.sshPort, v.sshKeyFile, v.sshUserName)

	destFileName := "chef-automate"

	hostIPsToCopyLatestCLI, err := v.getHostIPsWithNoLatestCLI(hostIPs)
	if err != nil {
		return nil, err
	}

	if len(hostIPsToCopyLatestCLI) > 0 {
		v.Writer.Printf("Copying CLI to the follwoing IPs: %v", hostIPsToCopyLatestCLI)
		// Copying Latest CLI binary to remote nodes
		err = v.copyCLIOnRemoteNodes(destFileName, sshConfig, hostIPsToCopyLatestCLI)
		if err != nil {
			return nil, err
		}

		// Starting automate-verify service on remote nodes
		err = v.startServiceOnRemoteNodes(destFileName, sshConfig, hostIPs)
		if err != nil {
			return nil, err
		}
	}

	// Doing batch-check API call for remote nodes
	batchCheckRemoteReq := models.BatchCheckRequest{
		Checks: constants.GetRemoteChecks(),
		Config: &batchCheckConfig,
	}

	return v.makeBatchCheckAPICall(batchCheckRemoteReq, REMOTE_NODES)
}

// Get Host IPs without chef-automate binary or without latest version chef-automate binary
func (v *verifyCmdFlow) getHostIPsWithNoLatestCLI(hostIPs []string) ([]string, error) {

	responseChan := make(chan StatusResponse, len(hostIPs))
	for _, hostIP := range hostIPs {
		statusAPIEndpoint := getAPIEndpoint(hostIP, getPort(), statusAPIRoute)

		go func(hostIP string) {
			rc := StatusResponse{hostIP, nil, nil}
			_, responseBody, err := v.Client.MakeRequest(http.MethodGet, statusAPIEndpoint, nil)
			if err != nil {
				rc.Error = err
				responseChan <- rc
				return
			}
			rc.ResponseBody = responseBody
			responseChan <- rc

		}(hostIP)
	}

	hostIPsWithoutLatestCLI := []string{}
	for i := 0; i < len(hostIPs); i++ {
		response := <-responseChan

		// Incase of unreachable error (connection refused) , appending IP to the array
		if response.Error != nil {
			hostIPsWithoutLatestCLI = append(hostIPsWithoutLatestCLI, response.HostIp)
			continue
		}

		resultBytes, err := v.getResultFromResponseBody(response.ResponseBody)
		if err != nil {
			return nil, err
		}

		var result models.StatusDetails
		if err := json.Unmarshal(resultBytes, &result); err != nil {
			return nil, fmt.Errorf("failed to unmarshal Result field: %v", err)
		}

		if result.CliVersion < version.BuildTime {
			hostIPsWithoutLatestCLI = append(hostIPsWithoutLatestCLI, response.HostIp)
		}
	}
	return hostIPsWithoutLatestCLI, nil
}

// Creates systemd service with current CLI binary
func (v *verifyCmdFlow) createSystemdOnBastion() error {
	err := v.CreateSystemdService.Create()
	if err != nil {
		return err
	}
	// TODO: Added 2 seconds sleep to make sure service is started. Need to handle this in a better way.
	time.Sleep(SLEEP_DURATION)
	return nil
}

// Makes batch-check API call
func (v *verifyCmdFlow) makeBatchCheckAPICall(requestBody models.BatchCheckRequest, nodeType string) ([]byte, error) {
	v.Writer.Printf("Doing batch-check API call for %s\n", nodeType)
	batchCheckAPIEndpoint := getAPIEndpoint(LOCALHOST, getPort(), batchCheckAPIRoute)
	_, responseBody, err := v.Client.MakeRequest(http.MethodPost, batchCheckAPIEndpoint, requestBody)
	if err != nil {
		if responseBody != nil {
			return nil, fmt.Errorf("error while doing batch-check API call for %s:\n%s", nodeType, string(responseBody))
		}
		return nil, fmt.Errorf("error while doing batch-check API call for %s: %v", nodeType, err)
	}
	resultBytes, err := v.getResultFromResponseBody(responseBody)
	if err != nil {
		return nil, err
	}
	v.Writer.Printf("Batch-check API call for %s completed\n", nodeType)
	return resultBytes, nil
}

// Copies CLI binary to remote nodes
func (v *verifyCmdFlow) copyCLIOnRemoteNodes(destFileName string, sshConfig sshutils.SSHConfig, hostIPs []string) error {
	v.Writer.Println("Copying automate-verify CLI to remote nodes")
	currentBinaryPath, err := v.SystemdCreateUtils.GetBinaryPath()
	if err != nil {
		return err
	}
	copyResults := v.SSHUtil.CopyFileToRemoteConcurrently(sshConfig, currentBinaryPath, destFileName, false, hostIPs)
	isError := false
	for _, result := range copyResults {
		if result.Error != nil {
			v.Writer.Errorf("Remote copying automate-verify CLI failed on node : %s with error: %v\n", result.HostIP, result.Error)
			isError = true
		}
	}
	if isError {
		return fmt.Errorf("remote copying failed")
	}
	v.Writer.Println("Copying automate-verify CLI to remote nodes completed")
	return nil
}

// Starts automate-verify service on remote nodes
func (v *verifyCmdFlow) startServiceOnRemoteNodes(destFileName string, sshConfig sshutils.SSHConfig, hostIPs []string) error {
	v.Writer.Println("Creating automate-verify service on remote nodes")
	cmd := fmt.Sprintf("sudo /tmp/%s verify systemd-service create", destFileName)
	excuteResults := v.SSHUtil.ExecuteConcurrently(sshConfig, cmd, hostIPs)
	isError := false
	for _, result := range excuteResults {
		if result.Error != nil {
			v.Writer.Errorf("Remote execution of cmd '%s' automate-verify CLI failed on node : %s with error: %v\n", cmd, result.HostIP, result.Error)
			isError = true
		}
	}
	if isError {
		return fmt.Errorf("remote execution failed")
	}
	// TODO: Added 2 seconds sleep to make sure service is started. Need to handle this in a better way.
	time.Sleep(SLEEP_DURATION)
	v.Writer.Println("Created automate-verify service on remote nodes")
	return nil
}

// Returns result field from response body
func (v *verifyCmdFlow) getResultFromResponseBody(responseBody []byte) ([]byte, error) {
	var responseBodyStruct response.ResponseBody
	if err := json.Unmarshal(responseBody, &responseBodyStruct); err != nil {
		return nil, fmt.Errorf("failed to unmarshal response body: %v", err)
	}

	var resultBytes []byte
	var err error
	if v.prettyPrint {
		resultBytes, err = json.MarshalIndent(responseBodyStruct.Result, "", "    ")
	} else {
		resultBytes, err = json.Marshal(responseBodyStruct.Result)
	}

	if err != nil {
		return nil, fmt.Errorf("failed to marshal result field: %v", err)
	}

	return resultBytes, nil
}

// Returns port, keyFile, userName
func (v *verifyCmdFlow) getSSHConfig(config *config.ConfigInitials) (string, string, string) {
	return config.SSHPort, config.SSHKeyFile, config.SSHUser
}

// Returns concatenated list of automatePrivateIps, chefServerPrivateIps, postgresqlPrivateIps, opensearchPrivateIps
func (v *verifyCmdFlow) getHostIPs(automatePrivateIps, chefServerPrivateIps, postgresqlPrivateIps, opensearchPrivateIps []string) []string {
	var hostIPs []string
	hostIPs = stringutils.ConcatSlice(hostIPs, automatePrivateIps)
	hostIPs = stringutils.ConcatSlice(hostIPs, chefServerPrivateIps)
	if !v.Config.IsExternalDb() {
		hostIPs = stringutils.ConcatSlice(hostIPs, postgresqlPrivateIps)
		hostIPs = stringutils.ConcatSlice(hostIPs, opensearchPrivateIps)
	}
	return hostIPs
}

func (v *verifyCmdFlow) printResponse(batchCheckResults []models.BatchCheckResult) {
	reports := buildReports(batchCheckResults)
	tables := createTables(len(v.automateIPs), len(v.chefServerIPs), len(v.postgresqlIPs), len(v.opensearchIPs))
	reportingModule := reporting.NewReportingModule(v.Writer, tables)
	nodeInfoMap := make(map[string][]reporting.Info)

	reporting.VerificationReports(reports, reportingModule, nodeInfoMap)
}

func buildReports(batchCheckResults []models.BatchCheckResult) []reporting.VerificationReport {
	var reports []reporting.VerificationReport

	for _, batchCheckResult := range batchCheckResults {

		for _, test := range batchCheckResult.Tests {
			report := reporting.VerificationReport{}
			report.TableKey = batchCheckResult.NodeType
			info := reporting.Info{}
			info.Hostip = batchCheckResult.Ip

			info.Parameter = test.Check

			if test.Skipped {
				info.Status = "Skipped"
			} else if test.Passed {
				info.Status = "Success"
			} else {
				info.Status = "Failed"
			}

			var errorMsgs, resolutionMsgs []string
			var successfulCount, failedCount int
			for _, check := range test.Checks {
				if check.Passed {
					successfulCount++
				} else {
					failedCount++
					errorMsgs = append(errorMsgs, check.ErrorMsg)
					resolutionMsgs = append(resolutionMsgs, check.ResolutionMsg)
				}
			}

			statusMessage := &reporting.StatusMessage{}
			statusMessage.MainMessage = fmt.Sprintf("%s - %s", test.Message, info.Status)
			statusMessage.SubMessage = errorMsgs
			info.StatusMessage = statusMessage

			summaryInfo := &reporting.SummaryInfo{}
			summaryInfo.SuccessfulCount = successfulCount
			summaryInfo.FailedCount = failedCount
			summaryInfo.ToResolve = resolutionMsgs
			info.SummaryInfo = summaryInfo

			report.Report = info

			reports = append(reports, report)
		}

	}

	return reports
}

func createTables(numberOfAutomateNodes, numberOfChefServerNodes, numberOfPostgreSQLNodes, numberOfOpenSearchNodes int) map[string]*reporting.Table {
	bastionSummaryTableTitle := "Summary: Bastion <Node> - 1"
	automateSummaryTableTitle := fmt.Sprintf("Summary: Automate <Nodes> - %d", numberOfAutomateNodes)
	chefServerSummaryTableTitle := fmt.Sprintf("Summary: Chef Infra Server <Nodes> - %d", numberOfChefServerNodes)
	postgreSQLSummaryTableTitle := fmt.Sprintf("Summary: PostgreSQL <Nodes> - %d", numberOfPostgreSQLNodes)
	openSearchSummaryTableTitle := fmt.Sprintf("Summary: Opensearch <Nodes> - %d", numberOfOpenSearchNodes)
	tb := make(map[string]*reporting.Table)
	tb["bastionStatusTable"] = getStatusTable("Bastion")
	tb["bastionSummaryTable"] = getSummaryTable(bastionSummaryTableTitle)
	tb["automateStatusTable"] = getStatusTable("Automate")
	tb["automateSummaryTable"] = getSummaryTable(automateSummaryTableTitle)
	tb["chef-infra-serverStatusTable"] = getStatusTable("Chef Infra Server")
	tb["chef-infra-serverSummaryTable"] = getSummaryTable(chefServerSummaryTableTitle)
	tb["postgresqlStatusTable"] = getStatusTable("PostgreSQL")
	tb["postgresqlSummaryTable"] = getSummaryTable(postgreSQLSummaryTableTitle)
	tb["opensearchStatusTable"] = getStatusTable("OpenSearch")
	tb["opensearchSummaryTable"] = getSummaryTable(openSearchSummaryTableTitle)
	return tb
}

func getStatusTable(title string) *reporting.Table {
	return &reporting.Table{
		Title:     title,
		Header:    table.Row{"No.", "Identifier", "Parameter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 5, WidthMin: 5}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 25, WidthMin: 25}, {Number: 4, WidthMax: 15, WidthMin: 15}, {Number: 5, WidthMax: 60, WidthMin: 60}},
	}
}

func getSummaryTable(title string) *reporting.Table {
	return &reporting.Table{
		Title:     title,
		Header:    table.Row{"Parameter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 30, WidthMin: 30}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 15, WidthMin: 15}, {Number: 4, WidthMax: 60, WidthMin: 60}},
	}
}

// Returns port
func getPort() string {
	port := server.DEFAULT_PORT
	envPort := os.Getenv(VERIFY_SERVER_PORT)
	if envPort != "" {
		if _, err := strconv.Atoi(envPort); err == nil {
			port = envPort
		}
	}
	return port
}
