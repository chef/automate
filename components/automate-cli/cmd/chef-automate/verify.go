package main

import (
	"encoding/json"
	"fmt"
	"io"
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
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/version"
	"github.com/spf13/cobra"
)

const (
	VERIFY_SERVER_PORT        = "VERIFY_SERVER_PORT"
	BINARY_DESTINATION_FOLDER = "/etc/automate-verify"
	SYSTEMD_PATH              = "/etc/systemd/system"
)

var (
	statusAPIEndpoint     = fmt.Sprintf("http://localhost:%s/status", getPort())
	batchCheckAPIEndpoint = fmt.Sprintf("http://localhost:%s/api/v1/checks/batch-checks", getPort())
)

type verifyCmdFlags struct {
	config      string
	debug       bool
	prettyPrint bool
}

type verifyCmdFlow struct {
	Client               httputils.HTTPClient
	CreateSystemdService *verifysystemdcreate.CreateSystemdService
	Config               *config.HaDeployConfig
	SSHUtil              sshutils.SSHUtil
	Writer               *cli.Writer
	prettyPrint          bool
}

func NewVerifyCmdFlow(client httputils.HTTPClient, createSystemdService *verifysystemdcreate.CreateSystemdService, config *config.HaDeployConfig, sshutil sshutils.SSHUtil, writer *cli.Writer) *verifyCmdFlow {
	return &verifyCmdFlow{
		Client:               client,
		CreateSystemdService: createSystemdService,
		Config:               config,
		SSHUtil:              sshutil,
		Writer:               writer,
	}
}

type verifyServeCmdFlow struct{}

type verifySystemdCreateFlow struct{}

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

		c := NewVerifyCmdFlow(httputils.NewClient(log), createSystemdServiceWithBinary, config.NewHaDeployConfig(), sshutils.NewSSHUtilWithCommandExecutor(sshutils.NewSshClient(), log, command.NewExecExecutor()), writer)
		return c.runVerifyCmd(cmd, args, flagsObj)
	}
}

func (v *verifyCmdFlow) runVerifyCmd(cmd *cobra.Command, args []string, flagsObj *verifyCmdFlags) error {
	var configPath string

	// TODO : config flag is optional for now. Need to handle the default config path
	if len(strings.TrimSpace(flagsObj.config)) > 0 {
		configPath = flagsObj.config
	}

	v.prettyPrint = flagsObj.prettyPrint

	err := v.Config.ParseAndVerify(configPath)
	if err != nil {
		return err
	}

	// Get config required for batch-check API call
	batchCheckConfig := &models.Config{}
	batchCheckConfig.PopulateWith(v.Config)

	// TODO: For now just print the result as json. Need to merge the result with the response from batch-check API call for remote.

	err = v.checkAutomateVerifyServiceForBastion(*batchCheckConfig)
	if err != nil {
		return err
	}

	err = v.checkAutomateVerifyServiceForRemote(*batchCheckConfig)
	if err != nil {
		return err
	}

	return nil
}

func (v *verifyCmdFlow) checkAutomateVerifyServiceForBastion(batchCheckConfig models.Config) error {
	v.Writer.Println("Checking automate-verify service on bastion")
	//Call status API to check if automate-verify service is running on bastion
	res, err := v.Client.MakeRequest(http.MethodGet, statusAPIEndpoint, nil)
	if err != nil {
		v.Writer.Println(fmt.Sprintf("error while checking automate-verify service on bastion: %v\n", err))
		v.Writer.Println("Adding automate-verify service to systemd and starting the service")
		err := v.CreateSystemdService.Create()
		if err != nil {
			return err
		}
		// Wait for 2 seconds for the service to start
		// TODO: Do retry logic to check if the service is up and running
		time.Sleep(2 * time.Second)
		v.Writer.Println("Adding automate-verify service to systemd and starting the service completed")
	} else {
		resultBytes, err := v.GetResultFromResponseBody(res.Body)
		if err != nil {
			return err
		}

		var result models.StatusDetails

		if err := json.Unmarshal(resultBytes, &result); err != nil {
			return fmt.Errorf("failed to unmarshal Result field: %v", err)
		}

		// Upgrade if latest CLI version is greater than the current CLI version on bastion
		latestCLIVersion := version.BuildTime
		if latestCLIVersion > result.CliVersion {
			v.Writer.Println(fmt.Sprintf("Upgrading from CLI %s to latest %s on bastion", result.CliVersion, latestCLIVersion))
			err := v.CreateSystemdService.Create()
			if err != nil {
				return err
			}
			v.Writer.Println("Upgrading from CLI completed on bastion")
			// Wait for 2 seconds for the service to start
			time.Sleep(2 * time.Second)
		}
	}
	v.Writer.Println("Checking automate-verify service on bastion completed")

	// Doing batch-check API call for bastion
	batchCheckBastionReq := models.BatchCheckRequest{
		Checks: constants.GetBastionChecks(),
		Config: batchCheckConfig,
	}
	v.Writer.Println("Doing batch-check API call for bastion")
	// Call batch-check API to run checks on bastion
	batchCheckBastionRes, err := v.Client.MakeRequest(http.MethodPost, batchCheckAPIEndpoint, batchCheckBastionReq)
	if err != nil {
		return fmt.Errorf("error while doing batch-check API call for bastion: %v", err)
	} else {
		resultBytes, err := v.GetResultFromResponseBody(batchCheckBastionRes.Body)
		if err != nil {
			return err
		}
		v.Writer.Printf("Response for batch-check API on Bastion: \n%s\n", string(resultBytes))
	}
	v.Writer.Println("Batch-check API call for bastion completed")

	return nil
}

func (v *verifyCmdFlow) checkAutomateVerifyServiceForRemote(batchCheckConfig models.Config) error {

	// TODO: Need to check if automate-verify service is already running on remote nodes and upgrade if needed.
	var port, keyFile, userName string
	var hostIPs []string

	if v.Config.IsExistingInfra() {
		port = v.Config.Architecture.ExistingInfra.SSHPort
		keyFile = v.Config.Architecture.ExistingInfra.SSHKeyFile
		userName = v.Config.Architecture.ExistingInfra.SSHUser

		hostIPs = stringutils.ConcatSlice(hostIPs, v.Config.ExistingInfra.Config.AutomatePrivateIps)
		hostIPs = stringutils.ConcatSlice(hostIPs, v.Config.ExistingInfra.Config.ChefServerPrivateIps)
		hostIPs = stringutils.ConcatSlice(hostIPs, v.Config.ExistingInfra.Config.PostgresqlPrivateIps)
		hostIPs = stringutils.ConcatSlice(hostIPs, v.Config.ExistingInfra.Config.OpensearchPrivateIps)

		sshConfig := sshutils.NewSshConfig("", port, keyFile, userName)

		destFileName := "chef-automate"

		currentBinaryPath, err := v.CreateSystemdService.SystemdCreateUtils.GetBinaryPath()
		if err != nil {
			return err
		}

		v.Writer.Println("Copying automate-verify CLI to remote nodes")
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

		v.Writer.Println("Creating automate-verify service on remote nodes")
		cmd := fmt.Sprintf("sudo /tmp/%s verify systemd-service create", destFileName)
		excuteResults := v.SSHUtil.ExecuteConcurrently(sshConfig, cmd, hostIPs)
		isError = false
		for _, result := range excuteResults {
			if result.Error != nil {
				v.Writer.Errorf("Remote execution of cmd '%s' automate-verify CLI failed on node : %s with error: %v\n", cmd, result.HostIP, result.Error)
				isError = true
			}
		}
		if isError {
			return fmt.Errorf("remote execution failed")
		}
		// Wait for 2 seconds for the service to start
		time.Sleep(2 * time.Second)
		v.Writer.Println("Creating automate-verify service on remote nodes completed")
	}

	//TODO: Need to handle the case for AWS

	// Doing batch-check API call for remote nodes
	batchCheckRemoteReq := models.BatchCheckRequest{
		Checks: constants.GetRemoteChecks(),
		Config: batchCheckConfig,
	}

	v.Writer.Println("Doing batch-check API call for remote nodes")
	// Call batch-check API to run checks on remote nodes
	batchCheckRemoteRes, err := v.Client.MakeRequest(http.MethodPost, batchCheckAPIEndpoint, batchCheckRemoteReq)
	if err != nil {
		return fmt.Errorf("error while doing batch-check API call for remote: %v", err)
	} else {
		resultBytes, err := v.GetResultFromResponseBody(batchCheckRemoteRes.Body)
		if err != nil {
			return err
		}
		//TODO: For now just print the result as json. Need to merge the result with the response from batch-check API call for remote.
		v.Writer.Printf("Response for batch-check API for Remote: \n%s\n", string(resultBytes))
	}
	v.Writer.Println("Batch-check API call for remote nodes completed")

	return nil
}

func (v *verifyCmdFlow) GetResultFromResponseBody(body io.ReadCloser) ([]byte, error) {
	// Read the response body
	responseBody, err := io.ReadAll(body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %v", err)
	}

	// Unmarshal the response body into the ResponseBody struct
	var responseBodyStruct response.ResponseBody
	if err := json.Unmarshal(responseBody, &responseBodyStruct); err != nil {
		return nil, fmt.Errorf("failed to unmarshal response body: %v", err)
	}

	// Marshal the Result field into a byte slice
	var resultBytes []byte
	if v.prettyPrint {
		resultBytes, err = json.MarshalIndent(responseBodyStruct.Result, "", "    ")
	} else {
		resultBytes, err = json.Marshal(responseBodyStruct.Result)
	}

	if err != nil {
		return nil, fmt.Errorf("failed to marshal Result field: %v", err)
	}

	return resultBytes, nil
}

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
