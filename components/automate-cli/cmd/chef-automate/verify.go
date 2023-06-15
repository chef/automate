package main

import (
	"encoding/json"
	"fmt"
	"io"
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
	"github.com/chef/automate/lib/version"
	"github.com/spf13/cobra"
)

const (
	VERIFY_SERVER_PORT        = "VERIFY_SERVER_PORT"
	BINARY_DESTINATION_FOLDER = "/etc/automate-verify"
	SYSTEMD_PATH              = "/etc/systemd/system"
)

type verifyCmdFlags struct {
	config string
	debug  bool
}

type verifyCmdFlow struct {
	Client               httputils.HTTPClient
	CreateSystemdService *verifysystemdcreate.CreateSystemdService
	Config               *config.HaDeployConfig
	Writer               *cli.Writer
}

func NewVerifyCmdFlow(client httputils.HTTPClient, createSystemdService *verifysystemdcreate.CreateSystemdService, config *config.HaDeployConfig, writer *cli.Writer) *verifyCmdFlow {
	return &verifyCmdFlow{
		Client:               client,
		CreateSystemdService: createSystemdService,
		Config:               config,
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
		"enable debugging")

	verifyServeCmd.Flags().BoolVarP(
		&flagsObj.debug,
		"debug",
		"d",
		false,
		"enable debugging")

	verifySystemdServiceCreateCmd.Flags().BoolVarP(
		&flagsObj.debug,
		"debug",
		"d",
		false,
		"enable debugging")

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

		l, err := logger.NewLogger("text", defaultLevel)
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

		c := NewVerifyCmdFlow(httputils.NewClient(l), createSystemdServiceWithBinary, config.NewHaDeployConfig(), writer)
		return c.runVerifyCmd(cmd, args, flagsObj)
	}
}

func (v *verifyCmdFlow) runVerifyCmd(cmd *cobra.Command, args []string, flagsObj *verifyCmdFlags) error {
	var configPath = ""

	// TODO : config flag is optional for now. Need to handle the default config path
	if len(strings.TrimSpace(flagsObj.config)) > 0 {
		configPath = flagsObj.config
	}

	err := v.Config.ParseAndVerify(configPath)
	if err != nil {
		return err
	}

	// Get config required for batch-check API call
	batchCheckConfig := &models.Config{}
	batchCheckConfig.PopulateWith(v.Config)

	// Check if automate-verify service is already running on bastion,
	// else add automate-verify service to systemd and start the automate-verify service,
	// upgrade if needed.
	err = v.checkAutomateVerifyServiceOnBastion(*batchCheckConfig)
	if err != nil {
		return err
	}

	return nil
}

func (v *verifyCmdFlow) checkAutomateVerifyServiceOnBastion(batchCheckConfig models.Config) error {
	port := getPort()
	res, err := v.Client.MakeRequest("GET", fmt.Sprintf("http://localhost:%s/status", port), nil)
	if err != nil {
		v.Writer.Println(fmt.Sprintf("error while checking automate-verify service on bastion: %v\n", err))
		v.Writer.Println("Adding automate-verify service to systemd and starting the service")
		err := v.CreateSystemdService.Create()
		if err != nil {
			return err
		}
		// Wait for 2 seconds for the service to start
		time.Sleep(2 * time.Second)
	} else {
		resultBytes, err := getResultFromResponseBody(res.Body)
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
			v.Writer.Println("Automate CLI Upgrade completed on bastion")
			// Wait for 2 seconds for the service to start
			time.Sleep(2 * time.Second)
		}
	}

	// Running batch-check API call for bastion

	batchCheckBastionReq := models.BatchCheckRequest{
		Checks: []string{constants.HARDWARE_RESOURCE_COUNT, constants.CERTIFICATE, constants.SSH_USER},
		Config: batchCheckConfig,
	}

	// reqJson, err := json.Marshal(batchCheckBastionReq)
	// if err != nil {
	// 	return fmt.Errorf("failed to marshal Request field: %v", err)
	// }
	// fmt.Println("Request Body: ", string(reqJson))

	batchCheckBastionRes, err := v.Client.MakeRequest("POST", fmt.Sprintf("http://localhost:%s/api/v1/checks/batch-checks", port), batchCheckBastionReq)
	if err != nil {
		return fmt.Errorf("error while doing batch-check API call for bastion: %v", err)
	} else {
		resultBytes, err := getResultFromResponseBody(batchCheckBastionRes.Body)
		if err != nil {
			return err
		}
		//TODO: For now just print the result as json. Need to merge the result with the response from batch-check API call for remote.
		v.Writer.Println(fmt.Sprintf("Response for batch-check API on Bastion: %s", string(resultBytes)))
	}
	return nil
}

func getResultFromResponseBody(body io.ReadCloser) ([]byte, error) {
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
	resultBytes, err := json.Marshal(responseBodyStruct.Result)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal Result field: %v", err)
	}

	return resultBytes, nil
}

func getPort() string {
	port := server.DEFAULT_PORT
	env_port := os.Getenv(VERIFY_SERVER_PORT)
	if env_port != "" {
		if _, err := strconv.Atoi(env_port); err == nil {
			port = env_port
		}
	}
	return port
}
