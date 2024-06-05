package main

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/gofrs/uuid"
	"github.com/spf13/cobra"
	"google.golang.org/grpc"
)

func init() {
	infrastructureCmd.AddCommand(nodeDeleteCmd)

	RootCmd.AddCommand(infrastructureCmd)
}

var infrastructureCmd = &cobra.Command{
	Use:               "infrastructure COMMAND",
	Short:             "Chef Automate infrastructure",
	Long:              "Commands for automation infrastructure management, for data related to chef-client runs and chef-server actions.",
	PersistentPreRunE: preInfrastructureCmd,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var nodeDeleteCmd = &cobra.Command{
	Use:               "node-delete [uuid]",
	Short:             "Delete node by node uuid",
	Long:              "",
	PersistentPreRunE: checkLicenseStatusForExpiry,
	RunE:              runDeleteNodeCmd,
	Args:              cobra.ExactArgs(1),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

type DSClient interface {
	InfrastructureNodeDelete(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error)
	Close() error
}

type InfraFlow struct {
	DsClient DSClient
	Writer   *cli.Writer
}

type LicenseResult struct {
	Result           LicenseStatus `json:"result"`
	ErrorType        string        `json:"error_type"`
	ErrorDescription string        `json:"error_description"`
}

type LicenseStatus struct {
	CustomerName   string         `json:"customer_name"`
	LicenseType    string         `json:"license_type"`
	LicenseId      string         `json:"license_id"`
	ExpirationDate ExpirationDate `json:"expiration_date"`
	GracePeriod    bool           `json:"grace_period"`
}

type ExpirationDate struct {
	Seconds int64 `json:"seconds"`
}

func runDeleteNodeCmd(cmd *cobra.Command, args []string) error {
	ifw, err := NewDeleteNode(writer)
	if err != nil {
		return err
	}
	return ifw.RunDeleteNode(args[0])
}

func preInfrastructureCmd(cmd *cobra.Command, args []string) error {
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
		os.Exit(0)
	}
	return nil
}

func checkLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	fileName := "/tmp/license"
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}

	cmd1 := exec.Command("chef-automate", "license", "status", "--result-json", fileName)
	//Not checking for error, as if the license is expired it will still return error and for commercial license a grace period is required
	cmd1.Output()

	licenseResult, err := readFileAndMarshal(fileName)
	if err != nil {
		return err
	}

	err = checkLicenseExpiry(licenseResult)
	if err != nil {
		return err
	}

	return nil
}

func readFileAndMarshal(fileName string) (*LicenseResult, error) {
	var result LicenseResult

	byteValue, err := os.ReadFile(fileName)
	if err != nil {
		return nil, err
	}

	defer os.Remove(fileName)
	json.Unmarshal([]byte(byteValue), &result)

	return &result, nil

}

func checkLicenseExpiry(licenseResult *LicenseResult) error {
	commercial := "commercial"
	if licenseResult.Result.LicenseId == "" {
		if licenseResult.ErrorType != "" {
			return status.New(
				status.DeploymentServiceCallError,
				licenseResult.ErrorDescription,
			)
		}
		return status.New(
			status.LicenseError,
			"Please apply a license.Please contact sales@chef.io to have your Chef Automate license.",
		)
	}
	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0) //gives unix time stamp in utc

	//If the license type is commercial, adding grace period of 1 week
	if licenseResult.Result.LicenseType == commercial {
		//Adding grace period for 7 days i.e. one week
		licenseValidDate = licenseValidDate.AddDate(0, 0, 7)
	}

	if licenseValidDate.Before(time.Now()) {
		return status.New(
			status.LicenseError,
			"This license has expired. Please contact sales@chef.io to renew your Chef Automate license.",
		)
	}

	return nil

}

func NewDeleteNode(writer *cli.Writer) (*InfraFlow, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)

	if err != nil {
		return nil, err
	}
	return &InfraFlow{DsClient: connection, Writer: writer}, nil
}

func (ifw *InfraFlow) RunDeleteNode(nodeID string) error {

	defer ifw.DsClient.Close()

	if !isValidUUID(nodeID) {
		return status.New(status.InvalidCommandArgsError, "argument in not a valid node UUID")
	}
	deleteReq := &api.InfrastructureNodeDeleteRequest{NodeId: nodeID}

	_, err := ifw.DsClient.InfrastructureNodeDelete(context.Background(), deleteReq)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Request to delete node failed")
	}

	ifw.Writer.Println("Node successfully deleted")
	return nil
}

func isValidUUID(id string) bool {
	_, err := uuid.FromString(id)
	return err == nil
}
