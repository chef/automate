package main

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver"
	verification "github.com/chef/automate/lib/verification"
	"github.com/spf13/cobra"
)

type verifyCmdFlags struct {
	file                      string
	haAWSProvision            bool
	haAWSManagedProvision     bool
	haOnpremDeploy            bool
	haOnPremAWSManagedDeploy  bool
	haOnPremCustManagedDeploy bool
	haAWSDeploy               bool
	haAWSManagedDeploy        bool
	standaloneDeploy          bool
	certificates              bool
}

type verifyCmdFlow struct {
	Verification      verification.Verification
	A2HARBFileExist   bool
	ManagedServicesOn bool
}

type verifyServeCmdFlow struct {
	sshUtil SSHUtil
}

var verifyServeCmd = &cobra.Command{
	Use:   "serve",
	Short: "Start verify server",
	Long:  "Start verify server for running checks",
	Annotations: map[string]string{
		docs.Compatibility: docs.Compatibility,
	},
	Args: cobra.ExactArgs(0),
	RunE: verifyServeCmdFunc(),
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
		Args: cobra.RangeArgs(0, 1),
		RunE: verifyCmdFunc(&flagsObj),
	}

	// flags for Verify Command
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.haAWSProvision,
		"ha-aws-provision",
		false,
		"Use this flag to verify the AWS Provision config")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.haAWSManagedProvision,
		"ha-aws-managed-provision",
		false,
		"Use this flag to verify the AWS Provision config with Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.haOnpremDeploy,
		"ha-onprem-deploy",
		false,
		"Use this flag to verify the On-Premise setup and config with Chef Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.haOnPremAWSManagedDeploy,
		"ha-onprem-aws-deploy",
		false,
		"Use this flag to verify the On-Premise setup and config with AWS Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.haOnPremCustManagedDeploy,
		"ha-onprem-customer-deploy",
		false,
		"Use this flag to verify the On-Premise setup and config with Customer Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.haAWSDeploy,
		"ha-aws-deploy",
		false,
		"Use this flag to verify the AWS Deployment setup and config")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.haAWSManagedDeploy,
		"ha-aws-managed-deploy",
		false,
		"Use this flag to verify the AWS Deployment setup and config with Managed Services")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.standaloneDeploy,
		"standalone-deploy",
		false,
		"Use this flag to verify the Automate standalone setup and config")
	verifyCmd.PersistentFlags().BoolVar(
		&flagsObj.certificates,
		"certificates",
		false,
		"Use this flag to verify the certificates provided in the file")
	verifyCmd.PersistentFlags().StringVar(
		&flagsObj.file,
		"file",
		"",
		"Config file that needs to be verified")

	verifyCmd.AddCommand(verifyServeCmd)
	RootCmd.AddCommand(verifyCmd)
}

func verifyServeCmdFunc() func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		c := verifyServeCmdFlow{
			sshUtil: NewSSHUtil(&SSHConfig{}),
		}
		return c.runVerifyServeCmd(cmd, args)
	}
}

func (v *verifyServeCmdFlow) runVerifyServeCmd(cmd *cobra.Command, args []string) error {
	verifyserver.Start()
	return nil
}

func verifyCmdFunc(flagsObj *verifyCmdFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		c := verifyCmdFlow{
			Verification:      &verification.VerificationModule{},
			A2HARBFileExist:   isA2HARBFileExist(),
			ManagedServicesOn: isManagedServicesOn(),
		}
		return c.runVerifyCmd(cmd, args, flagsObj)
	}
}

func (v *verifyCmdFlow) runVerifyCmd(cmd *cobra.Command, args []string, flagsObj *verifyCmdFlags) error {
	var configPath = ""
	if len(args) > 0 {
		configPath = args[0]
	}

	switch true {
	case flagsObj.haAWSProvision:
		err := v.verifyHaAWSProvision(configPath)
		return err
	case flagsObj.haAWSManagedProvision:
		err := v.verifyHaAWSManagedProvision(configPath)
		return err
	case flagsObj.haAWSDeploy:
		err := v.verifyHaAWSDeploy(configPath)
		return err
	case flagsObj.haAWSManagedDeploy:
		err := v.verifyHaAWSManagedDeploy(configPath)
		return err
	case flagsObj.standaloneDeploy:
		err := v.verifyStandaloneDeploy(configPath)
		return err
	case flagsObj.haOnpremDeploy:
		err := v.verifyHaOnpremDeploy(configPath)
		return err
	case flagsObj.haOnPremAWSManagedDeploy:
		err := v.verifyHaOnPremAWSManagedDeploy(configPath)
		return err
	case flagsObj.haOnPremCustManagedDeploy:
		err := v.verifyHaOnPremCustManagedDeploy(configPath)
		return err
	case flagsObj.certificates:
		err := v.Verification.VerifyCertificates("")
		return err
	default:
		fmt.Println(cmd.UsageString())
		return nil
	}

}

func (v *verifyCmdFlow) verifyHaAWSProvision(configPath string) error {
	if err := v.Verification.VerifyHAAWSProvision(configPath); err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func (v *verifyCmdFlow) verifyHaAWSManagedProvision(configPath string) error {
	if err := v.Verification.VerifyHAAWSManagedProvision(configPath); err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func (v *verifyCmdFlow) verifyHaAWSDeploy(configPath string) error {
	if !v.ManagedServicesOn {
		if v.A2HARBFileExist {
			if err := v.Verification.VerifyHAAWSDeployment(configPath); err != nil {
				return status.Annotate(err, status.ConfigError)
			}
			return nil
		}
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	}
	return status.New(status.InvalidCommandArgsError, "This flag will not verify the Managed Services Setup. Please use the --ha-aws-managed-deploy flag.")
}

func (v *verifyCmdFlow) verifyHaAWSManagedDeploy(configPath string) error {
	if v.ManagedServicesOn {
		if v.A2HARBFileExist {
			if err := v.Verification.VerifyHAAWSManagedDeployment(configPath); err != nil {
				return status.Annotate(err, status.ConfigError)
			}
			return nil
		}
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	}
	return status.New(status.InvalidCommandArgsError, "Managed Services flag is not set. Cannot verify the config.")
}

func (v *verifyCmdFlow) verifyStandaloneDeploy(configPath string) error {
	if v.A2HARBFileExist {
		return status.New(status.InvalidCommandArgsError, "Deployment type does not match with the requested flag.")
	}
	if err := v.Verification.VerifyStandaloneDeployment(configPath); err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func (v *verifyCmdFlow) verifyHaOnpremDeploy(configPath string) error {
	if err := v.Verification.VerifyOnPremDeployment(configPath); err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}

func (v *verifyCmdFlow) verifyHaOnPremAWSManagedDeploy(configPath string) error {
	if err := v.Verification.VerifyOnPremAWSManagedDeployment(configPath); err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}

func (v *verifyCmdFlow) verifyHaOnPremCustManagedDeploy(configPath string) error {
	if err := v.Verification.VerifyOnPremCustManagedDeployment(configPath); err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}
