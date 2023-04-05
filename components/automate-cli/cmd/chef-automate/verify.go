package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	verification "github.com/chef/automate/lib/verification"
	"github.com/spf13/cobra"
)

var verifyCmd = &cobra.Command{
	Use:   "verify",
	Short: "Verify Chef Automate setup",
	Long:  "Verify Chef Automate config files and Infrastructre",
	Annotations: map[string]string{
		docs.Compatibility: docs.Compatibility,
	},
	Args: cobra.RangeArgs(0, 1),
	RunE: runVerifyCmd,
}

var verifyCmdFlags = struct {
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
}{}

func init() {
	// flags for Verify Command
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSProvision,
		"ha-aws-provision",
		false,
		"Verifies the AWS Provision config")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSManagedProvision,
		"ha-aws-managed-provision",
		false,
		"Verifies the AWS provision config with Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haOnpremDeploy,
		"ha-onprem-deploy",
		false,
		"Verifies the On-Premise setup and config with Chef Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haOnPremAWSManagedDeploy,
		"ha-onprem-aws-deploy",
		false,
		"Verifies the On-Premise setup and config with AWS Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haOnPremCustManagedDeploy,
		"ha-onprem-customer-deploy",
		false,
		"Verifies the On-Premise setup and config with Customer Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSDeploy,
		"ha-aws-deploy",
		false,
		"Verifies the AWS Deployment setup and config")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSManagedDeploy,
		"ha-aws-managed-deploy",
		false,
		"Verifies the AWS Deployement setup and config with Managed Services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.standaloneDeploy,
		"standalone-deploy",
		false,
		"Verifies the Automate standalone setup and config")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.certificates,
		"certificates",
		false,
		"Verifies the certificates provided in the file")
	verifyCmd.PersistentFlags().StringVar(
		&verifyCmdFlags.file,
		"file",
		"",
		"Config file that needs to be verified")
	RootCmd.AddCommand(verifyCmd)
}

func runVerifyCmd(cmd *cobra.Command, args []string) error {

	var configPath = ""
	var err error
	if len(args) > 0 {
		configPath = args[0]
	}

	if verifyCmdFlags.haAWSProvision {
		err = verifyHaAWSProvision(configPath)
	}

	if verifyCmdFlags.haAWSManagedProvision {
		err = verifyHaAWSManagedProvision(configPath)
	}

	if verifyCmdFlags.haAWSDeploy {
		err = verifyHaAWSDeploy(configPath)
	}

	if verifyCmdFlags.haAWSManagedDeploy {
		err = verifyHaAWSManagedDeploy(configPath)
	}

	if verifyCmdFlags.standaloneDeploy {
		err = verifyStandaloneDeploy(configPath)
	}

	if verifyCmdFlags.haOnpremDeploy {
		err = verifyHaOnpremDeploy(configPath)
	}

	if verifyCmdFlags.haOnPremAWSManagedDeploy {
		err = verifyHaOnPremAWSManagedDeploy(configPath)
	}

	if verifyCmdFlags.haOnPremCustManagedDeploy {
		err = verifyHaOnPremCustManagedDeploy(configPath)
	}

	if verifyCmdFlags.certificates {
		err = verification.VerifyCertificates("")
	}

	if err != nil {
		return err
	}

	return nil
}

func verifyHaAWSProvision(configPath string) error {
	if isA2HARBFileExist() {
		return status.New(status.InvalidCommandArgsError, "Setup is already Provisioned. Please use --ha-aws-deploy flag.")
	} else {
		err := verification.VerifyHAAWSProvision(configPath)
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}
	}
	return nil
}

func verifyHaAWSManagedProvision(configPath string) error {
	if isA2HARBFileExist() {
		return status.New(status.InvalidCommandArgsError, "Setup is already Provisioned. Please use --ha-aws-managed-deploy flag.")
	} else {
		err := verification.VerifyHAAWSProvision(configPath)
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}
	}
	return nil
}

func verifyHaAWSDeploy(configPath string) error {
	if !isManagedServicesOn() && isA2HARBFileExist() {
		err := verification.VerifyHAAWSDeployment(configPath)
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}
	} else if !isA2HARBFileExist() {
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	} else {
		return status.New(status.InvalidCommandArgsError, "This flag will not verify the Managed Services Setup. Please use the --ha-aws-managed-deploy flag.")
	}
	return nil
}

func verifyHaAWSManagedDeploy(configPath string) error {
	if isManagedServicesOn() && isA2HARBFileExist() {
		err := verification.VerifyHAAWSManagedDeployment(configPath)
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}
	} else if !isA2HARBFileExist() {
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	} else {
		return status.New(status.InvalidCommandArgsError, "Managed Services flag is not set. Cannot verify the config.")
	}
	return nil
}

func verifyStandaloneDeploy(configPath string) error {
	if isA2HARBFileExist() {
		return status.New(status.InvalidCommandArgsError, "Deployment type does not match with the requested flag.")
	} else if !isA2HARBFileExist() {
		err := verification.VerifyStandaloneDeployment(configPath)
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}
	}
	return nil
}

func verifyHaOnpremDeploy(configPath string) error {
	err := verification.VerifyOnPremDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}

func verifyHaOnPremAWSManagedDeploy(configPath string) error {
	err := verification.VerifyOnPremAWSManagedDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}

func verifyHaOnPremCustManagedDeploy(configPath string) error {
	err := verification.VerifyOnPremCustManagedDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}
