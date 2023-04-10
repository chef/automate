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
	Long:  "Verify Chef Automate config files and Infrastructure",
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

var A2HARBFileExist bool

func init() {
	// flags for Verify Command
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSProvision,
		"ha-aws-provision",
		false,
		"Use this flag to verify the AWS Provision config")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSManagedProvision,
		"ha-aws-managed-provision",
		false,
		"Use this flag to verify the AWS Provision config with Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haOnpremDeploy,
		"ha-onprem-deploy",
		false,
		"Use this flag to verify the On-Premise setup and config with Chef Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haOnPremAWSManagedDeploy,
		"ha-onprem-aws-deploy",
		false,
		"Use this flag to verify the On-Premise setup and config with AWS Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haOnPremCustManagedDeploy,
		"ha-onprem-customer-deploy",
		false,
		"Use this flag to verify the On-Premise setup and config with Customer Managed services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSDeploy,
		"ha-aws-deploy",
		false,
		"Use this flag to verify the AWS Deployment setup and config")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.haAWSManagedDeploy,
		"ha-aws-managed-deploy",
		false,
		"Use this flag to verify the AWS Deployment setup and config with Managed Services")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.standaloneDeploy,
		"standalone-deploy",
		false,
		"Use this flag to verify the Automate standalone setup and config")
	verifyCmd.PersistentFlags().BoolVar(
		&verifyCmdFlags.certificates,
		"certificates",
		false,
		"Use this flag to verify the certificates provided in the file")
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

	A2HARBFileExist = isA2HARBFileExist()
	if verifyCmdFlags.haAWSProvision {
		err = verifyHaAWSProvision(configPath)
	} else if verifyCmdFlags.haAWSManagedProvision {
		err = verifyHaAWSManagedProvision(configPath)
	} else if verifyCmdFlags.haAWSDeploy {
		err = verifyHaAWSDeploy(configPath)
	} else if verifyCmdFlags.haAWSManagedDeploy {
		err = verifyHaAWSManagedDeploy(configPath)
	} else if verifyCmdFlags.standaloneDeploy {
		err = verifyStandaloneDeploy(configPath)
	} else if verifyCmdFlags.haOnpremDeploy {
		err = verifyHaOnpremDeploy(configPath)
	} else if verifyCmdFlags.haOnPremAWSManagedDeploy {
		err = verifyHaOnPremAWSManagedDeploy(configPath)
	} else if verifyCmdFlags.haOnPremCustManagedDeploy {
		err = verifyHaOnPremCustManagedDeploy(configPath)
	} else if verifyCmdFlags.certificates {
		err = verification.VerifyCertificates("")
	}

	if err != nil {
		return err
	}

	return nil
}

func verifyHaAWSProvision(configPath string) error {
	if A2HARBFileExist {
		return status.New(status.InvalidCommandArgsError, "Setup is already Provisioned. Please use --ha-aws-deploy flag.")
	}
	err := verification.VerifyHAAWSProvision(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func verifyHaAWSManagedProvision(configPath string) error {
	if A2HARBFileExist {
		return status.New(status.InvalidCommandArgsError, "Setup is already Provisioned. Please use --ha-aws-managed-deploy flag.")
	}
	err := verification.VerifyHAAWSManagedProvision(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func verifyHaAWSDeploy(configPath string) error {
	if !isManagedServicesOn() {
		if A2HARBFileExist {
			err := verification.VerifyHAAWSDeployment(configPath)
			if err != nil {
				return status.Annotate(err, status.ConfigError)
			}
			return nil
		}
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	}
	return status.New(status.InvalidCommandArgsError, "This flag will not verify the Managed Services Setup. Please use the --ha-aws-managed-deploy flag.")
}

func verifyHaAWSManagedDeploy(configPath string) error {
	if isManagedServicesOn() {
		if A2HARBFileExist {
			err := verification.VerifyHAAWSManagedDeployment(configPath)
			if err != nil {
				return status.Annotate(err, status.ConfigError)
			}
			return nil
		}
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	}
	return status.New(status.InvalidCommandArgsError, "Managed Services flag is not set. Cannot verify the config.")
}

func verifyStandaloneDeploy(configPath string) error {
	if A2HARBFileExist {
		return status.New(status.InvalidCommandArgsError, "Deployment type does not match with the requested flag.")
	}
	err := verification.VerifyStandaloneDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
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
