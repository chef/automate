package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
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
	RootCmd.AddCommand(verifyCmd)
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
	var err error
	if len(args) > 0 {
		configPath = args[0]
	}

	if flagsObj.haAWSProvision {
		err = v.verifyHaAWSProvision(configPath)
	} else if flagsObj.haAWSManagedProvision {
		err = v.verifyHaAWSManagedProvision(configPath)
	} else if flagsObj.haAWSDeploy {
		err = v.verifyHaAWSDeploy(configPath)
	} else if flagsObj.haAWSManagedDeploy {
		err = v.verifyHaAWSManagedDeploy(configPath)
	} else if flagsObj.standaloneDeploy {
		err = v.verifyStandaloneDeploy(configPath)
	} else if flagsObj.haOnpremDeploy {
		err = v.verifyHaOnpremDeploy(configPath)
	} else if flagsObj.haOnPremAWSManagedDeploy {
		err = v.verifyHaOnPremAWSManagedDeploy(configPath)
	} else if flagsObj.haOnPremCustManagedDeploy {
		err = v.verifyHaOnPremCustManagedDeploy(configPath)
	} else if flagsObj.certificates {
		err = v.Verification.VerifyCertificates("")
	}

	if err != nil {
		return err
	}

	return nil
}

func (v *verifyCmdFlow) verifyHaAWSProvision(configPath string) error {
	if v.A2HARBFileExist {
		return status.New(status.InvalidCommandArgsError, "Setup is already Provisioned. Please use --ha-aws-deploy flag.")
	}
	err := v.Verification.VerifyHAAWSProvision(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func (v *verifyCmdFlow) verifyHaAWSManagedProvision(configPath string) error {
	if v.A2HARBFileExist {
		return status.New(status.InvalidCommandArgsError, "Setup is already Provisioned. Please use --ha-aws-managed-deploy flag.")
	}
	err := v.Verification.VerifyHAAWSManagedProvision(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func (v *verifyCmdFlow) verifyHaAWSDeploy(configPath string) error {
	if !v.ManagedServicesOn {
		if v.A2HARBFileExist {
			err := v.Verification.VerifyHAAWSDeployment(configPath)
			if err != nil {
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
			err := v.Verification.VerifyHAAWSManagedDeployment(configPath)
			if err != nil {
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
	err := v.Verification.VerifyStandaloneDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	return nil
}

func (v *verifyCmdFlow) verifyHaOnpremDeploy(configPath string) error {
	err := v.Verification.VerifyOnPremDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}

func (v *verifyCmdFlow) verifyHaOnPremAWSManagedDeploy(configPath string) error {
	err := v.Verification.VerifyOnPremAWSManagedDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}

func (v *verifyCmdFlow) verifyHaOnPremCustManagedDeploy(configPath string) error {
	err := v.Verification.VerifyOnPremCustManagedDeployment(configPath)
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}
	return nil
}
