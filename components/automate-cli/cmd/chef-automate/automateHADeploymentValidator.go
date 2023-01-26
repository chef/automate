package main

import (
	automatehadeploymentvalidator "github.com/chef/automate/components/automate-cli/cmd/chef-automate/automate-ha-deployment-validator"
	"github.com/spf13/cobra"
)

func init() {

	validatorCmd := &cobra.Command{
		Use:   "validate-ha-infra COMMAND",
		Short: "Chef Automate HA Infra Validator",
		Long:  "Chef Automate HA Infra Validator, this command should always be executed from AutomateHA Bastion Node.",
	}

	runValidatorCmd := &cobra.Command{
		Use:   "run",
		Short: "Run Chef Automate HA Infra Validator",
		Long:  "Chef Automate CLI command to validate HA Infrastructure, this command should always be executed from AutomateHA Bastion Node",
		RunE:  runInfraValidator,
	}

	validatorCmd.AddCommand(runValidatorCmd)
	RootCmd.AddCommand(validatorCmd)
}

func runInfraValidator(cmd *cobra.Command, args []string) error {
	validator, err := automatehadeploymentvalidator.NewHADeploymentValidator()
	if err != nil {
		return err
	}
	return validator.RunHAValidator()
}
