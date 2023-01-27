package main

import (
	"fmt"

	automatehadeploymentvalidator "github.com/chef/automate/components/automate-cli/cmd/chef-automate/automate-ha-deployment-validator"
	"github.com/spf13/cobra"
)

var flags = automatehadeploymentvalidator.ValidatorFlags{}

func init() {
	validatorCmd := &cobra.Command{
		Use:   "validate-ha-infra COMMAND",
		Short: "Chef Automate HA Infra Validator",
		Long:  "Chef Automate HA Infra Validator, this command should always be executed from AutomateHA Bastion Node.",
	}

	runValidatorCmd := &cobra.Command{
		Use:   "run [/path/to/config.toml]",
		Short: "Run Chef Automate HA Infra Validator",
		Long:  "Chef Automate CLI command to validate HA Infrastructure, this command should always be executed from AutomateHA Bastion Node",
		RunE:  runInfraValidator,
	}

	runValidatorCmd.PersistentFlags().BoolVarP(&flags.All, "all", "f", false, "Run validations for all types of nodes")

	runValidatorCmd.PersistentFlags().BoolVarP(&flags.Bastion, "bastion", "b", false, "Run validations only for bastion nodes")

	runValidatorCmd.PersistentFlags().BoolVarP(&flags.Automate, CONST_AUTOMATE, "a", false, "Run validations only for automate nodes")
	runValidatorCmd.PersistentFlags().BoolVar(&flags.Automate, "a2", false, "Run validations only for automate nodes")

	runValidatorCmd.PersistentFlags().BoolVarP(&flags.Chefserver, CONST_CHEF_SERVER, "c", false, "Run validations only for chef server nodes")
	runValidatorCmd.PersistentFlags().BoolVar(&flags.Chefserver, "cs", false, "Run validations only for chef server nodes")

	runValidatorCmd.PersistentFlags().BoolVarP(&flags.Postgresql, CONST_POSTGRESQL, "p", false, "Run validations only for postgresql nodes")
	runValidatorCmd.PersistentFlags().BoolVar(&flags.Postgresql, "pg", false, "Run validations only for postgresql nodes")

	runValidatorCmd.PersistentFlags().BoolVarP(&flags.Opensearch, CONST_OPENSEARCH, "o", false, "Run validations only for opensearch nodes")
	runValidatorCmd.PersistentFlags().BoolVar(&flags.Opensearch, "os", false, "Run validations only for opensearch nodes")

	validatorCmd.AddCommand(runValidatorCmd)
	RootCmd.AddCommand(validatorCmd)
}

func runInfraValidator(cmd *cobra.Command, args []string) error {
	var configPath = ""
	if len(args) > 0 {
		configPath = args[0]
	} else {
		return fmt.Errorf("Config file is mandatory")
	}

	validator, err := automatehadeploymentvalidator.NewHADeploymentValidator(flags, configPath)
	if err != nil {
		return err
	}
	return validator.RunHAValidator()
}
