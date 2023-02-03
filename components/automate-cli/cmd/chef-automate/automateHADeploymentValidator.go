package main

import (
	"fmt"

	automatehadeploymentvalidator "github.com/chef/automate/components/automate-cli/cmd/chef-automate/automate-ha-deployment-validator"
	"github.com/fatih/color"
	"github.com/spf13/cobra"
)

var flags = automatehadeploymentvalidator.ValidatorFlags{}

func init() {
	validatorCmd := &cobra.Command{
		//Use:   "validate-ha-infra COMMAND",
		Use:   "validate COMMAND",
		Short: "Chef Automate Infra Validator",
		Long:  "Chef Automate Infra Validator, incase of HA, this command should always be executed from AutomateHA Bastion Node.",
	}

	runConfValidatorCmd := &cobra.Command{
		Use:   "deploy [/path/to/config.toml]",
		Short: "Run Chef Automate Infra validation based on configuration file",
		Long:  "Chef Automate CLI command to validate automate Infrastructure based on configuration file, incase of HA, this command should always be executed from AutomateHA Bastion Node",
		RunE:  runConfValidator,
	}

	runConfValidatorCmd.PersistentFlags().BoolVarP(&flags.All, "all", "f", false, "Run validations for all types of nodes")

	runConfValidatorCmd.PersistentFlags().BoolVarP(&flags.Bastion, "bastion", "b", false, "Run validations only for bastion nodes")

	runConfValidatorCmd.PersistentFlags().BoolVarP(&flags.Automate, CONST_AUTOMATE, "a", false, "Run validations only for automate nodes")
	runConfValidatorCmd.PersistentFlags().BoolVar(&flags.Automate, "a2", false, "Run validations only for automate nodes")

	runConfValidatorCmd.PersistentFlags().BoolVarP(&flags.Chefserver, CONST_CHEF_SERVER, "c", false, "Run validations only for chef server nodes")
	runConfValidatorCmd.PersistentFlags().BoolVar(&flags.Chefserver, "cs", false, "Run validations only for chef server nodes")

	runConfValidatorCmd.PersistentFlags().BoolVarP(&flags.Postgresql, CONST_POSTGRESQL, "p", false, "Run validations only for postgresql nodes")
	runConfValidatorCmd.PersistentFlags().BoolVar(&flags.Postgresql, "pg", false, "Run validations only for postgresql nodes")

	runConfValidatorCmd.PersistentFlags().BoolVarP(&flags.Opensearch, CONST_OPENSEARCH, "o", false, "Run validations only for opensearch nodes")
	runConfValidatorCmd.PersistentFlags().BoolVar(&flags.Opensearch, "os", false, "Run validations only for opensearch nodes")

	validatorCmd.AddCommand(runConfValidatorCmd)

	provisionValidatorCmd := &cobra.Command{
		Use:   "provision-infra [/path/to/config.toml]",
		Short: "Run Chef Automate Infra provision validation based on configuration file",
		Long:  "Chef Automate CLI command to validate automate Infrastructure provision based on configuration file, this command should always be executed from AutomateHA Bastion Node",
		RunE:  provisionValidator,
	}

	validatorCmd.AddCommand(provisionValidatorCmd)

	RootCmd.AddCommand(validatorCmd)
}

func runConfValidator(cmd *cobra.Command, args []string) error {
	var configPath = ""
	if len(args) > 0 {
		configPath = args[0]
	} else {
		return fmt.Errorf("Config file is mandatory")
	}
	//return ConfigValidator(flags, configPath)
	validator, err := automatehadeploymentvalidator.NewHADeploymentValidator(flags, configPath)
	if err != nil {
		return err
	}
	return RunValidator(validator)
}

func provisionValidator(cmd *cobra.Command, args []string) error {
	var configPath = ""
	if len(args) > 0 {
		configPath = args[0]
	} else {
		return fmt.Errorf("provision Config file is mandatory")
	}
	validator, err := automatehadeploymentvalidator.NewHADeploymentValidator(flags, configPath)
	if err != nil {
		return err
	}
	return RunValidator(validator)
}

func RunValidator(validator automatehadeploymentvalidator.HADeploymentValidator) error {
	resp, err := validator.RunHAValidator()
	if err != nil {
		return err
	}
	if len(resp) == 0 {

		fmt.Println("Overall Validation: ", color.New(color.FgGreen).Sprint("✔"), " ", color.New(color.FgGreen).Sprint("Passed"))
		return nil
	}

	fmt.Println("Overall Validation: ", color.New(color.FgHiRed).Sprint("x"), " ", color.New(color.FgHiRed).Sprint("Failed"))
	for _, v := range resp {
		fmt.Println(v)
	}

	return nil
}
