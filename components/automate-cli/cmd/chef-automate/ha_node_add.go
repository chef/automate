package main

import (
	"fmt"
	"path/filepath"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var nodeCmd = &cobra.Command{
	Use:    "node COMMAND",
	Short:  "This command is used to add or delete HA nodes",
	Hidden: false,
}

func addNodeHACmd() *cobra.Command {
	var addDeleteNodeHACmdFlags = AddDeleteNodeHACmdFlags{}
	var addNodeHACmd = &cobra.Command{
		Use:   "add",
		Short: "Add new node in HA",
		Long:  `Add new node in HA`,
		RunE:  runAddNodeHACmd(&addDeleteNodeHACmdFlags),
	}
	addNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.automateIp, "automate-ips", "A", "", "New automate ip addresses to be added. Works with --onprem-mode flag")
	addNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.chefServerIp, "chef-server-ips", "C", "", "New chef-server ip addresses to be added. Works with --onprem-mode flag")
	addNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.opensearchIp, "opensearch-ips", "O", "", "New opensearch ip addresses to be added. Works with --onprem-mode flag")
	addNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.postgresqlIp, "postgresql-ips", "P", "", "New postgres ip addresses to be added. Works with --onprem-mode flag")
	addNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.automateCount, "automate-count", "a", 0, "New automate instances to be added. Works with --aws-mode flag")
	addNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.chefServerCount, "chef-server-count", "c", 0, "New chef-server instances to be added. Works with --aws-mode flag")
	addNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.opensearchCount, "opensearch-count", "o", 0, "New opensearch instances to be added. Works with --aws-mode flag")
	addNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.postgresqlCount, "postgresql-count", "p", 0, "New postgresql instances to be added. Works with --aws-mode flag")
	addNodeHACmd.PersistentFlags().BoolVar(&addDeleteNodeHACmdFlags.onPremMode, "onprem-mode", false, "Use this flag if the deployment type is on prem")
	addNodeHACmd.PersistentFlags().BoolVar(&addDeleteNodeHACmdFlags.awsMode, "aws-mode", false, "Use this flag if the deployment type is AWS")
	addNodeHACmd.PersistentFlags().BoolVarP(&addDeleteNodeHACmdFlags.autoAccept, "auto-accept", "y", false, "auto-accept")

	return addNodeHACmd
}

func runAddNodeHACmd(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags) func(c *cobra.Command, args []string) error {
	return func(c *cobra.Command, args []string) error {
		configPath := filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml")
		deployerType, err := getModeFromConfig(configPath)
		if err != nil {
			return err
		}
		nodeAdder, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, configPath)
		if err != nil {
			return err
		}
		return nodeAdder.Execute(c, args)
	}
}

func haAddNodeFactory(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags, deployerType, configPath string) (HAModifyAndDeploy, error) {
	if addDeleteNodeHACmdFlags.onPremMode && addDeleteNodeHACmdFlags.awsMode {
		return nil, errors.New("Cannot use both --onprem-mode and --aws-mode together. Provide only one at a time")
	}
	var hamd HAModifyAndDeploy
	var err error
	switch deployerType {
	case EXISTING_INFRA_MODE:
		if !addDeleteNodeHACmdFlags.awsMode {
			hamd = NewAddNodeOnPrem(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{}))
		} else {
			err = fmt.Errorf("Flag given does not match with the current deployment type %s. Try with --aws-mode flag", deployerType)
		}
	case AWS_MODE:
		if !addDeleteNodeHACmdFlags.onPremMode {
			hamd = NewAddNodeAWS(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{}))
		} else {
			err = fmt.Errorf("Flag given does not match with the current deployment type %s. Try with --onprem-mode flag", deployerType)
		}
	default:
		err = fmt.Errorf("Unsupported deployment type. Please check %s", configPath)
	}
	return hamd, err
}

func init() {
	nodeCmd.AddCommand(addNodeHACmd())
	nodeCmd.AddCommand(deleteNodeHACmd())
	RootCmd.AddCommand(nodeCmd)
}
