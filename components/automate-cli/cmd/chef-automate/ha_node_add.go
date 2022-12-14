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
		nodeAdder, err := haAddNodeFactory(addDeleteNodeHACmdFlags)
		if err != nil {
			return err
		}
		return nodeAdder.Execute(c, args)
	}
}

func haAddNodeFactory(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags) (HAModifyAndDeploy, error) {
	if addDeleteNodeHACmdFlags.onPremMode {
		return NewAddNodeOnPrem(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
	} else if addDeleteNodeHACmdFlags.awsMode {
		return NewAddNodeAWS(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
	} else {
		deployerType, err := getModeFromConfig(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"))
		if err != nil {
			return nil, err
		}
		if deployerType == EXISTING_INFRA_MODE {
			return NewAddNodeOnPrem(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
		} else if deployerType == AWS_MODE {
			return NewAddNodeAWS(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
		} else {
			return nil, errors.New(fmt.Sprintf("Unsupported deployment type. Please check %s", filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml")))
		}
	}
}

func init() {
	nodeCmd.AddCommand(addNodeHACmd())
	nodeCmd.AddCommand(deleteNodeHACmd())
	RootCmd.AddCommand(nodeCmd)
}
