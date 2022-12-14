package main

import (
	"fmt"
	"path/filepath"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func deleteNodeHACmd() *cobra.Command {
	var addDeleteNodeHACmdFlags = AddDeleteNodeHACmdFlags{}
	var deleteNodeHACmd = &cobra.Command{
		Use:   "remove",
		Short: "remove existing node in HA",
		Long:  `remove existing node in HA`,
		RunE:  runDeleteNodeHACmd(&addDeleteNodeHACmdFlags),
	}
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.automateIp, "automate", "A", "", "Automate ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.chefServerIp, "chef-server", "C", "", "Chef-server ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.opensearchIp, "opensearch", "O", "", "OpenSearch ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.postgresqlIp, "postgresql", "P", "", "Postgresql ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.automateCount, "automate-count", "a", 0, "No of Automate instances to be removed. Works with --aws-mode flag")
	deleteNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.chefServerCount, "chef-server-count", "c", 0, "No of chef-server instances to be removed. Works with --aws-mode flag")
	deleteNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.opensearchCount, "opensearch-count", "o", 0, "No of opensearch instances to be removed. Works with --aws-mode flag")
	deleteNodeHACmd.PersistentFlags().IntVarP(&addDeleteNodeHACmdFlags.postgresqlCount, "postgresql-count", "p", 0, "No of postgresql instances to be removed. Works with --aws-mode flag")
	deleteNodeHACmd.PersistentFlags().BoolVar(&addDeleteNodeHACmdFlags.onPremMode, "onprem-mode", false, "Use this flag if the deployment type is on prem")
	deleteNodeHACmd.PersistentFlags().BoolVar(&addDeleteNodeHACmdFlags.awsMode, "aws-mode", false, "Use this flag if the deployment type is AWS")
	deleteNodeHACmd.PersistentFlags().BoolVarP(&addDeleteNodeHACmdFlags.autoAccept, "auto-accept", "y", false, "auto-accept")

	return deleteNodeHACmd
}

func runDeleteNodeHACmd(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags) func(c *cobra.Command, args []string) error {
	return func(c *cobra.Command, args []string) error {
		nodeDeleter, err := haAddNodeFactory(addDeleteNodeHACmdFlags)
		if err != nil {
			return err
		}
		return nodeDeleter.Execute(c, args)
	}
}

func haDeleteNodeFactory(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags) (HAModifyAndDeploy, error) {
	if addDeleteNodeHACmdFlags.onPremMode {
		return NewDeleteNodeOnPrem(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
	} else if addDeleteNodeHACmdFlags.awsMode {
		return NewDeleteNodeAWS(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
	} else {
		deployerType, err := getModeFromConfig(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"))
		if err != nil {
			return nil, err
		}
		if deployerType == EXISTING_INFRA_MODE {
			return NewDeleteNodeOnPrem(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
		} else if deployerType == AWS_MODE {
			return NewDeleteNodeAWS(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{})), nil
		} else {
			return nil, errors.New(fmt.Sprintf("Unsupported deployment type. Please check %s", filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml")))
		}
	}
}
