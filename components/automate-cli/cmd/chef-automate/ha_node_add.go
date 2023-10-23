package main

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var nodeCmd = &cobra.Command{
	Use:    "node COMMAND",
	Short:  "This command is used to add or delete HA nodes",
	Hidden: false,
	Annotations: map[string]string{
		docs.Compatibility: docs.CompatiblewithHA,
	},
}

func addNodeHACmd() *cobra.Command {
	var addDeleteNodeHACmdFlags = AddDeleteNodeHACmdFlags{}
	var addNodeHACmd = &cobra.Command{
		Use:   "add",
		Short: "Add new node in HA",
		Long:  `Add new node in HA`,
		RunE:  runAddNodeHACmd(&addDeleteNodeHACmdFlags),
		Annotations: map[string]string{
			docs.Compatibility: docs.CompatiblewithHA,
		},
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
		deployerType := getModeOfDeployment()
		nodeAdder, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType)
		if err != nil {
			return err
		}
		// 1. get the version of the automate from the bootstrap_automate
		// 2. get the bundle name from /hab/a2_deploy_workspace/terraform/a2ha_aib_fe.auto.tfvars
		// 3. check bundle existing here /hab/a2_deploy_workspace/terraform/transfer_files/*.aib
		// 1 and 2 should be same then we have to proceed, other wise we have to give instruction how to proceed
		// we can test the procedure in case 1 and 2 are different
		return nodeAdder.Execute(c, args)
	}
}

func haAddNodeFactory(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags, deployerType string) (HAModifyAndDeploy, error) {
	if addDeleteNodeHACmdFlags.onPremMode && addDeleteNodeHACmdFlags.awsMode {
		return nil, errors.New("Cannot use both --onprem-mode and --aws-mode together. Provide only one at a time")
	}
	var hamd HAModifyAndDeploy
	var err error
	switch deployerType {
	case EXISTING_INFRA_MODE:
		if !addDeleteNodeHACmdFlags.awsMode {
			hamd = NewAddNodeOnPrem(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), writer), command.NewExecExecutor(), writer), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{}))
		} else {
			err = fmt.Errorf("Flag given does not match with the current deployment type %s. Try with --onprem-mode flag", deployerType)
		}
	case AWS_MODE:
		if !addDeleteNodeHACmdFlags.onPremMode {
			hamd = NewAddNodeAWS(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), writer), command.NewExecExecutor(), writer), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{}))
		} else {
			err = fmt.Errorf("Flag given does not match with the current deployment type %s. Try with --aws-mode flag", deployerType)
		}
	default:
		err = fmt.Errorf("Unsupported deployment type. Current deployment type is %s", deployerType)
	}
	return hamd, err
}

func init() {
	nodeCmd.AddCommand(addNodeHACmd())
	nodeCmd.AddCommand(deleteNodeHACmd())
	RootCmd.AddCommand(nodeCmd)
}
