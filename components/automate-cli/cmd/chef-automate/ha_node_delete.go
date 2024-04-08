package main

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
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
		Annotations: map[string]string{
			docs.Compatibility: docs.CompatiblewithHA,
		},
	}
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.automateIp, "automate-ip", "A", "", "Automate ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.chefServerIp, "chef-server-ip", "C", "", "Chef-server ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.opensearchIp, "opensearch-ip", "O", "", "OpenSearch ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().StringVarP(&addDeleteNodeHACmdFlags.postgresqlIp, "postgresql-ip", "P", "", "Postgresql ip addresses to be removed. Works with --onprem-mode flag")
	deleteNodeHACmd.PersistentFlags().BoolVar(&addDeleteNodeHACmdFlags.onPremMode, "onprem-mode", false, "Use this flag if the deployment type is on prem")
	deleteNodeHACmd.PersistentFlags().BoolVar(&addDeleteNodeHACmdFlags.awsMode, "aws-mode", false, "Use this flag if the deployment type is AWS")
	deleteNodeHACmd.PersistentFlags().BoolVarP(&addDeleteNodeHACmdFlags.autoAccept, "auto-accept", "y", false, "auto-accept")
	deleteNodeHACmd.PersistentFlags().BoolVarP(&addDeleteNodeHACmdFlags.removeUnreachableNode, "remove-unreachable-node", "r", false, "remove unreachable nodes from cluster")

	return deleteNodeHACmd
}

func runDeleteNodeHACmd(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags) func(c *cobra.Command, args []string) error {
	return func(c *cobra.Command, args []string) error {
		deployerType := getModeOfDeployment()
		nodeDeleter, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType)
		if err != nil {
			return err
		}
		return nodeDeleter.Execute(c, args)
	}
}

func haDeleteNodeFactory(addDeleteNodeHACmdFlags *AddDeleteNodeHACmdFlags, deployerType string) (HAModifyAndDeploy, error) {
	if addDeleteNodeHACmdFlags.onPremMode && addDeleteNodeHACmdFlags.awsMode {
		return nil, errors.New("Cannot use both --onprem-mode and --aws-mode together. Provide only one at a time")
	}
	var hamd HAModifyAndDeploy
	var err error
	switch deployerType {
	case EXISTING_INFRA_MODE:
		if !addDeleteNodeHACmdFlags.awsMode {
			hamd = NewDeleteNodeOnPrem(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), writer), command.NewExecExecutor(), writer), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{}))
		} else {
			err = fmt.Errorf("flag given does not match with the current deployment type %s. Try with --onprem-mode flag", deployerType)
		}
	case AWS_MODE:
		if !addDeleteNodeHACmdFlags.onPremMode {
			hamd = NewDeleteNodeAWS(writer, *addDeleteNodeHACmdFlags, NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), writer), command.NewExecExecutor(), writer), initConfigHabA2HAPathFlag.a2haDirPath, &fileutils.FileSystemUtils{}, NewSSHUtil(&SSHConfig{}))
		} else {
			err = fmt.Errorf("flag given does not match with the current deployment type %s. Try with --aws-mode flag", deployerType)
		}
	default:
		err = fmt.Errorf("unsupported deployment type. Current deployment type is %s", deployerType)
	}
	return hamd, err
}
