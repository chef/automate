package main

import (
	"errors"
	"os"
	"path/filepath"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/chef/automate/lib/platform/command"
	"github.com/stretchr/testify/assert"
)

const (
	TEST_IP_2 = "192.0.2.12"
	TEST_IP_3 = "192.0.2.13"
	TEST_IP_4 = "192.0.2.14"
	TEST_IP_5 = "192.0.2.15"
	TEST_IP_6 = "192.0.2.16"
)

var MockWriter = majorupgrade_utils.NewCustomWriterWithInputs("y")

func TestTrimSliceSpace(t *testing.T) {
	testArr := []string{TEST_IP_1 + " ", " " + TEST_IP_3 + " "}
	resultArr := trimSliceSpace(testArr)
	assert.Equal(t, TEST_IP_1, resultArr[0])
	assert.Equal(t, TEST_IP_3, resultArr[1])
}

func TestModifyConfigForAddNewNode(t *testing.T) {
	incount := "2"
	existingIps := []string{TEST_IP_2, TEST_IP_3}
	newIps := []string{TEST_IP_4}
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	err := modifyConfigForAddNewNode(&incount, &existingIps, newIps, &certs)
	assert.NoError(t, err)
	assert.Equal(t, "3", incount)
	assert.Equal(t, []string{TEST_IP_2, TEST_IP_3, TEST_IP_4}, existingIps)
	assert.Equal(t, CertByIP{
		IP:         TEST_IP_4,
		PrivateKey: "private",
		PublicKey:  "public",
	}, certs[2])
}

func TestModifyConfigForDeleteNode(t *testing.T) {
	incount := "2"
	existingIps := []string{TEST_IP_2, TEST_IP_3}
	newIps := []string{TEST_IP_3}
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	err := modifyConfigForDeleteNode(&incount, &existingIps, newIps, &certs)
	assert.NoError(t, err)
	assert.Equal(t, "1", incount)
	assert.Equal(t, []string{TEST_IP_2}, existingIps)
	assert.Equal(t, 1, len(certs))
}

func TestDifference(t *testing.T) {
	a := []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}
	b := []string{TEST_IP_3}
	strArr := difference(a, b)
	assert.Equal(t, []string{TEST_IP_2, TEST_IP_5}, strArr)
}

func TestDifference1(t *testing.T) {
	a := []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}
	b := []string{TEST_IP_3, TEST_IP_5}
	strArr := difference(a, b)
	assert.Equal(t, []string{TEST_IP_2}, strArr)
}

func TestDifferenceIfNoMatch(t *testing.T) {
	a := []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}
	b := []string{TEST_IP_1, TEST_IP_6}
	strArr := difference(a, b)
	assert.Equal(t, []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}, strArr)
}

func TestFindAndDelete(t *testing.T) {
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	b := TEST_IP_3
	arr := findAndDelete(certs, b)
	assert.Equal(t, []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}, arr)
}

func TestFindAndDeleteIfNoMatch(t *testing.T) {
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_6,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	b := TEST_IP_1
	arr := findAndDelete(certs, b)
	assert.Equal(t, []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_6,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}, arr)
}

func TestModifyInstanceCount(t *testing.T) {
	count, err := modifyInstanceCount("3", 1)
	assert.NoError(t, err)
	assert.Equal(t, "4", count)

	count, err = modifyInstanceCount("3", -2)
	assert.NoError(t, err)
	assert.Equal(t, "1", count)
}

func TestSplitIPCSV(t *testing.T) {
	automateIpList, chefServerIpList, opensearchIpList, postgresqlIp := splitIPCSV(TEST_IP_1+","+TEST_IP_2, TEST_IP_1+","+TEST_IP_2, TEST_IP_1+","+TEST_IP_2, TEST_IP_1+","+TEST_IP_2)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, automateIpList)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, chefServerIpList)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, opensearchIpList)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, postgresqlIp)
}

func TestIsFinalInstanceCountAllowed(t *testing.T) {
	allowed, finalcount, err := isFinalInstanceCountAllowed("3", -1, 3)
	assert.NoError(t, err)
	assert.False(t, allowed)
	assert.Equal(t, 2, finalcount)

	allowed, finalcount, err = isFinalInstanceCountAllowed("5", -2, 3)
	assert.NoError(t, err)
	assert.True(t, allowed)
	assert.Equal(t, 3, finalcount)
}

func TestMoveAWSAutoTfvarsFileAllExist(t *testing.T) {

	nodeUtil := NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), MockWriter.CliWriter), command.NewMockExecutor(t), MockWriter.CliWriter)
	dir := t.TempDir()
	_, err := os.Create(filepath.Join(dir, AWS_AUTO_TFVARS))
	assert.NoError(t, err)
	err = os.MkdirAll(filepath.Join(dir, DESTROY_AWS_FOLDER), os.ModePerm)
	assert.NoError(t, err)

	err = nodeUtil.moveAWSAutoTfvarsFile(dir)
	assert.NoError(t, err)
}

func TestMoveAWSAutoTfvarsFileNotExist(t *testing.T) {
	nodeUtil := NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), MockWriter.CliWriter), command.NewMockExecutor(t), MockWriter.CliWriter)
	dir := t.TempDir()

	err := os.MkdirAll(filepath.Join(dir, DESTROY_AWS_FOLDER), os.ModePerm)
	assert.NoError(t, err)

	err = nodeUtil.moveAWSAutoTfvarsFile(dir)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Missing "+filepath.Join(dir, AWS_AUTO_TFVARS))
}

func TestMoveAWSAutoTfvarsDestroyFolderNotExist(t *testing.T) {
	nodeUtil := NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), MockWriter.CliWriter), command.NewMockExecutor(t), MockWriter.CliWriter)
	dir := t.TempDir()

	_, err := os.Create(filepath.Join(dir, AWS_AUTO_TFVARS))
	assert.NoError(t, err)

	err = nodeUtil.moveAWSAutoTfvarsFile(dir)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Missing "+filepath.Join(dir, DESTROY_AWS_FOLDER))

}

func TestSaveConfigToBastion(t *testing.T) {
	mockUtil := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		executeAutomateClusterCtlCommandAsyncfunc: func(command string, args []string, helpDocs string) error {
			return nil
		},
		writeHAConfigFilesFunc: func(templateName string, data interface{}) error {
			return nil
		},
		isA2HARBFileExistFunc: func() bool {
			return true
		},
		checkIfFileExistFunc: func(path string) bool {
			return checkIfFileExist(path)
		},
		getModeFromConfigFunc: func(path string) (string, error) {
			return EXISTING_INFRA_MODE, nil
		},
		taintTerraformFunc: func(path string) error {
			return nil
		},
		isManagedServicesOnFunc: func() bool {
			return false
		},
		stopServicesOnNodeFunc: func(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		pullAndUpdateConfigFunc: func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
			cfg, err := readConfig(CONFIG_TOML_PATH + "/config.toml")
			if err != nil {
				return nil, err
			}
			cfg.Automate.Config.CertsByIP = []CertByIP{}
			cfg.ChefServer.Config.CertsByIP = []CertByIP{}
			cfg.Postgresql.Config.CertsByIP = []CertByIP{}
			cfg.Opensearch.Config.CertsByIP = []CertByIP{}
			return &cfg, nil
		},

		executeCmdInAllNodeTypesAndCaptureOutputFunc: func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
			return nil
		},
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return nil
		},
		saveConfigToBastionFunc: func() error {
			return nil
		},
		syncConfigToAllNodesFunc: func() error {
			return nil
		},
	}
	err := mockUtil.saveConfigToBastion()
	assert.NoError(t, err)
}

func TestModifyTfArchFile(t *testing.T) {
	nodeUtil := NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), MockWriter.CliWriter), command.NewMockExecutor(t), MockWriter.CliWriter)
	dir := t.TempDir()
	_, err := os.Create(filepath.Join(dir, TF_ARCH_FILE))
	assert.NoError(t, err)

	err = nodeUtil.modifyTfArchFile(dir)
	assert.NoError(t, err)
	data, err := fileutils.ReadFile(filepath.Join(dir, TF_ARCH_FILE))
	assert.NoError(t, err)
	assert.Equal(t, "aws\n", string(data))
}

func TestModifyTfArchFileNotExist(t *testing.T) {
	nodeUtil := NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), MockWriter.CliWriter), command.NewMockExecutor(t), MockWriter.CliWriter)
	dir := t.TempDir()
	err := nodeUtil.modifyTfArchFile(dir)
	assert.Error(t, err)
}

func TestStopServicesOnNodeA2(t *testing.T) {
	mockUtil := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		excludeOpenSearchNodeFunc: func(ipToDelete string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		checkExistingExcludedOSNodesFunc: func(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
			return "", nil
		},
	}

	infra, _, err := mockUtil.getHaInfraDetails()
	assert.NoError(t, err)

	nodeUtil := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
		GetSshUtilFunc: func() SSHUtil {
			return &MockSSHUtilsImpl{
				connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
					return "", nil
				},
			}
		},
	}, command.NewMockExecutor(t), MockWriter.CliWriter)
	err = nodeUtil.stopServicesOnNode(TEST_IP_1, AUTOMATE, EXISTING_INFRA_MODE, infra)
	assert.NoError(t, err)
}

func TestStopServicesOnNodeA2AWS(t *testing.T) {
	mockUtil := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		excludeOpenSearchNodeFunc: func(ipToDelete string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		checkExistingExcludedOSNodesFunc: func(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
			return "", nil
		},
	}

	infra, _, err := mockUtil.getHaInfraDetails()
	assert.NoError(t, err)

	nodeUtil := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
		GetSshUtilFunc: func() SSHUtil {
			return &MockSSHUtilsImpl{
				connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
					return "", nil
				},
			}
		},
	}, command.NewMockExecutor(t), MockWriter.CliWriter)
	err = nodeUtil.stopServicesOnNode(TEST_IP_1, AUTOMATE, AWS_MODE, infra)
	assert.NoError(t, err)
}

func TestStopServicesOnNodeCS(t *testing.T) {
	mockUtil := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		excludeOpenSearchNodeFunc: func(ipToDelete string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		checkExistingExcludedOSNodesFunc: func(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
			return "", nil
		},
	}

	infra, _, err := mockUtil.getHaInfraDetails()
	assert.NoError(t, err)

	nodeUtil := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
		GetSshUtilFunc: func() SSHUtil {
			return &MockSSHUtilsImpl{
				connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
					return "", nil
				},
			}
		},
	}, command.NewMockExecutor(t), MockWriter.CliWriter)
	err = nodeUtil.stopServicesOnNode(TEST_IP_1, CHEF_SERVER, EXISTING_INFRA_MODE, infra)
	assert.NoError(t, err)
}
func TestStopServicesOnNodePG(t *testing.T) {
	mockUtil := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		excludeOpenSearchNodeFunc: func(ipToDelete string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		checkExistingExcludedOSNodesFunc: func(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
			return "", nil
		},
	}

	infra, _, err := mockUtil.getHaInfraDetails()
	assert.NoError(t, err)

	nodeUtil := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
		GetSshUtilFunc: func() SSHUtil {
			return &MockSSHUtilsImpl{
				connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
					return "", nil
				},
			}
		},
	}, command.NewMockExecutor(t), MockWriter.CliWriter)
	err = nodeUtil.stopServicesOnNode(TEST_IP_1, POSTGRESQL, EXISTING_INFRA_MODE, infra)
	assert.NoError(t, err)
}
func TestStopServicesOnNodeOS(t *testing.T) {
	mockUtil := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		excludeOpenSearchNodeFunc: func(ipToDelete string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		checkExistingExcludedOSNodesFunc: func(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
			return "", nil
		},
	}

	infra, _, err := mockUtil.getHaInfraDetails()
	assert.NoError(t, err)

	nodeUtil := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			//return dummy result
			return map[string][]*CmdResult{
				TEST_IP_1: {
					{
						ScriptName:  "",
						HostIP:      "",
						OutputFiles: []string{},
						Output:      "",
						Error:       nil,
					},
				},
			}, nil
		},
		GetSshUtilFunc: func() SSHUtil {
			return &MockSSHUtilsImpl{
				connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
					return "", nil
				},
			}
		},
	}, command.NewMockExecutor(t), MockWriter.CliWriter)
	err = nodeUtil.stopServicesOnNode(TEST_IP_1, OPENSEARCH, EXISTING_INFRA_MODE, infra)
	assert.NoError(t, err)
}

func TestStopServicesOnNodeOSAWS(t *testing.T) {
	mockUtil := &MockNodeUtilsImpl{
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		excludeOpenSearchNodeFunc: func(ipToDelete string, infra *AutomateHAInfraDetails) error {
			return nil
		},
		checkExistingExcludedOSNodesFunc: func(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
			return "", nil
		},
	}

	infra, _, err := mockUtil.getHaInfraDetails()
	assert.NoError(t, err)

	nodeUtil := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			//return dummy result
			return map[string][]*CmdResult{
				TEST_IP_1: {
					{
						ScriptName:  "",
						HostIP:      "",
						OutputFiles: []string{},
						Output:      "",
						Error:       nil,
					},
				},
			}, nil
		}}, command.NewMockExecutor(t), MockWriter.CliWriter)
	err = nodeUtil.stopServicesOnNode(TEST_IP_1, OPENSEARCH, AWS_MODE, infra)
	assert.NoError(t, err)
}

func TestStopServicesOnNodeInvalidNodeType(t *testing.T) {
	nodeUtil := NewNodeUtils(nil, command.NewMockExecutor(t), MockWriter.CliWriter)
	err := nodeUtil.stopServicesOnNode(TEST_IP_1, "invalid", EXISTING_INFRA_MODE, &AutomateHAInfraDetails{})
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Invalid node type")
}

func TestCalculateTotalInstanceCount(t *testing.T) {
	nodeUtil := NewNodeUtils(nil, &command.MockExecutorImpl{
		CombinedOutputFunc: func(cmd string, opts ...command.Opt) (string, error) {
			return "8\n", nil
		},
	}, MockWriter.CliWriter)
	count, err := nodeUtil.calculateTotalInstanceCount()
	assert.NoError(t, err)
	assert.Equal(t, 8, count)
}

func TestCalculateTotalInstanceCountCombineOutputError(t *testing.T) {
	nodeUtil := NewNodeUtils(nil, &command.MockExecutorImpl{
		CombinedOutputFunc: func(cmd string, opts ...command.Opt) (string, error) {
			return "", errors.New("random error")
		},
	}, MockWriter.CliWriter)
	count, err := nodeUtil.calculateTotalInstanceCount()
	assert.ErrorContains(t, err, "error")
	assert.Equal(t, -1, count)
}

func TestCalculateTotalInstanceCountAtoiError(t *testing.T) {
	nodeUtil := NewNodeUtils(nil, &command.MockExecutorImpl{
		CombinedOutputFunc: func(cmd string, opts ...command.Opt) (string, error) {
			return "abc", nil
		},
	}, MockWriter.CliWriter)
	count, err := nodeUtil.calculateTotalInstanceCount()
	assert.ErrorContains(t, err, "invalid syntax")
	assert.Equal(t, -1, count)
}

func TestGetIPsFromOSClusterResponsePersistent(t *testing.T) {
	input := `{"persistent":{"cluster":{"routing":{"allocation":{"exclude":{"_ip":"192.0.2.11"}}}},"plugins":{"index_state_management":{"template_migration":{"control":"-1"}}}},"transient":{}}`
	out := getIPsFromOSClusterResponse(input)
	assert.Equal(t, TEST_IP_1, out)
}

func TestGetIPsFromOSClusterResponsePersistentMultiple(t *testing.T) {
	input := `{"persistent":{"cluster":{"routing":{"allocation":{"exclude":{"_ip":"192.0.2.11,192.0.2.12,192.0.2.13"}}}},"plugins":{"index_state_management":{"template_migration":{"control":"-1"}}}},"transient":{}}`
	out := getIPsFromOSClusterResponse(input)
	assert.Equal(t, "192.0.2.11,192.0.2.12,192.0.2.13", out)
}

func TestGetIPsFromOSClusterResponseTransient(t *testing.T) {
	input := `{"persistent":{"plugins":{"index_state_management":{"template_migration":{"control":"-1"}}}},"transient":{"cluster":{"routing":{"allocation":{"exclude":{"_ip":"192.0.2.11"}}}}}}`
	out := getIPsFromOSClusterResponse(input)
	assert.Equal(t, TEST_IP_1, out)
}

func TestGetIPsFromOSClusterResponseTransientEmpty(t *testing.T) {
	input := `{"persistent":{"plugins":{"index_state_management":{"template_migration":{"control":"-1"}}}},"transient":{"cluster":{"routing":{"allocation":{"exclude":{"_ip":""}}}}}}`
	out := getIPsFromOSClusterResponse(input)
	assert.Equal(t, "", out)
}

func TestGetIPsFromOSClusterResponseNotFound(t *testing.T) {
	input := `{"persistent":{"plugins":{"index_state_management":{"template_migration":{"control":"-1"}}}},"transient":{}}`
	out := getIPsFromOSClusterResponse(input)
	assert.Equal(t, "", out)
}

func TestSyncConfigToAllNodes(t *testing.T) {
	nodeUtil := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
		GetSshUtilFunc: func() SSHUtil {
			return &MockSSHUtilsImpl{
				connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
					return "", nil
				},
				copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
					return nil
				},
			}
		},
	}, command.NewMockExecutor(t), MockWriter.CliWriter)

	t.Run("save config in bastion", func(t *testing.T) {
		err := nodeUtil.saveConfigToBastion()
		assert.Error(t, err, "Automate Ha infra confile file not exist")
	})
	t.Run("sync config in all nodes", func(t *testing.T) {
		err := nodeUtil.syncConfigToAllNodes()
		assert.Error(t, err, "Automate Ha infra confile file not exist")
	})
}

func TestexecuteCmdInAllNodeTypesAndCaptureOutput(t *testing.T) {
	t.Run("save config in bastion", func(t *testing.T) {

		mnu := &MockNodeUtilsImpl{
			parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
				return nil
			},
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
				return nil, nil, nil
			},
			executeCustomCmdOnEachNodeTypeFunc: func(outputFiles, inputFiles []string, inputFilesPrefix, service, cmdString string, singleNode bool) error {
				return nil
			},
		}
		nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
		singleNode := true
		outputDirectory := ""

		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.NoError(t, err)
	})

	t.Run("save config in bastion with error in parsing", func(t *testing.T) {

		mnu := &MockNodeUtilsImpl{
			parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
				return errors.New("error parsing output file")
			},
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
				return nil, nil, nil
			},
			executeCustomCmdOnEachNodeTypeFunc: func(outputFiles, inputFiles []string, inputFilesPrefix, service, cmdString string, singleNode bool) error {
				return nil
			},
		}
		nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
		singleNode := true
		outputDirectory := ""

		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.Error(t, err, "error parsing output file")
	})

	t.Run("Get node object with to patch command in all nodes", func(t *testing.T) {
		mnu := NewNodeUtils(&MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				return nil, nil
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
					copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
						return nil
					},
				}
			},
		}, command.NewMockExecutor(t), MockWriter.CliWriter)

		nodeObjects := getNodeObjectsToPatchWorkspaceConfigToAllNodes()
		singleNode := true
		outputDirectory := ""
		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.Error(t, err, "No ips found")
	})

	t.Run("Get node object with to patch command for PG node", func(t *testing.T) {
		mnu := NewNodeUtils(&MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				return nil, nil
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
					copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
						return nil
					},
				}
			},
		}, command.NewMockExecutor(t), MockWriter.CliWriter)

		nodeObjects := []*NodeObject{
			NewNodeObjectWithOutputFile("", nil, nil, "", POSTGRESQL),
		}
		singleNode := true
		outputDirectory := ""
		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.Error(t, err, "No ips found")
	})

	t.Run("Get node object with to patch command for OS node", func(t *testing.T) {
		mnu := NewNodeUtils(&MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				return nil, nil
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
					copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
						return nil
					},
				}
			},
		}, command.NewMockExecutor(t), MockWriter.CliWriter)

		nodeObjects := []*NodeObject{
			NewNodeObjectWithOutputFile("", nil, nil, "", OPENSEARCH),
		}
		singleNode := true
		outputDirectory := ""
		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.Error(t, err, "No ips found")
	})

	t.Run("Get node object for fetch command in all nodes", func(t *testing.T) {
		mnu := NewNodeUtils(&MockRemoteCmdExecutor{
			ExecuteFunc: func() (map[string][]*CmdResult, error) {
				return nil, nil
			},
			ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
				return nil, nil
			},
			GetSshUtilFunc: func() SSHUtil {
				return &MockSSHUtilsImpl{
					connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
						return "", nil
					},
					copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
						return nil
					},
				}
			},
		}, command.NewMockExecutor(t), MockWriter.CliWriter)

		nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
		singleNode := true
		outputDirectory := ""
		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.Error(t, err, "No ips found")
	})
}

func TestexecuteCmdInAllNodeTypesAndCaptureOutputToSaveConfigInBastionBeforeNodeModify(t *testing.T) {

	t.Run("save config in bastion", func(t *testing.T) {

		mnu := &MockNodeUtilsImpl{
			parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
				return nil
			},
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
				return nil, nil, nil
			},
			executeCustomCmdOnEachNodeTypeFunc: func(outputFiles, inputFiles []string, inputFilesPrefix, service, cmdString string, singleNode bool) error {
				return nil
			},
		}
		nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
		singleNode := true
		outputDirectory := ""

		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.NoError(t, err)
	})

	t.Run("save config in bastion with error in parsing", func(t *testing.T) {

		mnu := &MockNodeUtilsImpl{
			parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
				return errors.New("error parsing output file")
			},
			getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
				return nil, nil, nil
			},
			executeCustomCmdOnEachNodeTypeFunc: func(outputFiles, inputFiles []string, inputFilesPrefix, service, cmdString string, singleNode bool) error {
				return nil
			},
		}
		nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
		singleNode := true
		outputDirectory := ""

		err := executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, singleNode, outputDirectory, mnu)
		assert.Error(t, err, "error parsing output file")
	})
}

func TestCreateNodeMap(t *testing.T) {
	mnu := &MockNodeUtilsImpl{
		parseAndMoveConfigFileToWorkspaceDirFunc: func(outputFiles []string, outputDirectory string) error {
			return errors.New("error parsing output file")
		},
		getHaInfraDetailsfunc: func() (*AutomateHAInfraDetails, *SSHConfig, error) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = []string{TEST_IP_1}
			return infra, &SSHConfig{}, nil
		},
		executeCustomCmdOnEachNodeTypeFunc: func(outputFiles, inputFiles []string, inputFilesPrefix, service, cmdString string, singleNode bool) error {
			return nil
		},
	}

	infra, _, err := mnu.getHaInfraDetails()
	assert.NoError(t, err)

	outputFiles := []string{"outputFiles.toml"}
	inputFiles := []string{"inputFiles.toml"}
	inputFilesPrefix := "/tmp/"

	cmdString := "sudo chef-automate config patch"
	singleNode := true

	t.Run(AUTOMATE, func(t *testing.T) {
		service := AUTOMATE
		nodeMap := createNodeMap(outputFiles, inputFiles, inputFilesPrefix, service, cmdString, singleNode, infra)
		assert.NotNil(t, nodeMap, "automate")
	})
	t.Run(CHEF_SERVER, func(t *testing.T) {
		service := CHEF_SERVER
		nodeMap := createNodeMap(outputFiles, inputFiles, inputFilesPrefix, service, cmdString, singleNode, infra)
		assert.NotNil(t, nodeMap, "automate")
	})
	t.Run(POSTGRESQL, func(t *testing.T) {
		service := POSTGRESQL
		nodeMap := createNodeMap(outputFiles, inputFiles, inputFilesPrefix, service, cmdString, singleNode, infra)
		assert.NotNil(t, nodeMap, "automate")
	})
	t.Run(OPENSEARCH, func(t *testing.T) {
		service := OPENSEARCH
		nodeMap := createNodeMap(outputFiles, inputFiles, inputFilesPrefix, service, cmdString, singleNode, infra)
		assert.NotNil(t, nodeMap, "automate")
	})
}

func TestPrePatchForFrontendNodes(t *testing.T) {

	tomlFileContent := `
	[deployment]
  [deployment.v1]
    [deployment.v1.svc]
      channel = "current"
      upgrade_strategy = "none"
      deployment_type = "local"
      products = ["automate", "chef-server"]
			[[global.v1.frontend_tls]]
      cert=""
			[[load_balancer.v1.sys.frontend_tls]]
			cert = ""`

	filePath, err := fileutils.CreateTempFile(tomlFileContent, AUTOMATE_TOML)
	assert.NoError(t, err)
	defer fileutils.DeleteFile(filePath)

	t.Run("with empty toml", func(t *testing.T) {
		cmpInput := &CmdInputs{
			InputFiles: []string{AUTOMATE_TOML},
			Args:       []string{""},
		}
		err := prePatchForFrontendNodes(cmpInput, NewSSHUtil(&SSHConfig{}), nil, "", nil)
		assert.NoError(t, err)
	})
	t.Run("with toml file content", func(t *testing.T) {
		cmpInput := &CmdInputs{
			InputFiles: []string{AUTOMATE_TOML},
			Args:       []string{filePath},
		}
		err := prePatchForFrontendNodes(cmpInput, NewSSHUtil(&SSHConfig{}), nil, "", nil)
		assert.NoError(t, err)
	})
	t.Run("with invalid toml file content", func(t *testing.T) {
		tomlFileContent := `
	[deploy`
		filePath, err := fileutils.CreateTempFile(tomlFileContent, AUTOMATE_TOML)
		assert.NoError(t, err)
		cmpInput := &CmdInputs{
			InputFiles: []string{AUTOMATE_TOML},
			Args:       []string{filePath},
		}
		err = prePatchForFrontendNodes(cmpInput, NewSSHUtil(&SSHConfig{}), nil, "", nil)
		assert.Error(t, err, "expected '.' or ']'")
	})
}

func TestParseAndMoveConfigFileToWorkspaceDir(t *testing.T) {
	mnu := NewNodeUtils(&MockRemoteCmdExecutor{
		ExecuteFunc: func() (map[string][]*CmdResult, error) {
			return nil, nil
		},
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
		GetSshUtilFunc: func() SSHUtil {
			return &MockSSHUtilsImpl{
				connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
					return "", nil
				},
				copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
					return nil
				},
			}
		},
	}, command.NewMockExecutor(t), MockWriter.CliWriter)

	tomlFileContent := `Output of IP a.b.c.d:
[deployment]
  [deployment.v1]
    [deployment.v1.svc]
      channel = "current"
      upgrade_strategy = "none"
      deployment_type = "local"
      products = ["automate", "chef-server"]
			[[global.v1.frontend_tls]]
      cert=""
			[[load_balancer.v1.sys.frontend_tls]]
			cert = ""
`
	finalTomlFileContent := `[deployment]
  [deployment.v1]
    [deployment.v1.svc]
      channel = "current"
      upgrade_strategy = "none"
      deployment_type = "local"
      products = ["automate", "chef-server"]
			[[global.v1.frontend_tls]]
      cert=""
			[[load_balancer.v1.sys.frontend_tls]]
			cert = ""
`

	filePath, err := fileutils.CreateTempFile(tomlFileContent, AUTOMATE_TOML)
	assert.NoError(t, err)
	defer fileutils.DeleteFile(filePath)

	t.Run("No error", func(t *testing.T) {
		err = mnu.parseAndMoveConfigFileToWorkspaceDir([]string{filePath}, "")
		assert.NoError(t, err)
		contentByte, err := fileutils.ReadFile(filePath)
		assert.NoError(t, err)
		assert.Equal(t, finalTomlFileContent, string(contentByte))
	})
	t.Run("error on removing output header", func(t *testing.T) {
		err = mnu.parseAndMoveConfigFileToWorkspaceDir([]string{AUTOMATE_TOML}, "")
		assert.Error(t, err, "error on removing output header in fetched config")
	})
}
