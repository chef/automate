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
	"github.com/stretchr/testify/require"
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

func TestExecuteCmdInAllNodeAndCaptureOutput(t *testing.T) {
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

	t.Run("Get node object with to patch command in all nodes", func(t *testing.T) {

		nodeObjects := getNodeObjectsToPatchWorkspaceConfigToAllNodes()
		singleNode := true
		outputDirectory := ""
		err = nodeUtil.executeCmdInAllNodeAndCaptureOutput(nodeObjects, singleNode, outputDirectory, infra)
		assert.Error(t, err, "No ips found")
	})

	t.Run("Get node object with to fetch config in all nodes", func(t *testing.T) {

		nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
		singleNode := true
		outputDirectory := ""
		err = nodeUtil.executeCmdInAllNodeAndCaptureOutput(nodeObjects, singleNode, outputDirectory, infra)
		assert.ErrorContains(t, err, "error on removing output header in fetched config")
	})

}

func TestParseAndMoveConfigFilteToWorkspaceDir(t *testing.T) {
	outDir := "/tmp/"
	fileName, err := fileutils.CreateTempFile("Header\nabc", "file.txt")
	require.NoError(t, err)
	outFile := []string{fileName}
	defer fileutils.DeleteTempFile(fileName)
	t.Run("Move File to workspace directory", func(t *testing.T) {
		err := parseAndMoveConfigFilteToWorkspaceDir(outFile, outDir)
		require.NoError(t, err)
	})
}
