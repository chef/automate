package main

import (
	"container/list"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/toml"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	TAINT_TERRAFORM = `for x in $(terraform state list -state=/hab/a2_deploy_workspace/terraform/terraform.tfstate | grep module); do terraform taint $x; done`

	TAINT_EFS = `
    terraform init;
	for x in $(terraform state list -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate | grep "module.efs\[0\].null_resource.mount_efs");do terraform taint $x; done`

	REMOVE_TERRAFORM_STATE = `S3_STATE=$(terraform state list -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate | grep ".aws_s3_bucket.createS3bucket");
if [ "$S3_STATE" == "module.s3[0].aws_s3_bucket.createS3bucket" ]; then
terraform state rm $S3_STATE
fi`

	AWS_AUTO_TFVARS    = "aws.auto.tfvars"
	DESTROY_AWS_FOLDER = "destroy/aws/"
	TF_ARCH_FILE       = ".tf_arch"
)

type HAModifyAndDeploy interface {
	Execute(c *cobra.Command, args []string) error
	prepare() error
	validate() error
	modifyConfig() error
	promptUserConfirmation() (bool, error)
	runDeploy() error
}

type NodeOpUtils interface {
	executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error
	getHaInfraDetails() (*AutomateHAInfraDetails, *SSHConfig, error)
	writeHAConfigFiles(templateName string, data interface{}, state string) error
	taintTerraform(path string) error
	isA2HARBFileExist() bool
	getModeFromConfig(path string) (string, error)
	checkIfFileExist(path string) bool
	pullAndUpdateConfig(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error)
	pullAndUpdateConfigAws(sshUtil *SSHUtil, exceptionIps []string) (*AwsConfigToml, error)
	isManagedServicesOn() bool
	getConfigPuller(sshUtil *SSHUtil) (PullConfigs, error)
	getInfraConfig(sshUtil *SSHUtil) (*ExistingInfraConfigToml, error)
	getAWSConfig(sshUtil *SSHUtil) (*AwsConfigToml, error)
	getModeOfDeployment() string
	executeShellCommand(command string, path string) error
	moveAWSAutoTfvarsFile(string) error
	modifyTfArchFile(string) error
	getAWSConfigIp() (*AWSConfigIp, error)
	stopServicesOnNode(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error
	excludeOpenSearchNode(ipToDelete string, infra *AutomateHAInfraDetails) error
	checkExistingExcludedOSNodes(automateIp string, infra *AutomateHAInfraDetails) (string, error)
	calculateTotalInstanceCount() (int, error)
	parseAndMoveConfigFileToWorkspaceDir(outFiles []string, outputDirectory string) error
	executeCustomCmdOnEachNodeType(outputFiles []string, inputFiles []string, inputFilesPrefix string, service string, cmdString string, singleNode bool) error
	saveConfigToBastion() error
	syncConfigToAllNodes() error
}

type NodeUtilsImpl struct {
	cmdUtil RemoteCmdExecutor
	exec    command.Executor
	writer  *cli.Writer
}

func NewNodeUtils(cmdUtil RemoteCmdExecutor, exec command.Executor, writer *cli.Writer) NodeOpUtils {
	return &NodeUtilsImpl{
		cmdUtil: cmdUtil,
		exec:    exec,
		writer:  writer,
	}
}

func (nu *NodeUtilsImpl) getAWSConfigIp() (*AWSConfigIp, error) {
	outputDetails, err := getAutomateHAInfraDetails()
	if err != nil {
		return nil, err
	}
	return &AWSConfigIp{
		configAutomateIpList:   outputDetails.Outputs.AutomatePrivateIps.Value,
		configChefServerIpList: outputDetails.Outputs.ChefServerPrivateIps.Value,
		configOpensearchIpList: outputDetails.Outputs.OpensearchPrivateIps.Value,
		configPostgresqlIpList: outputDetails.Outputs.PostgresqlPrivateIps.Value,
	}, nil
}

func (nu *NodeUtilsImpl) pullAndUpdateConfig(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
	configPuller, err := nu.getConfigPuller(sshUtil)
	if err != nil {
		return nil, err
	}
	if len(exceptionIps) > 0 {
		configPuller.setExceptionIps(exceptionIps)
	}
	return configPuller.generateInfraConfig()
}

func (nu *NodeUtilsImpl) getInfraConfig(sshUtil *SSHUtil) (*ExistingInfraConfigToml, error) {
	configPuller, err := nu.getConfigPuller(sshUtil)
	if err != nil {
		return nil, err
	}
	return configPuller.fetchInfraConfig()
}

func (nu *NodeUtilsImpl) getAWSConfig(sshUtil *SSHUtil) (*AwsConfigToml, error) {
	configPuller, err := nu.getConfigPuller(sshUtil)
	if err != nil {
		return nil, err
	}
	return configPuller.fetchAwsConfig()
}

func (nu *NodeUtilsImpl) getConfigPuller(sshUtil *SSHUtil) (PullConfigs, error) {
	infra, cfg, err := nu.getHaInfraDetails()
	if err != nil {
		return nil, err
	}
	(*sshUtil).setSSHConfig(cfg)
	return NewPullConfigs(infra, *sshUtil), nil
}

func (nu *NodeUtilsImpl) pullAndUpdateConfigAws(sshUtil *SSHUtil, exceptionIps []string) (*AwsConfigToml, error) {
	infra, cfg, err := nu.getHaInfraDetails()
	if err != nil {
		return nil, err
	}
	(*sshUtil).setSSHConfig(cfg)
	configPuller := NewPullConfigs(infra, *sshUtil)
	if len(exceptionIps) > 0 {
		configPuller.setExceptionIps(exceptionIps)
	}
	return configPuller.generateAwsConfig()
}

func (nu *NodeUtilsImpl) checkIfFileExist(path string) bool {
	return checkIfFileExist(path)
}

func (nu *NodeUtilsImpl) getModeFromConfig(path string) (string, error) {
	return getModeFromConfig(path)
}

func (nu *NodeUtilsImpl) isA2HARBFileExist() bool {
	return isA2HARBFileExist()
}

func (nu *NodeUtilsImpl) taintTerraform(path string) error {
	modeOfDeployment := getModeOfDeployment()
	if modeOfDeployment == AWS_MODE {
		if err := executeShellCommand("/bin/bash", []string{"-c", REMOVE_TERRAFORM_STATE}, filepath.Join(path, "destroy", "aws")); err != nil {
			return err
		}

		if err := executeShellCommand("/bin/bash", []string{"-c", TAINT_EFS}, filepath.Join(path, "destroy", "aws")); err != nil {
			return err
		}

	}
	return executeShellCommand("/bin/sh", []string{"-c", TAINT_TERRAFORM}, path)
}

func (nu *NodeUtilsImpl) executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	return executeAutomateClusterCtlCommandAsync(command, args, helpDocs, true)
}

func (nu *NodeUtilsImpl) writeHAConfigFiles(templateName string, data interface{}, state string) error {
	return writeHAConfigFiles(templateName, data, state)
}

func (nu *NodeUtilsImpl) moveAWSAutoTfvarsFile(terraformPath string) error {
	AwsAutoTfvarsExist, err := dirExists(filepath.Join(terraformPath, AWS_AUTO_TFVARS))
	if err != nil {
		return err
	}
	if !AwsAutoTfvarsExist {
		return errors.New("Missing " + filepath.Join(terraformPath, AWS_AUTO_TFVARS))
	}
	destroyAwsFolderExist, err := dirExists(filepath.Join(terraformPath, DESTROY_AWS_FOLDER))
	if err != nil {
		return err
	}
	if !destroyAwsFolderExist {
		return errors.New("Missing " + filepath.Join(terraformPath, DESTROY_AWS_FOLDER))
	}
	arg := []string{
		filepath.Join(terraformPath, AWS_AUTO_TFVARS),
		filepath.Join(terraformPath, DESTROY_AWS_FOLDER),
	}
	err = executeCommand("mv", arg, "")
	if err != nil {
		return errors.Wrap(err, "Failed to move aws.auto.tfvars")
	}
	return nil
}

func (nu *NodeUtilsImpl) modifyTfArchFile(terraformPath string) error {
	tfArchPath := filepath.Join(terraformPath, TF_ARCH_FILE)
	data, err := fileutils.ReadFile(tfArchPath)
	if err != nil {
		return errors.Wrap(err, "Failed to read .tf_arch file")
	}

	if string(data) == "aws" {
		return nil
	}

	err = os.Remove(tfArchPath)
	if err != nil {
		return errors.Wrap(err, "Failed to remove .tf_arch file")
	}

	f, err := os.Create(tfArchPath)
	if err != nil {
		return errors.Wrap(err, "Failed to create .tf_arch file")
	}

	_, err = f.WriteString("aws\n")
	if err != nil {
		return errors.Wrap(err, "Failed to write aws in .tf_arch file")
	}
	f.Close()
	return nil
}

func (nu *NodeUtilsImpl) getHaInfraDetails() (*AutomateHAInfraDetails, *SSHConfig, error) {

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return nil, nil, err
	}
	sshconfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return infra, sshconfig, nil
}

func (nu *NodeUtilsImpl) saveConfigToBastion() error {
	nodeObjects := getNodeObjectsToFetchConfigFromAllNodeTypes()
	return executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, true, AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR, nu)
}

func (nu *NodeUtilsImpl) syncConfigToAllNodes() error {
	nodeObjects := getNodeObjectsToPatchWorkspaceConfigToAllNodes()
	return executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects, false, "", nu)
}

// Execute custom command in one node of all the each node-type
func executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects []*NodeObject, singleNode bool, outputDirectory string, nu NodeOpUtils) error {
	for _, nodeObject := range nodeObjects {
		outFiles := nodeObject.OutputFile
		if nodeObject.NodeType == OPENSEARCH || nodeObject.NodeType == POSTGRESQL {
			singleNode = true
		}
		err := nu.executeCustomCmdOnEachNodeType(outFiles, nodeObject.InputFile, nodeObject.InputFilePrefix, nodeObject.NodeType, nodeObject.CmdString, singleNode)
		if err != nil {
			return err
		}
		if len(outFiles) > 0 {
			if err = nu.parseAndMoveConfigFileToWorkspaceDir(outFiles, outputDirectory); err != nil {
				return err
			}
		}
	}
	return nil
}

// Execute 'config show' command in specific service and fetch the output file to bastion
func (nu *NodeUtilsImpl) executeCustomCmdOnEachNodeType(outputFiles []string, inputFiles []string, inputFilesPrefix string, service string, cmdString string, singleNode bool) error {

	infra, _, err := nu.getHaInfraDetails()
	if err != nil {
		return err
	}
	nodeMap := createNodeMap(outputFiles, inputFiles, inputFilesPrefix, service, cmdString, singleNode, infra)

	sshUtil := nu.cmdUtil.GetSshUtil()

	sshUtil.setSSHConfig(&SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	})

	_, err = nu.cmdUtil.ExecuteWithNodeMap(nodeMap)
	return err
}

func (nu *NodeUtilsImpl) isManagedServicesOn() bool {
	return isManagedServicesOn()
}

// GetModeOfDeployment returns the mode of deployment ie. EXISTING_INFRA_MODE or AWS_MODE using terraform.tf_arch file
func (nu *NodeUtilsImpl) getModeOfDeployment() string {
	return getModeOfDeployment()
}
func (nu *NodeUtilsImpl) executeShellCommand(command, path string) error {
	return executeShellCommand("/bin/sh", []string{
		"-c",
		command,
	}, path)
}

// Stop services on node
func (nu *NodeUtilsImpl) stopServicesOnNode(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error {

	if nodeType == OPENSEARCH {
		err := nu.excludeOpenSearchNode(ip, infra)
		if err != nil {
			return err
		}
		nu.writer.Println(fmt.Sprintf("OpenSearch node %s excluded from the cluster.", ip))
	}

	// If deployment type is AWS, then we don't need to stop services on node
	if deploymentType == AWS_MODE {
		return nil
	}

	var cmd string
	switch nodeType {
	case AUTOMATE:
		cmd = STOP_FE_SERVICES_CMD
	case CHEF_SERVER:
		cmd = STOP_FE_SERVICES_CMD
	case POSTGRESQL:
		cmd = STOP_BE_SERVICES_CMD
	case OPENSEARCH:
		cmd = STOP_BE_SERVICES_CMD
	default:
		return errors.New("Invalid node type")
	}

	sshUtil := nu.cmdUtil.GetSshUtil()

	sshUtil.setSSHConfig(&SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		hostIP:     ip,
	})

	_, err := sshUtil.connectAndExecuteCommandOnRemote(cmd, false)
	if err != nil {
		return err
	}

	nu.writer.Println(fmt.Sprintf("Services stopped on the node %s.", ip))

	return nil
}

func (nu *NodeUtilsImpl) excludeOpenSearchNode(ipToDelete string, infra *AutomateHAInfraDetails) error {

	automateIp := infra.Outputs.AutomatePrivateIps.Value[0]

	existingIps, err := nu.checkExistingExcludedOSNodes(automateIp, infra)
	if err != nil {
		return err
	}

	if strings.Contains(existingIps, ipToDelete) {
		return errors.New("Node is already excluded from the cluster")
	}

	if existingIps != "" {
		ipToDelete = existingIps + "," + ipToDelete
	}

	nodeMap := &NodeTypeAndCmd{
		Frontend:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate:   createCmdInputs(automateIp, fmt.Sprintf(EXCLUDE_OPENSEARCH_NODE_REQUEST, ipToDelete)),
		ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      infra,
	}

	_, err = nu.cmdUtil.ExecuteWithNodeMap(nodeMap)
	if err != nil {
		return err
	}
	return nil
}

func (nu *NodeUtilsImpl) checkExistingExcludedOSNodes(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate: &Cmd{CmdInputs: &CmdInputs{
			Cmd:                      GET_OPENSEARCH_CLUSTER_SETTINGS,
			NodeIps:                  []string{automateIp},
			NodeType:                 true,
			SkipPrintOutput:          true,
			HideSSHConnectionMessage: true}},
		ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      infra,
	}

	out, err := nu.cmdUtil.ExecuteWithNodeMap(nodeMap)
	if err != nil {
		return "", err
	}

	var res *CmdResult
	val, ok := out[automateIp]
	if !ok {
		return "", errors.New("Failed to get the response from the node")
	}
	if len(val) > 0 {
		res = val[0]
	}

	if res.Error != nil {
		return "", res.Error
	}

	return getIPsFromOSClusterResponse(res.Output), nil
}

// Calculate total instance count
func (nu *NodeUtilsImpl) calculateTotalInstanceCount() (int, error) {
	cmd := `
        a2csCount=$(terraform state list -state=/hab/a2_deploy_workspace/terraform/terraform.tfstate | grep -E "module.automate|module.bootstrap_automate|module.chef_server" | wc -l)
        pgosCount=$(terraform state list -state=/hab/a2_deploy_workspace/terraform/terraform.tfstate | grep -E "module.opensearch|module.postgresql" | wc -l)
        echo $(($a2csCount/2 + $pgosCount))
    `

	out, err := nu.exec.CombinedOutput("/bin/sh", command.Args("-c", cmd))
	if err != nil {
		return -1, err
	}

	count, err := strconv.Atoi(strings.TrimSuffix(out, "\n"))
	if err != nil {
		return -1, err
	}

	return count, nil
}

func getIPsFromOSClusterResponse(output string) string {
	// Regular expression pattern to match the IP addresses
	pattern := `"_ip":"((?:\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})(?:,\s*\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})*)"`

	// Compile the regular expression pattern
	reg := regexp.MustCompile(pattern)

	// Find all matches of the pattern in the string
	matches := reg.FindAllStringSubmatch(output, -1)

	if len(matches) > 0 {
		// Extract the IP addresses from the first match
		return strings.TrimSpace(matches[0][1])
	}
	return ""
}

func createCmdInputs(ip string, cmd string) *Cmd {
	return &Cmd{
		CmdInputs: &CmdInputs{
			Cmd:                      cmd,
			NodeIps:                  []string{ip},
			NodeType:                 true,
			SkipPrintOutput:          true,
			HideSSHConnectionMessage: true,
		},
	}
}

func trimSliceSpace(slc []string) []string {
	for i := range slc {
		slc[i] = strings.TrimSpace(slc[i])
	}
	return slc
}

func modifyConfigForAddNewNode(instanceCount *string, existingPrivateIPs *[]string, newIps []string, certsIp *[]CertByIP) error {
	if len(newIps) == 0 {
		return nil
	}
	*existingPrivateIPs = append(*existingPrivateIPs, newIps...)
	inc, err := modifyInstanceCount(*instanceCount, len(newIps))
	*instanceCount = inc
	if err != nil {
		return err
	}
	if len(*certsIp) > 0 {
		for _, ip := range newIps {
			c := CertByIP{
				IP:         ip,
				PrivateKey: (*certsIp)[len(*certsIp)-1].PrivateKey,
				PublicKey:  (*certsIp)[len(*certsIp)-1].PublicKey,
				NodesDn:    (*certsIp)[len(*certsIp)-1].NodesDn,
			}
			*certsIp = append(*certsIp, c)
		}
	}
	return nil
}

func modifyConfigForNewNodeCertByIp(instanceCount int, existingPrivateIPs []string, certsIp *[]CertByIP) error {
	var newNode []string
	if len(existingPrivateIPs) >= instanceCount {
		newNode = existingPrivateIPs[len(existingPrivateIPs)-instanceCount : len(existingPrivateIPs)]
	} else {
		newNode = existingPrivateIPs[:]
	}
	if len(*certsIp) > 0 {
		for _, ip := range newNode {
			c := CertByIP{
				IP:         ip,
				PrivateKey: (*certsIp)[len(*certsIp)-1].PrivateKey,
				PublicKey:  (*certsIp)[len(*certsIp)-1].PublicKey,
				NodesDn:    (*certsIp)[len(*certsIp)-1].NodesDn,
			}
			*certsIp = append(*certsIp, c)
		}
	}
	return nil
}

func modifyConfigForDeleteNode(instanceCount *string, existingPrivateIPs *[]string, newIps []string, certsIp *[]CertByIP) error {
	if len(newIps) == 0 {
		return nil
	}
	*existingPrivateIPs = difference(*existingPrivateIPs, newIps)
	inc, err := modifyInstanceCount(*instanceCount, -len(newIps))
	*instanceCount = inc
	if err != nil {
		return err
	}
	if len(*certsIp) > 0 {
		for _, ip := range newIps {
			*certsIp = findAndDelete(*certsIp, ip)
		}
	}
	return nil
}

func modifyConfigForDeleteNodeForAWS(instanceCount *string, newIps []string, certsIp *[]CertByIP) error {
	if len(newIps) == 0 {
		return nil
	}
	inc, err := modifyInstanceCount(*instanceCount, -len(newIps))
	if err != nil {
		return err
	}
	*instanceCount = inc
	if len(*certsIp) > 0 {
		for _, ip := range newIps {
			*certsIp = findAndDelete(*certsIp, ip)
		}
	}
	return nil
}

func difference(a, b []string) []string {
	mb := make(map[string]struct{}, len(b))
	for _, x := range b {
		mb[x] = struct{}{}
	}
	var diff []string
	for _, x := range a {
		if _, found := mb[x]; !found {
			diff = append(diff, x)
		}
	}
	return diff
}

func findAndDelete(s []CertByIP, item string) []CertByIP {
	index := 0
	for _, i := range s {
		if i.IP != item {
			s[index] = i
			index++
		}
	}
	return s[:index]
}

func modifyInstanceCount(instanceCount string, additive int) (string, error) {
	i, err := strconv.Atoi(instanceCount)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%v", i+additive), nil
}

func splitIPCSV(automateIp, chefserverIp, opensearchIp, postgresIp string) (automateIpList, chefServerIpList, opensearchIpList, postgresqlIp []string) {
	if automateIp != "" {
		automateIpList = strings.Split(automateIp, ",")
		automateIpList = trimSliceSpace(automateIpList)
	}
	if chefserverIp != "" {
		chefServerIpList = strings.Split(chefserverIp, ",")
		chefServerIpList = trimSliceSpace(chefServerIpList)
	}
	if opensearchIp != "" {
		opensearchIpList = strings.Split(opensearchIp, ",")
		opensearchIpList = trimSliceSpace(opensearchIpList)
	}
	if postgresIp != "" {
		postgresqlIp = strings.Split(postgresIp, ",")
		postgresqlIp = trimSliceSpace(postgresqlIp)
	}
	return
}

func isFinalInstanceCountAllowed(current string, additive int, minAllowed int) (bool, int, error) {
	i, err := strconv.Atoi(current)
	if err != nil {
		return false, -1, err
	}
	final := i + additive
	if final < minAllowed {
		return false, final, nil
	}
	return true, final, nil
}

func checkIfPresentInPrivateIPList(existingIPArray []string, ips []string, errorPrefix string) *list.List {
	errorList := list.New()
	prefixAdder := ""
	if errorPrefix != "" {
		prefixAdder = " "
	}
	for _, ip := range ips {
		if !stringutils.SliceContains(existingIPArray, ip) {
			errorList.PushBack(fmt.Sprintf("%s%sIp %s is not present in existing list of ip addresses. Please use a different private ip.", errorPrefix, prefixAdder, ip))
		}
	}
	return errorList
}

func readConfig(path string) (ExistingInfraConfigToml, error) {
	templateBytes, err := fileutils.ReadFile(path)
	if err != nil {
		return ExistingInfraConfigToml{}, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := ExistingInfraConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return ExistingInfraConfigToml{}, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return config, nil
}

func readAnyConfig(path string, configType string) (interface{}, error) {
	templateBytes, err := fileutils.ReadFile(path) // nosemgrep
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	var config interface{}
	switch configType {
	case EXISTING_INFRA_MODE:
		config = &ExistingInfraConfigToml{}
	case AWS_MODE:
		config = &AwsConfigToml{}
	default:
		return nil, fmt.Errorf("invalid config type: %s", configType)
	}
	err = ptoml.Unmarshal(templateBytes, config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return config, nil
}

func readConfigAWS(path string) (AwsConfigToml, error) {
	templateBytes, err := fileutils.ReadFile(path) // nosemgrep
	if err != nil {
		return AwsConfigToml{}, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := AwsConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return AwsConfigToml{}, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return config, nil
}

type NodeObject struct {
	CmdString       string
	OutputFile      []string
	InputFile       []string
	InputFilePrefix string
	NodeType        string
}

func NewNodeObjectWithOutputFile(cmdString string, outFile []string, inputFile []string, inputFilePrefix string, nodeType string) *NodeObject {
	return &NodeObject{cmdString, outFile, inputFile, inputFilePrefix, nodeType}
}

func (nu *NodeUtilsImpl) parseAndMoveConfigFileToWorkspaceDir(outFiles []string, outputDirectory string) error {

	err := removeOutputHeaderInConfigFile(outFiles[0])
	if err != nil {
		return errors.Wrap(err, "error on removing output header in fetched config")
	}
	if err = fileutils.Move(outFiles[0], outputDirectory+outFiles[0]); err != nil {
		return err
	}
	return nil
}

func removeOutputHeaderInConfigFile(filePath string) error {
	return fileutils.RemoveFirstLine(filePath)
}

func getNodeObjectsToFetchConfigFromAllNodeTypes() []*NodeObject {
	nodeObjects := []*NodeObject{
		NewNodeObjectWithOutputFile(fmt.Sprintf(GET_FRONTEND_CONFIG, AUTOMATE_TOML), []string{AUTOMATE_TOML}, nil, "", AUTOMATE),
		NewNodeObjectWithOutputFile(fmt.Sprintf(GET_FRONTEND_CONFIG, CHEF_SERVER_TOML), []string{CHEF_SERVER_TOML}, nil, "", CHEF_SERVER),
	}
	if !isManagedServicesOn() {
		backendNodeObjects := []*NodeObject{
			NewNodeObjectWithOutputFile(fmt.Sprintf(GET_BACKEND_CONFIG, POSTGRESQL, " > "+POSTGRESQL_TOML), []string{POSTGRESQL_TOML}, nil, "", POSTGRESQL),
			NewNodeObjectWithOutputFile(fmt.Sprintf(GET_BACKEND_CONFIG, OPENSEARCH, " > "+OPENSEARCH_TOML), []string{OPENSEARCH_TOML}, nil, "", OPENSEARCH),
		}
		nodeObjects = append(nodeObjects, backendNodeObjects...)
	}
	return nodeObjects
}

func getNodeObjectsToPatchWorkspaceConfigToAllNodes() []*NodeObject {
	timestamp := time.Now().Format("20060102150405")
	fmt.Println("====================================================================")
	fmt.Println("sync Config to all nodes")
	frontendPrefix := "frontend" + "_" + timestamp + "_"
	frontend := fmt.Sprintf(FRONTEND_COMMAND, PATCH, frontendPrefix+AUTOMATE_TOML, DATE_FORMAT)
	chefserver := fmt.Sprintf(FRONTEND_COMMAND, PATCH, frontendPrefix+CHEF_SERVER_TOML, DATE_FORMAT)
	backendPrefix := "opensearch" + "_" + timestamp + "_"
	os := fmt.Sprintf(BACKEND_COMMAND, DATE_FORMAT, OPENSEARCH, "%s", backendPrefix+OPENSEARCH_TOML)
	nodeObjects := []*NodeObject{
		NewNodeObjectWithOutputFile(frontend, nil, []string{AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR + AUTOMATE_TOML}, frontendPrefix, AUTOMATE),
		NewNodeObjectWithOutputFile(chefserver, nil, []string{AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR + CHEF_SERVER_TOML}, frontendPrefix, CHEF_SERVER),
	}
	if !isManagedServicesOn() {
		backendNodeObjects := []*NodeObject{
			NewNodeObjectWithOutputFile(os, nil, []string{AUTOMATE_HA_AUTOMATE_NODE_CONFIG_DIR + OPENSEARCH_TOML}, backendPrefix, OPENSEARCH),
		}
		nodeObjects = append(nodeObjects, backendNodeObjects...)
	}
	return nodeObjects
}

func createNodeMap(outputFiles []string, inputFiles []string, inputFilesPrefix string, service string, cmdString string, singleNode bool, infra *AutomateHAInfraDetails) *NodeTypeAndCmd {

	nodeMap := NewNodeTypeAndCmd()
	cmd := newNodeTypeCmd(nodeMap, cmdString, outputFiles, singleNode)
	if service == POSTGRESQL {
		cmd.CmdInputs.Args = inputFiles
	} else if service == OPENSEARCH {
		cmd.CmdInputs.Args = inputFiles
		if len(inputFiles) > 0 {
			cmd.PreExec = prePatchForOsNodes
			cmd.CmdInputs.InputFilesPrefix = inputFilesPrefix
			cmd.CmdInputs.WaitTimeout = 300
		}
	} else {
		cmd.CmdInputs.Args = inputFiles
		// Patch only incase of frontends node
		if len(inputFiles) > 0 {
			cmd.PreExec = prePatchForFrontendNodes
			cmd.CmdInputs.InputFilesPrefix = inputFilesPrefix
			cmd.CmdInputs.WaitTimeout = 300
		}
	}
	switch service {
	case AUTOMATE:
		nodeMap.Automate = cmd
	case CHEF_SERVER:
		nodeMap.ChefServer = cmd
	case POSTGRESQL:
		nodeMap.Postgresql = cmd
	case OPENSEARCH:
		nodeMap.Opensearch = cmd
	}

	nodeMap.Infra = infra

	return nodeMap
}

// Create *Cmd struct instance with 'cmdString' and 'outputFiles' params
func newNodeTypeCmd(nodeMap *NodeTypeAndCmd, cmdString string, outputFiles []string, singleNode bool) *Cmd {
	return &Cmd{
		CmdInputs: &CmdInputs{
			Cmd:                      cmdString,
			Single:                   singleNode,
			InputFiles:               []string{},
			Outputfiles:              outputFiles,
			NodeType:                 true,
			HideSSHConnectionMessage: true,
		},
	}
}

// prePatchForFrontendNodes parse frontend config before patch and remove cert related keys-values
func prePatchForFrontendNodes(inputs *CmdInputs, sshUtil SSHUtil, infra *AutomateHAInfraDetails, remoteService string, writer *cli.Writer) error {
	srcPath, err := removeRestrictedKeysFromSrcFile(inputs.Args[0])
	if err != nil {
		return err
	}

	inputs.InputFiles = []string{srcPath}

	return nil
}

func prePatchForOsNodes(inputs *CmdInputs, sshUtil SSHUtil, infra *AutomateHAInfraDetails, remoteService string, writer *cli.Writer) error {
	srcPath, err := removeRestrictedKeysFromSrcFileOs(inputs.Args[0])
	if err != nil {
		return err
	}

	inputs.InputFiles = []string{srcPath}

	return nil
}

func removeRestrictedKeysFromSrcFileOs(srcString string) (string, error) {
	tomlbyt, _ := fileutils.ReadFile(srcString) // nosemgrep
	destString := string(tomlbyt)
	var dest PatchOpensearchConfig

	if _, err := toml.Decode(destString, &dest); err != nil {
		return "", err
	}

	srcString, err := fileutils.CreateTomlFileFromConfig(dest, srcString)
	if err != nil {
		return "", err
	}
	return srcString, nil
}

// Remove TLS values used for frontend LB and product key in frontend nodes
func removeRestrictedKeysFromSrcFile(srcString string) (string, error) {

	tomlbyt, _ := fileutils.ReadFile(srcString) // nosemgrep
	destString := string(tomlbyt)
	var dest dc.AutomateConfig
	if _, err := toml.Decode(destString, &dest); err != nil {
		return "", err
	}

	// Ignoring cert values for "global.v1.frontend_tls"
	if dest.Global != nil &&
		dest.Global.V1 != nil &&
		len(dest.Global.V1.FrontendTls) != 0 {
		dest.Global.V1.FrontendTls = nil
	}

	// Ignoring cert values for "load_balancer.v1.sys.frontend_tls"
	if dest.LoadBalancer != nil &&
		dest.LoadBalancer.V1 != nil &&
		dest.LoadBalancer.V1.Sys != nil &&
		len(dest.LoadBalancer.V1.Sys.FrontendTls) != 0 {
		dest.LoadBalancer.V1.Sys.FrontendTls = nil
	}

	// Ignoring product key
	if dest.Deployment == nil ||
		dest.Deployment.V1 == nil ||
		dest.Deployment.V1.Svc == nil ||
		dest.Deployment.V1.Svc.Products == nil {
		return srcString, nil
	} else {
		// Following are the unsupported or restricted key to patch via bastion
		dest.Deployment.V1.Svc.Products = nil

		srcString, err := fileutils.CreateTomlFileFromConfig(dest, srcString)
		if err != nil {
			return "", err
		}
		return srcString, nil
	}
}
