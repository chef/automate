package main

type MockNodeUtilsImpl struct {
	executeAutomateClusterCtlCommandAsyncfunc    func(command string, args []string, helpDocs string) error
	getHaInfraDetailsfunc                        func() (*AutomateHAInfraDetails, *SSHConfig, error)
	writeHAConfigFilesFunc                       func(templateName string, data interface{}) error
	taintTerraformFunc                           func(path string) error
	isA2HARBFileExistFunc                        func() bool
	getModeFromConfigFunc                        func(path string) (string, error)
	checkIfFileExistFunc                         func(path string) bool
	pullAndUpdateConfigFunc                      func(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error)
	pullAndUpdateConfigAwsFunc                   func(sshUtil *SSHUtil, exceptionIps []string) (*AwsConfigToml, error)
	isManagedServicesOnFunc                      func() bool
	getConfigPullerFunc                          func(sshUtil *SSHUtil) (PullConfigs, error)
	getInfraConfigFunc                           func(sshUtil *SSHUtil) (*ExistingInfraConfigToml, error)
	getAWSConfigFunc                             func(sshUtil *SSHUtil) (*AwsConfigToml, error)
	getModeOfDeploymentFunc                      func() string
	executeShellCommandFunc                      func() error
	moveAWSAutoTfvarsFileFunc                    func(path string) error
	modifyTfArchFileFunc                         func(path string) error
	getAWSConfigIpFunc                           func() (*AWSConfigIp, error)
	stopServicesOnNodeFunc                       func(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error
	excludeOpenSearchNodeFunc                    func(ipToDelete string, infra *AutomateHAInfraDetails) error
	checkExistingExcludedOSNodesFunc             func(automateIp string, infra *AutomateHAInfraDetails) (string, error)
	calculateTotalInstanceCountFunc              func() (int, error)
	parseAndMoveConfigFileToWorkspaceDirFunc     func(outputFiles []string, outputDirectory string) error
	executeCmdInAllNodeTypesAndCaptureOutputFunc func(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error
	executeCustomCmdOnEachNodeTypeFunc           func(outputFiles []string, inputFiles []string, inputFilesPrefix string, service string, cmdString string, singleNode bool) error
	saveConfigToBastionFunc                      func() error
	syncConfigToAllNodesFunc                     func() error
}

func (mnu *MockNodeUtilsImpl) executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	return mnu.executeAutomateClusterCtlCommandAsyncfunc(command, args, helpDocs)
}
func (mnu *MockNodeUtilsImpl) getAWSConfigIp() (*AWSConfigIp, error) {
	return mnu.getAWSConfigIpFunc()
}
func (mnu *MockNodeUtilsImpl) getHaInfraDetails() (*AutomateHAInfraDetails, *SSHConfig, error) {
	return mnu.getHaInfraDetailsfunc()
}
func (mnu *MockNodeUtilsImpl) writeHAConfigFiles(templateName string, data interface{}) error {
	return mnu.writeHAConfigFilesFunc(templateName, data)
}
func (mnu *MockNodeUtilsImpl) taintTerraform(path string) error {
	return mnu.taintTerraformFunc(path)
}

func (mnu *MockNodeUtilsImpl) isA2HARBFileExist() bool {
	return mnu.isA2HARBFileExistFunc()
}
func (mnu *MockNodeUtilsImpl) getModeFromConfig(path string) (string, error) {
	return mnu.getModeFromConfigFunc(path)
}
func (mnu *MockNodeUtilsImpl) checkIfFileExist(path string) bool {
	return mnu.checkIfFileExistFunc(path)
}
func (mnu *MockNodeUtilsImpl) pullAndUpdateConfig(sshUtil *SSHUtil, exceptionIps []string) (*ExistingInfraConfigToml, error) {
	return mnu.pullAndUpdateConfigFunc(sshUtil, exceptionIps)
}
func (mnu *MockNodeUtilsImpl) isManagedServicesOn() bool {
	return mnu.isManagedServicesOnFunc()
}
func (mnu *MockNodeUtilsImpl) getConfigPuller(sshUtil *SSHUtil) (PullConfigs, error) {
	return mnu.getConfigPullerFunc(sshUtil)
}
func (mnu *MockNodeUtilsImpl) getInfraConfig(sshUtil *SSHUtil) (*ExistingInfraConfigToml, error) {
	return mnu.getInfraConfigFunc(sshUtil)
}
func (mnu *MockNodeUtilsImpl) getAWSConfig(sshUtil *SSHUtil) (*AwsConfigToml, error) {
	return mnu.getAWSConfigFunc(sshUtil)
}
func (mnu *MockNodeUtilsImpl) getModeOfDeployment() string {
	return mnu.getModeOfDeploymentFunc()
}
func (mnu *MockNodeUtilsImpl) executeShellCommand(command, path string) error {
	return mnu.executeShellCommandFunc()
}

func (mnu *MockNodeUtilsImpl) moveAWSAutoTfvarsFile(path string) error {
	return mnu.moveAWSAutoTfvarsFileFunc(path)
}
func (mnu *MockNodeUtilsImpl) modifyTfArchFile(path string) error {
	return mnu.modifyTfArchFileFunc(path)
}
func (mnu *MockNodeUtilsImpl) pullAndUpdateConfigAws(sshUtil *SSHUtil, exceptionIps []string) (*AwsConfigToml, error) {
	return mnu.pullAndUpdateConfigAwsFunc(sshUtil, exceptionIps)
}
func (mnu *MockNodeUtilsImpl) stopServicesOnNode(ip, nodeType, deploymentType string, infra *AutomateHAInfraDetails) error {
	return mnu.stopServicesOnNodeFunc(ip, nodeType, deploymentType, infra)
}
func (mnu *MockNodeUtilsImpl) excludeOpenSearchNode(ipToDelete string, infra *AutomateHAInfraDetails) error {
	return mnu.excludeOpenSearchNodeFunc(ipToDelete, infra)
}
func (mnu *MockNodeUtilsImpl) checkExistingExcludedOSNodes(automateIp string, infra *AutomateHAInfraDetails) (string, error) {
	return mnu.checkExistingExcludedOSNodesFunc(automateIp, infra)
}
func (mnu *MockNodeUtilsImpl) calculateTotalInstanceCount() (int, error) {
	return mnu.calculateTotalInstanceCountFunc()
}

func (mnu *MockNodeUtilsImpl) parseAndMoveConfigFileToWorkspaceDir(outputFiles []string, outputDirectory string) error {
	return mnu.parseAndMoveConfigFileToWorkspaceDirFunc(outputFiles, outputDirectory)
}

func (mnu *MockNodeUtilsImpl) executeCmdInAllNodeTypesAndCaptureOutput(nodeObjects []*NodeObject, singleNode bool, outputDirectory string) error {
	return mnu.executeCmdInAllNodeTypesAndCaptureOutputFunc(nodeObjects, singleNode, outputDirectory)
}

func (mnu *MockNodeUtilsImpl) executeCustomCmdOnEachNodeType(outputFiles []string, inputFiles []string, inputFilesPrefix string, service string, cmdString string, singleNode bool) error {
	return mnu.executeCustomCmdOnEachNodeTypeFunc(outputFiles, inputFiles, inputFilesPrefix, service, cmdString, singleNode)
}

func (mnu *MockNodeUtilsImpl) saveConfigToBastion() error {
	return mnu.saveConfigToBastionFunc()
}

func (mnu *MockNodeUtilsImpl) syncConfigToAllNodes() error {
	return mnu.syncConfigToAllNodesFunc()
}
