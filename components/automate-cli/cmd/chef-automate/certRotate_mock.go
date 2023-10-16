package main

import (
	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/sshutils"
	"github.com/spf13/cobra"
)

type MockCertRotateFlowImpl struct {
	CertRotateFunc                                   func(cmd *cobra.Command, args []string, flagsObj *certRotateFlags) error
	CertRotateFrontendFunc                           func(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error
	CertRotatePGFunc                                 func(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates, skipFrontend bool) error
	CertRotateFrontendForPGFunc                      func(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, timestamp string) error
	GetSkipIpsListForPgRootCAPatchingFunc            func(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates) ([]string, error)
	CertRotateOSFunc                                 func(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates, skipFrontend bool) error
	CertRotateFrontendForOSFunc                      func(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, nodesCn string, timestamp string) error
	GetSkipIpsListForOsRootCACNPatchingFunc          func(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates, nodesCn string, flagsObj *certRotateFlags) ([]string, error)
	GetFrontIpsToSkipRootCAandCNPatchingForOsFunc    func(automatesConfig map[string]*deployment.AutomateConfig, newRootCA string, newCn string, node string, infra *AutomateHAInfraDetails) []string
	GetFrontendIPsToSkipRootCAPatchingForPgFunc      func(automatesConfig map[string]*deployment.AutomateConfig, newRootCA string, infra *AutomateHAInfraDetails) []string
	PatchConfigFunc                                  func(param *patchFnParameters) error
	CopyAndExecuteFunc                               func(ips []string, sshUtil SSHUtil, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags) error
	CopyAndExecuteConcurrentlyToFrontEndNodesFunc    func(ips []string, sshConfig sshutils.SSHConfig, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags) error
	ValidateEachIpFunc                               func(remoteService string, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) bool
	GetSshDetailsFunc                                func(infra *AutomateHAInfraDetails) *SSHConfig
	GetIpsFunc                                       func(remoteService string, infra *AutomateHAInfraDetails) []string
	IsIPInClusterFunc                                func(ip string, infra *AutomateHAInfraDetails) bool
	GetAllIPsFunc                                    func(infra *AutomateHAInfraDetails) []string
	GetFilteredIpsFunc                               func(serviceIps, skipIpsList []string) []string
	CompareCurrentCertsWithNewCertsFunc              func(remoteService string, newCerts *certificates, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) []string
	ComparePublicCertAndPrivateCertFunc              func(newCerts *certificates, certByIpList []CertByIP, isCertsSame bool, flagsObj *certRotateFlags) []string
	GetFrontEndIpsForSkippingCnAndRootCaPatchingFunc func(newRootCA, newCn, oldCn, oldRootCA, node string) bool
	SkipMessagePrinterFunc                           func(remoteService, skipIpsMsg, nodeFlag string, skipIpsList []string)
	GetCertsFunc                                     func(infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) (*certificates, error)
	IsRemotePathFunc                                 func(path string) bool
	GetIPV4Func                                      func(path string) string
	GetMergerFunc                                    func(fileName string, timestamp string, remoteType string, config string, sshUtil SSHUtil) (string, error)
	CertRotateFromTemplateFunc                       func(clusterCertificateFile string, sshUtil SSHUtil, infra *AutomateHAInfraDetails, currentCertsInfo *certShowCertificates) error
	RotatePGNodeCertsFunc                            func(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, pgRootCA string, pgIps *IP, concurrent bool) error
	RotateOSNodeCertsFunc                            func(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, oss *NodeCertficate, osIp *IP, concurrent bool) error
	RotateAutomateNodeCertsFunc                      func(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, a2Ip *IP) error
	RotateChefServerNodeCertsFunc                    func(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, csIp *IP) error
	RotateClusterFrontendCertificatesFunc            func(infra *AutomateHAInfraDetails, sshUtil SSHUtil, flagsObj certRotateFlags, currentCertsInfo *certShowCertificates, certToml *CertificateToml) error
}

func (mcrf *MockCertRotateFlowImpl) CertRotate(cmd *cobra.Command, args []string, flagsObj *certRotateFlags) error {
	return mcrf.CertRotateFunc(cmd, args, flagsObj)
}
func (mcrf *MockCertRotateFlowImpl) CertRotateFrontend(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	return mcrf.CertRotateFrontendFunc(sshUtil, certs, infra, flagsObj, currentCertsInfo)
}
func (mcrf *MockCertRotateFlowImpl) CertRotatePG(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates, skipFrontend bool) error {
	return mcrf.CertRotatePGFunc(sshUtil, certs, infra, flagsObj, currentCertsInfo, skipFrontend)
}
func (mcrf *MockCertRotateFlowImpl) CertRotateFrontendForPG(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, timestamp string) error {
	return mcrf.CertRotateFrontendForPGFunc(sshUtil, certs, infra, flagsObj, timestamp)
}
func (mcrf *MockCertRotateFlowImpl) GetSkipIpsListForPgRootCAPatching(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates) ([]string, error) {
	return mcrf.GetSkipIpsListForPgRootCAPatchingFunc(infra, sshUtil, certs)
}
func (mcrf *MockCertRotateFlowImpl) CertRotateOS(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates, skipFrontend bool) error {
	return mcrf.CertRotateOSFunc(sshUtil, certs, infra, flagsObj, currentCertsInfo, skipFrontend)
}
func (mcrf *MockCertRotateFlowImpl) CertRotateFrontendForOS(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, nodesCn string, timestamp string) error {
	return mcrf.CertRotateFrontendForOSFunc(sshUtil, certs, infra, flagsObj, nodesCn, timestamp)
}
func (mcrf *MockCertRotateFlowImpl) GetSkipIpsListForOsRootCACNPatching(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates, nodesCn string, flagsObj *certRotateFlags) ([]string, error) {
	return mcrf.GetSkipIpsListForOsRootCACNPatchingFunc(infra, sshUtil, certs, nodesCn, flagsObj)
}
func (mcrf *MockCertRotateFlowImpl) GetFrontIpsToSkipRootCAandCNPatchingForOs(automatesConfig map[string]*deployment.AutomateConfig, newRootCA string, newCn string, node string, infra *AutomateHAInfraDetails) []string {
	return mcrf.GetFrontIpsToSkipRootCAandCNPatchingForOsFunc(automatesConfig, newRootCA, newCn, node, infra)
}
func (mcrf *MockCertRotateFlowImpl) GetFrontendIPsToSkipRootCAPatchingForPg(automatesConfig map[string]*deployment.AutomateConfig, newRootCA string, infra *AutomateHAInfraDetails) []string {
	return mcrf.GetFrontendIPsToSkipRootCAPatchingForPgFunc(automatesConfig, newRootCA, infra)
}
func (mcrf *MockCertRotateFlowImpl) PatchConfig(param *patchFnParameters) error {
	return mcrf.PatchConfigFunc(param)
}
func (mcrf *MockCertRotateFlowImpl) CopyAndExecute(ips []string, sshUtil SSHUtil, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags) error {
	return mcrf.CopyAndExecuteFunc(ips, sshUtil, timestamp, remoteService, fileName, scriptCommands, flagsObj)
}
func (mcrf *MockCertRotateFlowImpl) CopyAndExecuteConcurrentlyToFrontEndNodes(ips []string, sshConfig sshutils.SSHConfig, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags) error {
	return mcrf.CopyAndExecuteConcurrentlyToFrontEndNodesFunc(ips, sshConfig, timestamp, remoteService, fileName, scriptCommands, flagsObj)
}
func (mcrf *MockCertRotateFlowImpl) ValidateEachIp(remoteService string, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) bool {
	return mcrf.ValidateEachIpFunc(remoteService, infra, flagsObj)
}
func (mcrf *MockCertRotateFlowImpl) GetSshDetails(infra *AutomateHAInfraDetails) *SSHConfig {
	return mcrf.GetSshDetailsFunc(infra)
}
func (mcrf *MockCertRotateFlowImpl) GetIps(remoteService string, infra *AutomateHAInfraDetails) []string {
	return mcrf.GetIpsFunc(remoteService, infra)
}
func (mcrf *MockCertRotateFlowImpl) IsIPInCluster(ip string, infra *AutomateHAInfraDetails) bool {
	return mcrf.IsIPInClusterFunc(ip, infra)
}
func (mcrf *MockCertRotateFlowImpl) GetAllIPs(infra *AutomateHAInfraDetails) []string {
	return mcrf.GetAllIPsFunc(infra)
}
func (mcrf *MockCertRotateFlowImpl) GetFilteredIps(serviceIps, skipIpsList []string) []string {
	return mcrf.GetFilteredIpsFunc(serviceIps, skipIpsList)
}
func (mcrf *MockCertRotateFlowImpl) CompareCurrentCertsWithNewCerts(remoteService string, newCerts *certificates, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) []string {
	return mcrf.CompareCurrentCertsWithNewCertsFunc(remoteService, newCerts, flagsObj, currentCertsInfo)
}
func (mcrf *MockCertRotateFlowImpl) ComparePublicCertAndPrivateCert(newCerts *certificates, certByIpList []CertByIP, isCertsSame bool, flagsObj *certRotateFlags) []string {
	return mcrf.ComparePublicCertAndPrivateCertFunc(newCerts, certByIpList, isCertsSame, flagsObj)
}
func (mcrf *MockCertRotateFlowImpl) GetFrontEndIpsForSkippingCnAndRootCaPatching(newRootCA, newCn, oldCn, oldRootCA, node string) bool {
	return mcrf.GetFrontEndIpsForSkippingCnAndRootCaPatchingFunc(newRootCA, newCn, oldCn, oldRootCA, node)
}
func (mcrf *MockCertRotateFlowImpl) SkipMessagePrinter(remoteService, skipIpsMsg, nodeFlag string, skipIpsList []string) {
	mcrf.SkipMessagePrinterFunc(remoteService, skipIpsMsg, nodeFlag, skipIpsList)
}
func (mcrf *MockCertRotateFlowImpl) GetCerts(infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) (*certificates, error) {
	return mcrf.GetCertsFunc(infra, flagsObj)
}
func (mcrf *MockCertRotateFlowImpl) IsRemotePath(path string) bool {
	return mcrf.IsRemotePathFunc(path)
}
func (mcrf *MockCertRotateFlowImpl) GetIPV4(path string) string {
	return mcrf.GetIPV4Func(path)
}
func (mcrf *MockCertRotateFlowImpl) GetMerger(fileName string, timestamp string, remoteType string, config string, sshUtil SSHUtil) (string, error) {
	return mcrf.GetMergerFunc(fileName, timestamp, remoteType, config, sshUtil)
}
func (mcrf *MockCertRotateFlowImpl) CertRotateFromTemplate(clusterCertificateFile string, sshUtil SSHUtil, infra *AutomateHAInfraDetails, currentCertsInfo *certShowCertificates) error {
	return mcrf.CertRotateFromTemplateFunc(clusterCertificateFile, sshUtil, infra, currentCertsInfo)
}
func (mcrf *MockCertRotateFlowImpl) RotatePGNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, pgRootCA string, pgIps *IP, concurrent bool) error {
	return mcrf.RotatePGNodeCertsFunc(infra, sshUtil, currentCertsInfo, pgRootCA, pgIps, concurrent)
}
func (mcrf *MockCertRotateFlowImpl) RotateOSNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, oss *NodeCertficate, osIp *IP, concurrent bool) error {
	return mcrf.RotateOSNodeCertsFunc(infra, sshUtil, currentCertsInfo, oss, osIp, concurrent)
}
func (mcrf *MockCertRotateFlowImpl) RotateAutomateNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, a2Ip *IP) error {
	return mcrf.RotateAutomateNodeCertsFunc(infra, sshUtil, currentCertsInfo, certToml, a2Ip)
}
func (mcrf *MockCertRotateFlowImpl) RotateChefServerNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, csIp *IP) error {
	return mcrf.RotateChefServerNodeCertsFunc(infra, sshUtil, currentCertsInfo, certToml, csIp)
}
func (mcrf *MockCertRotateFlowImpl) RotateClusterFrontendCertificates(infra *AutomateHAInfraDetails, sshUtil SSHUtil, flagsObj certRotateFlags, currentCertsInfo *certShowCertificates, certToml *CertificateToml) error {
	return mcrf.RotateClusterFrontendCertificatesFunc(infra, sshUtil, flagsObj, currentCertsInfo, certToml)
}
