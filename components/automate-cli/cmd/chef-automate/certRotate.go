package main

import (
	"crypto/x509/pkix"
	"encoding/pem"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	FRONTEND_CONFIG = `
	[[load_balancer.v1.sys.frontend_tls]]
		cert = """%v"""
		key = """%v"""
	[[global.v1.frontend_tls]]
		cert = """%v"""
		key = """%v"""`

	POSTGRES_CONFIG = `
	[ssl]
		enable = true
		ssl_key = """%v"""
		ssl_cert = """%v"""
		issuer_cert = """%v"""`

	POSTGRES_CONFIG_IGNORE_ISSUER_CERT = `
	[ssl]
		enable = true
		ssl_key = """%v"""
		ssl_cert = """%v"""`

	POSTGRES_FRONTEND_CONFIG = `
	[global.v1.external.postgresql.ssl]
		enable = true
		root_cert = """%v"""`

	OPENSEARCH_CONFIG = `
	[tls]
		rootCA = """%v"""
		admin_cert = """%v"""
		admin_key = """%v"""
		ssl_cert = """%v"""
		ssl_key = """%v"""
	[plugins.security.authcz]
		admin_dn = '- %v'
	[plugins.security.ssl.transport]
		enforce_hostname_verification = false
		resolve_hostname = false
	[plugins.security]
		nodes_dn = '- %v'`

	OPENSEARCH_CONFIG_IGNORE_ADMIN_AND_ROOTCA = `
	[tls]
		ssl_cert = """%v"""
		ssl_key = """%v"""
	[plugins.security]
		nodes_dn = """- %v"""`

	OPENSEARCH_FRONTEND_CONFIG = `
	[global.v1.external.opensearch.ssl]
		root_cert = """%v"""
		server_name = "%v"`

	OPENSEARCH_FRONTEND_CONFIG_IGNORE_ROOT_CERT = `
	[global.v1.external.opensearch.ssl]
		server_name = "%v"`

	OPENSEARCH_DN_CONFIG_FOR_PEERS = `
	[plugins]
	[plugins.security]
		nodes_dn = """- %v"""`

	GET_USER_CONFIG = `
	sudo cat /hab/user/automate-ha-%s/config/user.toml`

	COPY_USER_CONFIG = `
	sudo systemctl stop hab-sup.service
	echo "y" | sudo cp /tmp/%s /hab/user/automate-ha-%s/config/user.toml
	sudo systemctl start hab-sup.service`

	IP_V4_REGEX = `(\b25[0-5]|\b2[0-4][0-9]|\b[01]?[0-9][0-9]?)(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}`

	ERROR_SELF_MANAGED_DB_CERT_ROTATE = "Certificate rotation for externally configured %s is not supported."
	SKIP_IPS_MSG_CERT_ROTATE          = "The following %s %s will skip during certificate rotation as the following %s have the same certificates as currently provided certificates.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_A2         = "The following %s %s will skip during root-ca patching as the following %s have same root-ca as currently provided Automate root-ca.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_PG         = "The following %s %s will skip during root-ca patching as the following %s have same root-ca as currently provided Postgresql root-ca.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_OS         = "The following %s %s will skip during root-ca and common name patching as the following %s have same root-ca and common name as currently provided OpenSearch root-ca and common name.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_CN         = "The following %s %s will skip during common name patching as the following %s have same common name as currently provided OpenSearch common name.\n\t %s"
	DEFAULT_TIMEOUT                   = 180
)

type certificates struct {
	privateCert string
	publicCert  string
	rootCA      string
	adminCert   string
	adminKey    string
}

type certRotateFlags struct {
	//SSH
	automate   bool
	chefserver bool
	postgres   bool
	opensearch bool

	// Certificates Path
	privateCertPath string
	publicCertPath  string
	rootCAPath      string
	adminCertPath   string
	adminKeyPath    string

	// Node
	node    string
	timeout int
}

type certRotateFlow struct {
	FileUtils fileutils.FileUtils
}

type patchFnParameters struct {
	sshUtil       SSHUtil
	config        string
	fileName      string
	timestamp     string
	remoteService string
	infra         *AutomateHAInfraDetails
	flagsObj      *certRotateFlags
	skipIpsList   []string
}

type CertRoateCmdResult struct {
	hostIp       string
	outputString string
	err          error
	writer       *cli.Writer
	nodeType     string
}

func init() {
	flagsObj := certRotateFlags{}

	certRotateCmd := &cobra.Command{
		Use:   "cert-rotate",
		Short: "Chef Automate rotate cert",
		Long:  "Chef Automate CLI command to rotate certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certRotateCmdFunc(&flagsObj),
		Annotations: map[string]string{
			docs.Compatibility: docs.CompatiblewithHA,
		},
	}

	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.automate, AUTOMATE, "a", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.automate, "a2", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.chefserver, CHEF_SERVER, "c", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.chefserver, "cs", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.postgres, POSTGRESQL, "p", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.postgres, "pg", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.opensearch, OPENSEARCH, "o", false, "OS Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.opensearch, "os", false, "OS Certificate Rotation")

	certRotateCmd.PersistentFlags().StringVar(&flagsObj.privateCertPath, "private-cert", "", "Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.publicCertPath, "public-cert", "", "Public certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.rootCAPath, "root-ca", "", "RootCA certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.adminCertPath, "admin-cert", "", "Admin certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.adminKeyPath, "admin-key", "", "Admin Private certificate")

	certRotateCmd.PersistentFlags().StringVar(&flagsObj.node, "node", "", "Node Ip address")
	certRotateCmd.PersistentFlags().IntVar(&flagsObj.timeout, "wait-timeout", DEFAULT_TIMEOUT, "This flag sets the operation timeout duration (in seconds) for each individual node during the certificate rotation process")

	RootCmd.AddCommand(certRotateCmd)
}

func certRotateCmdFunc(flagsObj *certRotateFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		c := certRotateFlow{FileUtils: &fileutils.FileSystemUtils{}}
		return c.certRotate(cmd, args, flagsObj)
	}
}

// certRotate will rotate the certificates of Automate, Chef Infra Server, Postgres and Opensearch.
func (c *certRotateFlow) certRotate(cmd *cobra.Command, args []string, flagsObj *certRotateFlags) error {
	if isA2HARBFileExist() {

		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		certs, err := c.getCerts(infra, flagsObj)
		if err != nil {
			return err
		}

		// we need to ignore root-ca, adminCert and adminKey in the case of each node
		if certs.rootCA != "" && flagsObj.node != "" {
			writer.Warn("root-ca flag will be ignored when node flag is provided")
		}
		if (certs.adminCert != "" || certs.adminKey != "") && flagsObj.node != "" {
			writer.Warn("admin-cert and admin-key flag will be ignored when node flag is provided")
		}

		sshConfig := c.getSshDetails(infra)
		sshUtil := NewSSHUtil(sshConfig)
		certShowFlow := NewCertShowImpl(certShowFlags{}, NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), writer), command.NewExecExecutor(), writer), sshUtil, writer)
		currentCertsInfo, err := certShowFlow.fetchCurrentCerts()

		if err != nil {
			return errors.Wrap(err, "Error occured while fetching current certs")
		}

		if flagsObj.timeout < DEFAULT_TIMEOUT {
			return errors.Errorf("The operation timeout duration for each individual node during the certificate rotation process should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT)
		}

		sshConfig.timeout = flagsObj.timeout
		sshUtil.setSSHConfig(sshConfig)

		if flagsObj.automate || flagsObj.chefserver {
			err := c.certRotateFrontend(sshUtil, certs, infra, flagsObj, currentCertsInfo)
			if err != nil {
				return err
			}
		} else if flagsObj.postgres {
			err := c.certRotatePG(sshUtil, certs, infra, flagsObj, currentCertsInfo)
			if err != nil {
				return err
			}
		} else if flagsObj.opensearch {
			err := c.certRotateOS(sshUtil, certs, infra, flagsObj, currentCertsInfo)
			if err != nil {
				return err
			}
		} else {
			return errors.New("Please Provide service flag")
		}
	} else {
		return fmt.Errorf("cert-rotate command should be executed from Automate HA Bastion Node")
	}

	return nil
}

// certRotateFrontend will rotate the certificates of Automate and Chef Infra Server.
func (c *certRotateFlow) certRotateFrontend(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	fileName := "cert-rotate-fe.toml"
	timestamp := time.Now().Format("20060102150405")
	var remoteService string

	if flagsObj.automate {
		remoteService = AUTOMATE
	} else if flagsObj.chefserver {
		remoteService = CHEF_SERVER
	}
	//get ips to exclude
	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.
	config := fmt.Sprintf(FRONTEND_CONFIG, certs.publicCert, certs.privateCert, certs.publicCert, certs.privateCert)

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	err := c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}

	return nil
}

// certRotatePG will rotate the certificates of Postgres.
func (c *certRotateFlow) certRotatePG(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, POSTGRESQL)
	}
	fileName := "cert-rotate-pg.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := POSTGRESQL

	// Creating and patching the required configurations.
	var config string
	if flagsObj.node != "" {
		config = fmt.Sprintf(POSTGRES_CONFIG_IGNORE_ISSUER_CERT, certs.privateCert, certs.publicCert)
	} else {
		config = fmt.Sprintf(POSTGRES_CONFIG, certs.privateCert, certs.publicCert, certs.rootCA)
	}

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	//patching on PG
	err := c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}

	// ignore patching of root-ca when node flag is provided
	if flagsObj.node != "" {
		return nil
	}
	//get frontend ips to skip root-ca patch
	skipIpsList = c.getFrontEndIpsForSkippingRootCAPatching(remoteService, certs.rootCA, infra, currentCertsInfo)
	c.skipMessagePrinter("frontend", SKIP_FRONT_END_IPS_MSG_PG, "", skipIpsList)

	// Patching root-ca to frontend-nodes for maintaining the connection.
	filenameFe := "pg_fe.toml"
	remoteService = "frontend"
	// Creating and patching the required configurations.
	configFe := fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, certs.rootCA)

	patchFnParam.config = configFe
	patchFnParam.fileName = filenameFe
	patchFnParam.remoteService = remoteService
	patchFnParam.skipIpsList = skipIpsList

	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}
	return nil
}

// certRotateOS will rotate the certificates of OpenSearch.
func (c *certRotateFlow) certRotateOS(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, OPENSEARCH)
	}
	fileName := "cert-rotate-os.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := OPENSEARCH

	var adminDn pkix.Name
	var err error
	if flagsObj.node == "" {
		adminDn, err = getDistinguishedNameFromKey(certs.adminCert)
		if err != nil {
			return err
		}
	}
	nodeDn, err := getDistinguishedNameFromKey(certs.publicCert)
	if err != nil {
		return err
	}

	existingNodesDN := currentCertsInfo.OpensearchCertsByIP[0].NodesDn
	nodesDn := ""
	nodesCn := ""

	if flagsObj.node != "" {
		nodesDn = nodesDn + fmt.Sprintf("%v", existingNodesDN) + "\n  - " + fmt.Sprintf("%v", nodeDn) + "\n"
	} else {
		nodesDn = fmt.Sprintf("%v", nodeDn) + nodesDn
		nodesCn = nodeDn.CommonName
	}

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.
	var config string
	if flagsObj.node != "" {
		config = fmt.Sprintf(OPENSEARCH_CONFIG_IGNORE_ADMIN_AND_ROOTCA, certs.publicCert, certs.privateCert, fmt.Sprintf("%v", nodesDn))
	} else {
		config = fmt.Sprintf(OPENSEARCH_CONFIG, certs.rootCA, certs.adminCert, certs.adminKey, certs.publicCert, certs.privateCert, fmt.Sprintf("%v", adminDn), fmt.Sprintf("%v", nodesDn))
	}

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}

	if flagsObj.node != "" && stringutils.SliceContains(skipIpsList, flagsObj.node) {
		return nil
	}

	if flagsObj.node != "" {

		err := patchOSNodeDN(flagsObj, patchFnParam, c, nodesDn)
		if err != nil {
			return err
		}

	}

	//fetching current config from automate
	pullAutomateCurrentConfig := NewPullConfigs(infra, sshUtil)
	automateCurrentConfig, err := pullAutomateCurrentConfig.pullAutomateConfigs()

	if err != nil {
		return err
	}

	// Patching root-ca to frontend-nodes for maintaining the connection.
	cn := nodesCn
	filenameFe := "os_fe.toml"
	remoteService = "frontend"

	skipIpsList = c.getFrontIpsToSkipRootCAandCNPatching(automateCurrentConfig, certs.rootCA, cn, flagsObj.node, infra)
	fmt.Println("new rootca : ",certs.rootCA)	
	fmt.Println("new CN",cn)	

	//fetching current config from chefServer
	pullChefServerCurrentConfig := NewPullConfigs(infra, sshUtil)
	chefServerCurrentConfig, err := pullChefServerCurrentConfig.pullChefServerConfigs()

	if err != nil {
		return err
	}
	newskipIpsList := c.getFrontIpsToSkipRootCAandCNPatching(chefServerCurrentConfig, certs.rootCA, cn, flagsObj.node, infra)
	skipIpsList = append(skipIpsList, newskipIpsList...)
	skipMessage := ""

	// Creating and patching the required configurations.
	var configFe string
	if flagsObj.node != "" {
		configFe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG_IGNORE_ROOT_CERT, cn)
		skipMessage = SKIP_FRONT_END_IPS_MSG_CN
	} else {
		configFe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, certs.rootCA, cn)
		skipMessage = SKIP_FRONT_END_IPS_MSG_OS
	}
	c.skipMessagePrinter(remoteService, skipMessage, "", skipIpsList)

	patchFnParam.config = configFe
	patchFnParam.fileName = filenameFe
	patchFnParam.remoteService = remoteService
	patchFnParam.skipIpsList = skipIpsList

	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}
	return nil
}

func patchOSNodeDN(flagsObj *certRotateFlags, patchFnParam *patchFnParameters, c *certRotateFlow, nodesDn string) error {

	peerConfig := fmt.Sprintf(OPENSEARCH_DN_CONFIG_FOR_PEERS, fmt.Sprintf("%v", nodesDn))

	nodeVal := flagsObj.node
	flagsObj.node = ""

	patchFnParam.fileName = "cert-rotate-os-peer.toml"
	patchFnParam.config = peerConfig
	patchFnParam.timestamp = time.Now().Format("20060102150405")
	patchFnParam.skipIpsList = []string{flagsObj.node}

	err := c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}

	flagsObj.node = nodeVal
	return nil
}

func (c *certRotateFlow) getFrontIpsToSkipRootCAandCNPatching(automatesConfig map[string]*deployment.AutomateConfig, newRootCA string, newCn string, node string, infra *AutomateHAInfraDetails) []string {
	oldRootCA := ""
	oldCn := ""
	skipIpsList := []string{}
	for ip, config := range automatesConfig {
		if config.Global.V1.External.Opensearch != nil && config.Global.V1.External.Opensearch.Ssl != nil {
			if config.Global.V1.External.Opensearch.Ssl.RootCert != nil {
				oldRootCA = config.Global.V1.External.Opensearch.Ssl.RootCert.Value
				oldCn = config.Global.V1.External.Opensearch.Ssl.ServerName.Value
				if c.getFrontEndIpsForSkippingCnAndRootCaPatching(newRootCA, newCn, oldCn, oldRootCA, node, infra) == true {
					skipIpsList = append(skipIpsList, ip)
				}
				fmt.Printf("%s old rootca : %s ",ip,oldRootCA+"\n")	
				fmt.Printf("%s old CN : %s",ip,oldCn+"\n")
			}
		}
	}
	return skipIpsList
}

// patchConfig will patch the configurations to required nodes.
func (c *certRotateFlow) patchConfig(param *patchFnParameters) error {

	f, err := os.Create(param.fileName)
	if err != nil {
		return err
	}
	_, err = f.Write([]byte(param.config))
	if err != nil {
		return err
	}
	f.Close()

	var ips []string
	if param.flagsObj.node != "" && param.remoteService != "frontend" {
		isValid := c.validateEachIp(param.remoteService, param.infra, param.flagsObj)
		if !isValid {
			return errors.New(fmt.Sprintf("Please Enter Valid %s IP", param.remoteService))
		}
		ips = append(ips, param.flagsObj.node)
	} else {
		ips = c.getIps(param.remoteService, param.infra)
	}
	if len(ips) == 0 {
		return errors.New(fmt.Sprintf("No %s IPs are found", param.remoteService))
	}

	//collect ips on which need to perform action/cert-rotation
	filteredIps := c.getFilteredIps(ips, param.skipIpsList)

	// Defining set of commands which run on particular remoteservice nodes
	var scriptCommands string
	if param.remoteService == AUTOMATE || param.remoteService == CHEF_SERVER || param.remoteService == "frontend" {
		scriptCommands = fmt.Sprintf(FRONTEND_COMMAND, PATCH, param.remoteService+param.timestamp, DATE_FORMAT)
	} else if param.remoteService == POSTGRESQL || param.remoteService == OPENSEARCH {
		scriptCommands = fmt.Sprintf(COPY_USER_CONFIG, param.remoteService+param.timestamp, param.remoteService)
	}
	err = c.copyAndExecute(filteredIps, param.sshUtil, param.timestamp, param.remoteService, param.fileName, scriptCommands, param.flagsObj, printCertRotateOutput)
	if err != nil {
		return err
	}
	return nil
}

// copyAndExecute will copy the toml file to each required node and then execute the set of commands.
func (c *certRotateFlow) copyAndExecute(ips []string, sshUtil SSHUtil, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags, printCertRotateOutput func(CertRoateCmdResult, string, *cli.Writer)) error {
	//var err error
	var tomlFilePath string
	responseChan := make(chan CertRoateCmdResult, len(ips))
	defer close(responseChan)
	for i := 0; i < len(ips); i++ {
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		SSHConfig := c.getSshDetails(infra)
		SSHUtils := NewSSHUtil(SSHConfig)
		SSHConfig.timeout = flagsObj.timeout
		SSHUtils.setSSHConfig(SSHConfig)
		go func(hostIP string, responseChan chan CertRoateCmdResult) {
			rc := CertRoateCmdResult{hostIP, "", nil, writer, remoteService}
			SSHUtils.getSSHConfig().hostIP = hostIP
			if (flagsObj.postgres || flagsObj.opensearch) && remoteService != "frontend" {
				tomlFilePath, err = c.getMerger(fileName, timestamp, remoteService, GET_USER_CONFIG, SSHUtils)
				if err != nil {
					rc.err = err
					responseChan <- rc
					return
				}
				// Copying the new toml file which includes both old and new configurations (for backend nodes).
				err = SSHUtils.copyFileToRemote(tomlFilePath, remoteService+timestamp, false)
			} else {
				// Copying the new toml file which includes new configurations (for frontend nodes).
				err = SSHUtils.copyFileToRemote(fileName, remoteService+timestamp, false)
			}
			if err != nil {
				writer.Errorf("%v", err)
				rc.err = err
				responseChan <- rc
			}

			fmt.Printf("\nStarted Applying the Configurations in %s node: %s \n", remoteService, hostIP)

			output, err := SSHUtils.connectAndExecuteCommandOnRemote(scriptCommands, true)
			if err != nil {
				writer.Errorf("%v", err)
				rc.err = err
				responseChan <- rc
				return
			}
			rc.outputString = output
			responseChan <- rc

		}(ips[i], responseChan)
	}
	getCertChannelValue(ips, responseChan, printCertRotateOutput)
	return nil
}

func getCertChannelValue(ips []string, certRotateCmdResults chan CertRoateCmdResult, printCertRotateOutput func(CertRoateCmdResult, string, *cli.Writer)) error {
	for i := 0; i < len(ips); i++ {
		CertRoateCmdResult := <-certRotateCmdResults
		if CertRoateCmdResult.err != nil {
			return CertRoateCmdResult.err
		} else {
			if CertRoateCmdResult.writer != nil {
				printCertRotateOutput(CertRoateCmdResult, CertRoateCmdResult.nodeType, CertRoateCmdResult.writer)
			}
		}
	}
	return nil
}

func printCertRotateOutput(cmdResult CertRoateCmdResult, remoteService string, writer *cli.Writer) {
	writer.Printf("\n=======================================================================\n")
	if cmdResult.err != nil || strings.Contains(cmdResult.outputString, "DeploymentServiceCallError") {
		printCertRotateErrorOutput(cmdResult, remoteService, writer)
	} else {
		writer.Printf("Output for Host IP %s : \n%s", cmdResult.hostIp, cmdResult.outputString+"\n")
		writer.Success("Command is executed on node : " + cmdResult.hostIp + "\n")
	}
	writer.BufferWriter().Flush()
}

func printCertRotateErrorOutput(cmdResult CertRoateCmdResult, remoteService string, writer *cli.Writer) {
	isOutputError := false
	if strings.Contains(cmdResult.outputString, "DeploymentServiceCallError") {
		isOutputError = true
		writer.Failf(CMD_FAILED_MSG, remoteService, cmdResult.hostIp, cmdResult.outputString)
	}
	if !isOutputError {
		writer.Failf(CMD_FAILED_MSG, remoteService, cmdResult.hostIp, cmdResult.err.Error())
	}
}

// validateEachIp validate given ip with the remoteService cluster.
func (c *certRotateFlow) validateEachIp(remoteService string, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) bool {
	ips := c.getIps(remoteService, infra)
	for i := 0; i < len(ips); i++ {
		if ips[i] == flagsObj.node {
			return true
		}
	}
	return false
}

// getSshDetails will return the SSH details.
func (c *certRotateFlow) getSshDetails(infra *AutomateHAInfraDetails) *SSHConfig {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshConfig
}

// getIps will return the Ips based on the given remote service.
func (c *certRotateFlow) getIps(remoteService string, infra *AutomateHAInfraDetails) []string {
	if remoteService == AUTOMATE {
		return infra.Outputs.AutomatePrivateIps.Value
	} else if remoteService == CHEF_SERVER {
		return infra.Outputs.ChefServerPrivateIps.Value
	} else if remoteService == POSTGRESQL {
		return infra.Outputs.PostgresqlPrivateIps.Value
	} else if remoteService == OPENSEARCH {
		return infra.Outputs.OpensearchPrivateIps.Value
	} else if remoteService == "frontend" {
		return append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	}
	return []string{}
}

// isIPInCluster will check whether the given ip is in the cluster or not.
func (c *certRotateFlow) isIPInCluster(ip string, infra *AutomateHAInfraDetails) bool {
	cluster := c.getAllIPs(infra)
	for _, clusterIP := range cluster {
		if ip == clusterIP {
			return true
		}
	}
	return false
}

// getAllIps will return all the ips of the cluster.
func (c *certRotateFlow) getAllIPs(infra *AutomateHAInfraDetails) []string {
	ips := append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	ips = append(ips, infra.Outputs.PostgresqlPrivateIps.Value...)
	ips = append(ips, infra.Outputs.OpensearchPrivateIps.Value...)
	return ips
}

// getFilteredIps will return ips on which cert-rotation need to perform.
func (c *certRotateFlow) getFilteredIps(serviceIps, skipIpsList []string) []string {
	filteredIps := []string{}
	for _, ip := range serviceIps {
		if !stringutils.SliceContains(skipIpsList, ip) {
			filteredIps = append(filteredIps, ip)
		}
	}
	return filteredIps
}

// compareCurrentCertsWithNewCerts compare current certs and new certs and returns ips to skip cert-rotation.
func (c *certRotateFlow) compareCurrentCertsWithNewCerts(remoteService string, newCerts *certificates, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) []string {
	skipIpsList := []string{}
	isCertsSame := true
	if remoteService == AUTOMATE {
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.AutomateCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == CHEF_SERVER {
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.ChefServerCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == OPENSEARCH {
		if flagsObj.node == "" {
			isCertsSame = strings.TrimSpace(currentCertsInfo.OpensearchRootCert) == newCerts.rootCA
			isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminCert) == newCerts.adminCert) && isCertsSame
			isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminKey) == newCerts.adminKey) && isCertsSame
		}
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.OpensearchCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == POSTGRESQL {
		if flagsObj.node == "" {
			isCertsSame = strings.TrimSpace(currentCertsInfo.PostgresqlRootCert) == newCerts.rootCA
		}
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.PostgresqlCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}
	return skipIpsList
}

// comparePublicCertAndPrivateCert compare new public-cert and private cert with current public-cert and private-cert and returns ips to skip cert-rotation.
func (c *certRotateFlow) comparePublicCertAndPrivateCert(newCerts *certificates, certByIpList []CertByIP, isCertsSame bool, flagsObj *certRotateFlags) []string {
	skipIpsList := []string{}
	for _, currentCerts := range certByIpList {
		isCertsSameTemp := false
		if (strings.TrimSpace(currentCerts.PublicKey) == newCerts.publicCert) && (strings.TrimSpace(currentCerts.PrivateKey) == newCerts.privateCert) && isCertsSame {
			isCertsSameTemp = true
		}

		if isCertsSameTemp {
			if flagsObj.node == currentCerts.IP {
				return []string{flagsObj.node}
			}
			skipIpsList = append(skipIpsList, currentCerts.IP)
		}
	}
	return skipIpsList
}

// getFrontEndIpsForSkippingRootCAPatching compare new root-ca and current root-ca of remoteService and returns ips to skip root-ca patching.
func (c *certRotateFlow) getFrontEndIpsForSkippingRootCAPatching(remoteService string, newRootCA string, infra *AutomateHAInfraDetails, currentCertsInfo *certShowCertificates) []string {
	skipIpsList := []string{}

	if remoteService == POSTGRESQL {
		if strings.TrimSpace(currentCertsInfo.PostgresqlRootCert) == newRootCA {
			skipIpsList = append(skipIpsList, c.getIps(POSTGRESQL, infra)...)
		}
	}

	if remoteService == AUTOMATE {
		if strings.TrimSpace(currentCertsInfo.AutomateRootCert) == newRootCA {
			skipIpsList = append(skipIpsList, c.getIps(AUTOMATE, infra)...)
		}
	}

	if remoteService == CHEF_SERVER {
		if strings.TrimSpace(currentCertsInfo.AutomateRootCert) == newRootCA {
			skipIpsList = append(skipIpsList, c.getIps(CHEF_SERVER, infra)...)
		}
	}

	return skipIpsList
}

// getFrontEndIpsForSkippingCnAndRootCaPatching compare new root-ca and new cn with current root-ca and cn and returns ips to skip root-ca patching.
func (c *certRotateFlow) getFrontEndIpsForSkippingCnAndRootCaPatching(newRootCA, newCn, oldCn, oldRootCA, node string, infra *AutomateHAInfraDetails) bool {
	isRootCaSame := false

	if strings.TrimSpace(oldRootCA) == newRootCA {
		isRootCaSame = true
	}

	isCnSame := false

	if oldCn == newCn {
		isCnSame = true
	}

	if node == "" && isRootCaSame && isCnSame {
		return true
	}

	if node != "" && isCnSame {
		return true
	}

	return false
}

// skipMessagePrinter print the skip message
func (c *certRotateFlow) skipMessagePrinter(remoteService, skipIpsMsg, nodeFlag string, skipIpsList []string) {
	nodeString := "node"

	if len(skipIpsList) > 1 {
		nodeString = "nodes"
	}

	if len(skipIpsList) != 0 && nodeFlag == "" {
		writer.Skippedf(skipIpsMsg, remoteService, nodeString, nodeString, strings.Join(skipIpsList, ", "))
	}

	if len(skipIpsList) != 0 && nodeFlag != "" {
		if stringutils.SliceContains(skipIpsList, nodeFlag) {
			writer.Skippedf(skipIpsMsg, remoteService, nodeString, nodeString, strings.Join(skipIpsList, ", "))
		}
	}
}

// getCerts will read the certificate paths, and then return the required certificates.
func (c *certRotateFlow) getCerts(infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) (*certificates, error) {
	privateCertPath := flagsObj.privateCertPath
	publicCertPath := flagsObj.publicCertPath
	rootCaPath := flagsObj.rootCAPath
	adminCertPath := flagsObj.adminCertPath
	adminKeyPath := flagsObj.adminKeyPath

	var rootCA, adminCert, adminKey []byte
	var err error

	const fileAccessErrorMsg string = "failed reading data from the given source"

	if privateCertPath == "" || publicCertPath == "" {
		return nil, errors.New("Please provide both public-cert and private-cert flags")
	}

	privateCert, err := c.getCertFromFile(privateCertPath, infra)
	if err != nil {
		return nil, status.Wrap(
			err,
			status.FileAccessError,
			fileAccessErrorMsg,
		)
	}
	block, _ := pem.Decode(privateCert)
	if block == nil {
		return nil, errors.New("Please provide the valid certificate for private-cert")
	}

	publicCert, err := c.getCertFromFile(publicCertPath, infra)
	if err != nil {
		return nil, status.Wrap(
			err,
			status.FileAccessError,
			fileAccessErrorMsg,
		)
	}
	block, _ = pem.Decode(publicCert)
	if block == nil {
		return nil, errors.New("Please provide the valid certificate for public-cert")
	}

	// Root CA is mandatory for PG and OS nodes. But root CA is ignored when node flag is provided
	if flagsObj.postgres || flagsObj.opensearch {
		if rootCaPath == "" && flagsObj.node == "" {
			return nil, errors.New("Please provide root-ca flag")
		}
		if rootCaPath != "" {
			rootCA, err = c.getCertFromFile(rootCaPath, infra)
			if err != nil {
				return nil, status.Wrap(
					err,
					status.FileAccessError,
					fileAccessErrorMsg,
				)
			}
			block, _ = pem.Decode(rootCA)
			if block == nil {
				return nil, errors.New("Please provide the valid certificate for root-ca")
			}
		}
	}

	// Admin Cert and Admin Key is mandatory for OS nodes.
	if flagsObj.opensearch {
		if (adminCertPath == "" || adminKeyPath == "") && flagsObj.node == "" {
			return nil, errors.New("Please provide both admin-cert and admin-key flags")
		}
		if adminCertPath != "" && adminKeyPath != "" {
			adminCert, err = c.getCertFromFile(adminCertPath, infra)
			if err != nil {
				return nil, status.Wrap(
					err,
					status.FileAccessError,
					fileAccessErrorMsg,
				)
			}
			block, _ = pem.Decode(adminCert)
			if block == nil {
				return nil, errors.New("Please provide the valid certificate for admin-cert")
			}

			adminKey, err = c.getCertFromFile(adminKeyPath, infra)
			if err != nil {
				return nil, status.Wrap(
					err,
					status.FileAccessError,
					fileAccessErrorMsg,
				)
			}
			block, _ = pem.Decode(adminKey)
			if block == nil {
				return nil, errors.New("Please provide the valid certificate for admin-key")
			}
		}
	}

	certs := &certificates{
		privateCert: strings.TrimSpace(string(privateCert)),
		publicCert:  strings.TrimSpace(string(publicCert)),
		rootCA:      strings.TrimSpace(string(rootCA)),
		adminCert:   strings.TrimSpace(string(adminCert)),
		adminKey:    strings.TrimSpace(string(adminKey)),
	}
	return certs, nil
}

// getCertFromFile will read the certificate from the given path (local or remote).
func (c *certRotateFlow) getCertFromFile(certPath string, infra *AutomateHAInfraDetails) ([]byte, error) {
	certPath = strings.TrimSpace(certPath)
	// Checking if the given path is remote or local.
	if c.IsRemotePath(certPath) {
		remoteFilePath, _, hostIP, err := c.GetRemoteFileDetails(certPath, infra)
		if err != nil {
			return nil, err
		}
		// Download certificate from remote host.
		sshConfig := c.getSshDetails(infra)
		sshConfig.hostIP = hostIP
		sshUtil := NewSSHUtil(sshConfig)

		out, err := sshUtil.connectAndExecuteCommandOnRemote("sudo cat "+remoteFilePath, true)
		if err != nil {
			return nil, errors.Wrap(err, fmt.Sprintf("Unable to read file from remote path: %v", certPath))
		}
		return []byte(out), nil
	}
	return c.FileUtils.ReadFile(certPath)
}

// GetRemoteFileDetails returns the remote file details from the remotePath.
func (c *certRotateFlow) GetRemoteFileDetails(remotePath string, infra *AutomateHAInfraDetails) (string, string, string, error) {
	// Get Host IP from the given path and validate it.
	hostIP := c.GetIPV4(remotePath)
	if net.ParseIP(hostIP).To4() == nil {
		return "", "", "", errors.New(fmt.Sprintf("%v is not a valid IPv4 address", hostIP))
	}

	// Check if given IP is part cluster.
	if !c.isIPInCluster(hostIP, infra) {
		return "", "", "", errors.New(fmt.Sprintf("%v is not part of the cluster", hostIP))
	}

	// Get the file path and filename from the given remote address.
	certPaths := strings.Split(remotePath, ":")
	if len(certPaths) != 2 || certPaths[1] == "" {
		return "", "", "", errors.New(fmt.Sprintf("Invalid remote path: %v", remotePath))
	}

	// Get the file path and validate it.
	remoteFilePath := filepath.Clean(strings.TrimSpace(certPaths[1]))

	// Get the filename from the file path.
	fileName := filepath.Base(remoteFilePath)
	if fileName == "." || fileName == string(os.PathSeparator) {
		return "", "", "", errors.New(fmt.Sprintf("Invalid remote path: %v", remotePath))
	}

	return remoteFilePath, fileName, hostIP, nil
}

/*
IsRemotePath checks whether given path is valid remote path.
Path should be in this format <IPv4>:<PathToFile>.

Example: 10.1.0.234:/home/ec2-user/certs/public.pem
*/
func (c *certRotateFlow) IsRemotePath(path string) bool {
	pattern := regexp.MustCompile(`^` + IP_V4_REGEX + `:`)
	return pattern.MatchString(path)
}

// GetIPV4 returns ip from given path.
func (c *certRotateFlow) GetIPV4(path string) string {
	pattern := regexp.MustCompile(IP_V4_REGEX)
	return pattern.FindString(path)
}

/*
getMerger will create the new toml file which includes old and new configurations.

If we are working on backend service, then first we have to get the applied configurations
and then merge it with new configurations, then apply that configuration.
Because if we directly apply the new configurations, then the old applied configurations will be gone.
So, we have to retain the old configurations also.
*/
func (c *certRotateFlow) getMerger(fileName string, timestamp string, remoteType string, config string, sshUtil SSHUtil) (string, error) {
	tomlFile := fileName + timestamp
	scriptCommands := fmt.Sprintf(config, remoteType)
	rawOutput, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	if err != nil {
		return "", err
	}

	var (
		dest interface{}
		err1 error
	)
	if remoteType == OPENSEARCH {
		dest, err1 = getMergedOpensearchInterface(rawOutput, fileName, remoteType)
	} else {
		dest, err1 = getMergedPostgresqlInterface(rawOutput, fileName, remoteType)
	}
	if err1 != nil {
		return "", err1
	}

	f, err := os.Create(tomlFile)

	if err != nil {
		// failed to create/open the file
		writer.Bodyf("Failed to create/open the file, \n%v", err)
		return "", err
	}
	if err := toml.NewEncoder(f).Encode(dest); err != nil {
		// failed to encode
		writer.Bodyf("Failed to encode\n%v", err)
		return "", err
	}
	if err := f.Close(); err != nil {
		// failed to close the file
		writer.Bodyf("Failed to close the file\n%v", err)
		return "", err
	}

	return tomlFile, nil
}
