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

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/io/fileutils"
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

	CHEFSERVER_ROOTCA_CONFIG = `
	[cs_nginx.v1.sys.ngx.http]
		ssl_verify_depth = 6
	[global.v1.external.automate.ssl]
		server_name = "https://%v"
		root_cert = """%v"""`

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
		nodes_dn = '- %v'`

	OPENSEARCH_FRONTEND_CONFIG = `
	[global.v1.external.opensearch.ssl]
		root_cert = """%v"""
		server_name = "%v"`

	OPENSEARCH_FRONTEND_CONFIG_IGNORE_ROOT_CERT = `
	[global.v1.external.opensearch.ssl]
		server_name = "%v"`

	GET_USER_CONFIG = `
	sudo cat /hab/user/automate-ha-%s/config/user.toml`

	COPY_USER_CONFIG = `
	sudo systemctl stop hab-sup.service
	echo "y" | sudo cp /tmp/%s /hab/user/automate-ha-%s/config/user.toml
	sudo systemctl start hab-sup.service`

	IP_V4_REGEX = `(\b25[0-5]|\b2[0-4][0-9]|\b[01]?[0-9][0-9]?)(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}`
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
	node string
}

type certRotateFlow struct {
	FileUtils fileutils.FileUtils
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

	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.automate, CONST_AUTOMATE, "a", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.automate, "a2", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.chefserver, CONST_CHEF_SERVER, "c", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.chefserver, "cs", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.postgres, CONST_POSTGRESQL, "p", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.postgres, "pg", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.opensearch, CONST_OPENSEARCH, "o", false, "OS Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.opensearch, "os", false, "OS Certificate Rotation")

	certRotateCmd.PersistentFlags().StringVar(&flagsObj.privateCertPath, "private-cert", "", "Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.publicCertPath, "public-cert", "", "Public certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.rootCAPath, "root-ca", "", "RootCA certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.adminCertPath, "admin-cert", "", "Admin certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.adminKeyPath, "admin-key", "", "Admin Private certificate")

	certRotateCmd.PersistentFlags().StringVar(&flagsObj.node, "node", "", "Node Ip address")

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
		certShowFlow := NewCertShowImpl(certShowFlags{}, NewNodeUtils(), sshUtil, writer)
		currentCertsInfo,err:= certShowFlow.certShowHelper(true)

		if err!=nil {
			return err
		}

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
func (c *certRotateFlow) certRotateFrontend(sshUtil SSHUtil, certs *certificates, infra *AutomteHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	fileName := "cert-rotate-fe.toml"
	timestamp := time.Now().Format("20060102150405")
	var remoteService string

	if flagsObj.automate {
		remoteService = CONST_AUTOMATE
	} else if flagsObj.chefserver {
		remoteService = CONST_CHEF_SERVER
	}
	//get ips to exclude
	skipIpsList := c.CompareCurrentCertsWithNewCerts(remoteService, certs,flagsObj, currentCertsInfo)
	skipMessage := "Following %s ip/ips will skipped while cert-rotation\n %v"
	c.skipMessagePrinter(remoteService, skipMessage, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.
	config := fmt.Sprintf(FRONTEND_CONFIG, certs.publicCert, certs.privateCert, certs.publicCert, certs.privateCert)
	err := c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, flagsObj, skipIpsList)
	if err != nil {
		return err
	}

	// ignore root-ca when node flag is provided
	if flagsObj.node != "" {
		return nil
	}

	// If we pass root-ca flag in automate then we need to update root-ca in the ChefServer to maintain the connection
	if flagsObj.automate {
		skipIpsList := c.getFrontEndIpsForSkippingRootCAPatching(CONST_AUTOMATE, certs.rootCA, infra, currentCertsInfo)
		skipMessage = "Follwing chef_server ip/ips will skipped while patching Automate root-ca\n %v"
		if len(skipIpsList) != 0 {
			writer.Skippedf(skipMessage,skipIpsList)
		}
		err = c.patchRootCAinCS(sshUtil, certs.rootCA, timestamp, infra, flagsObj, skipIpsList)
		if err != nil {
			return err
		}
	}
	return nil
}

// certRotatePG will rotate the certificates of Postgres.
func (c *certRotateFlow) certRotatePG(sshUtil SSHUtil, certs *certificates, infra *AutomteHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	if isManagedServicesOn() {
		return errors.New("You can not rotate certs for AWS managed services")
	}
	fileName := "cert-rotate-pg.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := CONST_POSTGRESQL

	// Creating and patching the required configurations.
	var config string
	if flagsObj.node != "" {
		config = fmt.Sprintf(POSTGRES_CONFIG_IGNORE_ISSUER_CERT, certs.privateCert, certs.publicCert)
	} else {
		config = fmt.Sprintf(POSTGRES_CONFIG, certs.privateCert, certs.publicCert, certs.rootCA)
	}

	skipIpsList := c.CompareCurrentCertsWithNewCerts(remoteService, certs,flagsObj, currentCertsInfo)
	skipMessage := "Following %s ip/ips will skipped while cert-rotation\n %v"
	c.skipMessagePrinter(remoteService, skipMessage, flagsObj.node, skipIpsList)

	//patching on PG
	err := c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, flagsObj, skipIpsList)
	if err != nil {
		return err
	}

	// ignore patching of root-ca when node flag is provided
	if flagsObj.node != "" {
		return nil
	}
	//get frontend ips to skip root-ca patch
	skipIpsList = c.getFrontEndIpsForSkippingRootCAPatching(remoteService, certs.rootCA, infra, currentCertsInfo)
	skipMessage = "Following FrontEnd ip/ips will skipped while patching Postgresql root-ca\n %v"

	if len(skipIpsList) != 0 {
		writer.Skippedf(skipMessage, skipIpsList)
	}

	// Patching root-ca to frontend-nodes for maintaining the connection.
	filenameFe := "pg_fe.toml"
	remoteService = "frontend"
	// Creating and patching the required configurations.
	configFe := fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, certs.rootCA)
	err = c.patchConfig(sshUtil, configFe, filenameFe, timestamp, remoteService, infra, flagsObj, skipIpsList)
	if err != nil {
		return err
	}
	return nil
}

// certRotateOS will rotate the certificates of OpenSearch.
func (c *certRotateFlow) certRotateOS(sshUtil SSHUtil, certs *certificates, infra *AutomteHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	if isManagedServicesOn() {
		return errors.New("You can not rotate certs for AWS managed services")
	}
	fileName := "cert-rotate-os.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := CONST_OPENSEARCH

	var adminDn pkix.Name
	var err error
	if flagsObj.node == "" {
		adminDn, err = getDistinguishedNameFromKey(certs.adminCert)
		if err != nil {
			return err
		}
	}
	nodesDn, err := getDistinguishedNameFromKey(certs.publicCert)
	if err != nil {
		return err
	}
	skipIpsList := c.CompareCurrentCertsWithNewCerts(remoteService, certs,flagsObj, currentCertsInfo)
	skipMessage := "Following %s ip/ips will skipped while cert-rotation\n %v"
	c.skipMessagePrinter(remoteService, skipMessage, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.
	var config string
	if flagsObj.node != "" {
		config = fmt.Sprintf(OPENSEARCH_CONFIG_IGNORE_ADMIN_AND_ROOTCA, certs.publicCert, certs.privateCert, fmt.Sprintf("%v", nodesDn))
	} else {
		config = fmt.Sprintf(OPENSEARCH_CONFIG, certs.rootCA, certs.adminCert, certs.adminKey, certs.publicCert, certs.privateCert, fmt.Sprintf("%v", adminDn), fmt.Sprintf("%v", nodesDn))
	}
	err = c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, flagsObj, skipIpsList)
	if err != nil {
		return err
	}

	// Patching root-ca to frontend-nodes for maintaining the connection.
	cn := nodesDn.CommonName
	filenameFe := "os_fe.toml"
	remoteService = "frontend"

	// Creating and patching the required configurations.
	var configFe string
	if flagsObj.node != "" {
		configFe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG_IGNORE_ROOT_CERT, cn)
	} else {
		configFe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, certs.rootCA, cn)
	}
	err = c.patchConfig(sshUtil, configFe, filenameFe, timestamp, remoteService, infra, flagsObj, []string{})
	if err != nil {
		return err
	}
	return nil
}

// patchConfig will patch the configurations to required nodes.
func (c *certRotateFlow) patchConfig(sshUtil SSHUtil, config, filename, timestamp, remoteService string, infra *AutomteHAInfraDetails, flagsObj *certRotateFlags, skipIpsList []string) error {

	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	_, err = f.Write([]byte(config))
	if err != nil {
		return err
	}
	f.Close()

	var ips []string
	if flagsObj.node != "" && remoteService != "frontend" {
		isValid := c.validateEachIp(remoteService, infra, flagsObj)
		if !isValid {
			return errors.New(fmt.Sprintf("Please Enter Valid %s IP", remoteService))
		}
		ips = append(ips, flagsObj.node)
	} else {
		ips = c.getIps(remoteService, infra)
	}
	if len(ips) == 0 {
		return errors.New(fmt.Sprintf("No %s IPs are found", remoteService))
	}

	//collect ips on which need to perform action/cert-rotation
	filteredIps := []string{}
	for _, ip := range ips {
		if stringutils.SliceContains(skipIpsList, ip) == false {
			filteredIps = append(filteredIps, ip)
		}
	}

	// Defining set of commands which run on particular remoteservice nodes
	var scriptCommands string
	if remoteService == CONST_AUTOMATE || remoteService == CONST_CHEF_SERVER || remoteService == "frontend" {
		scriptCommands = fmt.Sprintf(FRONTEND_COMMAND, PATCH, remoteService+timestamp, dateFormat)
	} else if remoteService == CONST_POSTGRESQL || remoteService == CONST_OPENSEARCH {
		scriptCommands = fmt.Sprintf(COPY_USER_CONFIG, remoteService+timestamp, remoteService)
	}
	err = c.copyAndExecute(filteredIps, sshUtil, timestamp, remoteService, filename, scriptCommands, flagsObj)
	if err != nil {
		return err
	}
	return nil
}

// patchRootCAinCS will rotate the root-ca in the ChefServer to maintain the connection.
func (c *certRotateFlow) patchRootCAinCS(sshUtil SSHUtil, rootCA, timestamp string, infra *AutomteHAInfraDetails, flagsObj *certRotateFlags, skipIpsList []string) error {

	fileName := "rotate-root_CA.toml"
	remoteService := CONST_CHEF_SERVER
	cmd := `sudo chef-automate config show | grep fqdn | awk '{print $3}' | tr -d '"'`
	ips := c.getIps(remoteService, infra)
	if len(ips) == 0 {
		return errors.New(fmt.Sprintf("No %s IPs are found", remoteService))
	}
	sshUtil.getSSHConfig().hostIP = ips[0]
	fqdn, err := sshUtil.connectAndExecuteCommandOnRemote(cmd, true)
	if err != nil {
		return err
	}

	config := fmt.Sprintf(CHEFSERVER_ROOTCA_CONFIG, strings.TrimSpace(string(fqdn)), rootCA)
	//TODO : print some prompt before patching of node exluding....
	err = c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, flagsObj, skipIpsList)
	if err != nil {
		return err
	}
	return nil
}

// copyAndExecute will copy the toml file to each required node and then execute the set of commands.
func (c *certRotateFlow) copyAndExecute(ips []string, sshUtil SSHUtil, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags) error {

	var err error
	var tomlFilePath string
	for i := 0; i < len(ips); i++ {
		sshUtil.getSSHConfig().hostIP = ips[i]
		if (flagsObj.postgres || flagsObj.opensearch) && remoteService != "frontend" {
			tomlFilePath, err = c.getMerger(fileName, timestamp, remoteService, GET_USER_CONFIG, sshUtil)
			if err != nil {
				return err
			}
			// Copying the new toml file which includes both old and new configurations (for backend nodes).
			err = sshUtil.copyFileToRemote(tomlFilePath, remoteService+timestamp, false)
		} else {
			// Copying the new toml file which includes new configurations (for frontend nodes).
			err = sshUtil.copyFileToRemote(fileName, remoteService+timestamp, false)
		}
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}

		fmt.Printf("Started Applying the Configurations in %s node: %s", remoteService, ips[i])
		output, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}
		writer.Printf(output + "\n")
	}
	return nil
}

// validateEachIp validate given ip with the remoteService cluster.
func (c *certRotateFlow) validateEachIp(remoteService string, infra *AutomteHAInfraDetails, flagsObj *certRotateFlags) bool {
	ips := c.getIps(remoteService, infra)
	for i := 0; i < len(ips); i++ {
		if ips[i] == flagsObj.node {
			return true
		}
	}
	return false
}

// getSshDetails will return the SSH details.
func (c *certRotateFlow) getSshDetails(infra *AutomteHAInfraDetails) *SSHConfig {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshConfig
}

// getIps will return the Ips based on the given remote service.
func (c *certRotateFlow) getIps(remoteService string, infra *AutomteHAInfraDetails) []string {
	if remoteService == CONST_AUTOMATE {
		return infra.Outputs.AutomatePrivateIps.Value
	} else if remoteService == CONST_CHEF_SERVER {
		return infra.Outputs.ChefServerPrivateIps.Value
	} else if remoteService == CONST_POSTGRESQL {
		return infra.Outputs.PostgresqlPrivateIps.Value
	} else if remoteService == CONST_OPENSEARCH {
		return infra.Outputs.OpensearchPrivateIps.Value
	} else if remoteService == "frontend" {
		return append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	}
	return []string{}
}

// isIPInCluster will check whether the given ip is in the cluster or not.
func (c *certRotateFlow) isIPInCluster(ip string, infra *AutomteHAInfraDetails) bool {
	cluster := c.getAllIPs(infra)
	for _, clusterIP := range cluster {
		if ip == clusterIP {
			return true
		}
	}
	return false
}

// getAllIps will return all the ips of the cluster.
func (c *certRotateFlow) getAllIPs(infra *AutomteHAInfraDetails) []string {
	ips := append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	ips = append(ips, infra.Outputs.PostgresqlPrivateIps.Value...)
	ips = append(ips, infra.Outputs.OpensearchPrivateIps.Value...)
	return ips
}

func (c *certRotateFlow) CompareCurrentCertsWithNewCerts(remoteService string, newCerts *certificates, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) []string {
	skipIpsList := []string{}

	if remoteService == CONST_AUTOMATE {
		isCertsSame := strings.TrimSpace(currentCertsInfo.AutomateRootCert) == newCerts.rootCA
		skipIpsList = c.ComparePublicCertAndPrivateCert(newCerts, currentCertsInfo.AutomateCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == CONST_CHEF_SERVER {
		skipIpsList = c.ComparePublicCertAndPrivateCert(newCerts, currentCertsInfo.ChefServerCertsByIP, true, flagsObj)
		return skipIpsList
	}

	if remoteService == CONST_OPENSEARCH {
		isCertsSame := strings.TrimSpace(currentCertsInfo.OpensearchRootCert) == newCerts.rootCA
		isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminCert) == newCerts.adminCert) && isCertsSame
		isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminKey) == newCerts.adminKey) && isCertsSame
		skipIpsList = c.ComparePublicCertAndPrivateCert(newCerts, currentCertsInfo.OpensearchCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == CONST_POSTGRESQL {
		isCertsSame := strings.TrimSpace(currentCertsInfo.PostgresqlRootCert) == newCerts.rootCA
		skipIpsList = c.ComparePublicCertAndPrivateCert(newCerts, currentCertsInfo.PostgresqlCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	return skipIpsList
}

func (c *certRotateFlow) ComparePublicCertAndPrivateCert(newCerts *certificates, certByIpList []CertByIP, isCertsSame bool, flagsObj *certRotateFlags) []string {
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

func (c *certRotateFlow) getFrontEndIpsForSkippingRootCAPatching(remoteService string, newRootCA string, infra *AutomteHAInfraDetails, currentCertsInfo *certShowCertificates) []string {
	skipIpsList := []string{}

	if remoteService == CONST_POSTGRESQL {
		//patch root-ca FE
		if strings.TrimSpace(currentCertsInfo.PostgresqlRootCert) == newRootCA {
			skipIpsList = append(skipIpsList, c.getIps("frontend", infra)...)
		}
	}

	if remoteService == CONST_AUTOMATE {
		//patch root-ca in CS
		if strings.TrimSpace(currentCertsInfo.AutomateRootCert) == newRootCA {
			skipIpsList = append(skipIpsList, c.getIps(CONST_CHEF_SERVER, infra)...)
		}
	}

	return skipIpsList
}

func (c *certRotateFlow) skipMessagePrinter(remoteService, skipMessage, nodeFlag string, skipIpsList []string) {
	if len(skipIpsList) != 0 && nodeFlag==""{
		writer.Skippedf(skipMessage, remoteService, skipIpsList)
	}

	if len(skipIpsList) != 0 && nodeFlag != "" {
		if stringutils.SliceContains(skipIpsList, nodeFlag) {
			writer.Skippedf(skipMessage, remoteService, skipIpsList)
		}
	}
}

// getCerts will read the certificate paths, and then return the required certificates.
func (c *certRotateFlow) getCerts(infra *AutomteHAInfraDetails, flagsObj *certRotateFlags) (*certificates, error) {
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

	// Root CA is mandatory for A2, PG and OS nodes. But root CA is ignored when node flag is provided
	if flagsObj.automate || flagsObj.postgres || flagsObj.opensearch {
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
func (c *certRotateFlow) getCertFromFile(certPath string, infra *AutomteHAInfraDetails) ([]byte, error) {
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
func (c *certRotateFlow) GetRemoteFileDetails(remotePath string, infra *AutomteHAInfraDetails) (string, string, string, error) {
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
	if remoteType == CONST_OPENSEARCH {
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
