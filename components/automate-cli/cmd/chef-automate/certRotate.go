package main

import (
	"crypto/x509/pkix"
	"fmt"
	"log"
	"net"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type certFlags struct {
	privateCert string
	publicCert  string
	rootCA      string
	adminCert   string
	adminKey    string
}

type nodeFlag struct {
	node string
}

type sshFlag struct {
	automate   bool
	chefserver bool
	postgres   bool
	opensearch bool
}

type certRotateFlow struct {
	FileUtils fileutils.FileUtils
}

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

func init() {
	sshFlagObj := sshFlag{}
	certFlagsObj := certFlags{}
	nodeFlagObj := nodeFlag{}

	certRotateCmd := &cobra.Command{
		Use:   "cert-rotate",
		Short: "Chef Automate rotate cert",
		Long:  "Chef Automate CLI command to rotate certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certRotateCmdFunc(&sshFlagObj, &certFlagsObj, &nodeFlagObj),
	}

	RootCmd.AddCommand(certRotateCmd)

	certRotateCmd.PersistentFlags().BoolVarP(&sshFlagObj.automate, "automate", "a", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlagObj.automate, "a2", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&sshFlagObj.chefserver, "chef_server", "c", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlagObj.chefserver, "cs", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&sshFlagObj.postgres, "postgresql", "p", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlagObj.postgres, "pg", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&sshFlagObj.opensearch, "opensearch", "o", false, "OS Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlagObj.opensearch, "os", false, "OS Certificate Rotation")

	certRotateCmd.PersistentFlags().StringVar(&certFlagsObj.privateCert, "private-cert", "", "Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlagsObj.publicCert, "public-cert", "", "Public certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlagsObj.rootCA, "root-ca", "", "RootCA certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlagsObj.adminCert, "admin-cert", "", "Admin certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlagsObj.adminKey, "admin-key", "", "Admin Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&nodeFlagObj.node, "node", "", "Node Ip address")
}

func certRotateCmdFunc(sshFlagObj *sshFlag, certFlagsObj *certFlags, nodeFlagObj *nodeFlag) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		c := certRotateFlow{FileUtils: &fileutils.FileSystemUtils{}}
		return c.certRotate(cmd, args, sshFlagObj, certFlagsObj, nodeFlagObj)
	}
}

// This function will rotate the certificates of Automate, Chef Infra Server, Postgres and Opensearch.
func (c *certRotateFlow) certRotate(cmd *cobra.Command, args []string, sshFlagObj *sshFlag, certFlagsObj *certFlags, nodeFlagObj *nodeFlag) error {
	if isA2HARBFileExist() {
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}

		rootCA, publicCert, privateCert, adminCert, adminKey, err := c.getCerts(infra, sshFlagObj, certFlagsObj, nodeFlagObj)
		if err != nil {
			log.Fatal(err)
		}

		// we need to ignore root-ca, adminCert and adminKey in the case of each node
		if rootCA != "" && nodeFlagObj.node != "" {
			writer.Warn("root-ca flag will be ignored when node flag is provided")
		}
		if (adminCert != "" || adminKey != "") && nodeFlagObj.node != "" {
			writer.Warn("admin-cert and admin-key flag will be ignored when node flag is provided")
		}

		sshConfig := c.getSshDetails(infra)
		sshUtil := NewSSHUtil(sshConfig)
		if sshFlagObj.automate || sshFlagObj.chefserver {
			err := c.certRotateFrontend(sshUtil, publicCert, privateCert, rootCA, infra, sshFlagObj, nodeFlagObj)
			if err != nil {
				log.Fatal(err)
			}
		} else if sshFlagObj.postgres {
			err := c.certRotatePG(sshUtil, publicCert, privateCert, rootCA, infra, sshFlagObj, nodeFlagObj)
			if err != nil {
				log.Fatal(err)
			}
		} else if sshFlagObj.opensearch {
			err := c.certRotateOS(sshUtil, publicCert, privateCert, rootCA, adminCert, adminKey, infra, sshFlagObj, nodeFlagObj)
			if err != nil {
				log.Fatal(err)
			}
		}
	} else {
		log.Fatal(fmt.Errorf("cert-rotate command should be executed from Automate HA Bastion Node"))
	}

	return nil
}

// This function will rotate the certificates of Automate and Chef Infra Server,
func (c *certRotateFlow) certRotateFrontend(sshUtil SSHUtil, publicCert, privateCert, rootCA string, infra *AutomteHAInfraDetails, sshFlagObj *sshFlag, nodeFlagObj *nodeFlag) error {
	fileName := "cert-rotate-fe.toml"
	timestamp := time.Now().Format("20060102150405")
	var remoteService string

	if sshFlagObj.automate {
		remoteService = "automate"
	} else if sshFlagObj.chefserver {
		remoteService = "chefserver"
	}
	// Creating and patching the required configurations.
	config := fmt.Sprintf(FRONTEND_CONFIG, publicCert, privateCert, publicCert, privateCert)
	err := c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, sshFlagObj, nodeFlagObj)
	if err != nil {
		log.Fatal(err)
	}

	// ignore root-ca when node flag is provided
	if nodeFlagObj.node != "" {
		return nil
	}

	// If we pass root-ca flag in automate then we need to update root-ca in the ChefServer to maintain the connection
	if sshFlagObj.automate {
		err = c.patchRootCAinCS(sshUtil, rootCA, timestamp, infra, sshFlagObj, nodeFlagObj)
		if err != nil {
			log.Fatal(err)
		}
	}
	return nil
}

// This function will rotate the certificates of Postgres
func (c *certRotateFlow) certRotatePG(sshUtil SSHUtil, publicCert, privateCert, rootCA string, infra *AutomteHAInfraDetails, sshFlagObj *sshFlag, nodeFlagObj *nodeFlag) error {
	if isManagedServicesOn() {
		return errors.New("You can not rotate certs for AWS managed services")
	}
	fileName := "cert-rotate-pg.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := "postgresql"

	// Creating and patching the required configurations.
	var config string
	if nodeFlagObj.node != "" {
		config = fmt.Sprintf(POSTGRES_CONFIG_IGNORE_ISSUER_CERT, privateCert, publicCert)
	} else {
		config = fmt.Sprintf(POSTGRES_CONFIG, privateCert, publicCert, rootCA)
	}

	err := c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, sshFlagObj, nodeFlagObj)
	if err != nil {
		log.Fatal(err)
	}

	// ignore patching of root-ca when node flag is provided
	if nodeFlagObj.node != "" {
		return nil
	}
	// Patching root-ca to frontend-nodes for maintaining the connection.
	filename_fe := "pg_fe.toml"
	remoteService = "frontend"
	// Creating and patching the required configurations.
	config_fe := fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, rootCA)
	err = c.patchConfig(sshUtil, config_fe, filename_fe, timestamp, remoteService, infra, sshFlagObj, nodeFlagObj)
	if err != nil {
		log.Fatal(err)
	}
	return nil
}

// This function will rotate the certificates of OpenSearch
func (c *certRotateFlow) certRotateOS(sshUtil SSHUtil, publicCert, privateCert, rootCA, adminCert, adminKey string, infra *AutomteHAInfraDetails, sshFlagObj *sshFlag, nodeFlagObj *nodeFlag) error {
	if isManagedServicesOn() {
		return errors.New("You can not rotate certs for AWS managed services")
	}
	fileName := "cert-rotate-os.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := "opensearch"

	e := existingInfra{}
	var admin_dn pkix.Name
	var err error
	if nodeFlagObj.node == "" {
		admin_dn, err = e.getDistinguishedNameFromKey(adminCert)
		if err != nil {
			return err
		}
	}
	nodes_dn, err := e.getDistinguishedNameFromKey(publicCert)
	if err != nil {
		return err
	}

	// Creating and patching the required configurations.
	var config string
	if nodeFlagObj.node != "" {
		config = fmt.Sprintf(OPENSEARCH_CONFIG_IGNORE_ADMIN_AND_ROOTCA, publicCert, privateCert, fmt.Sprintf("%v", nodes_dn))
	} else {
		config = fmt.Sprintf(OPENSEARCH_CONFIG, rootCA, adminCert, adminKey, publicCert, privateCert, fmt.Sprintf("%v", admin_dn), fmt.Sprintf("%v", nodes_dn))
	}
	err = c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, sshFlagObj, nodeFlagObj)
	if err != nil {
		log.Fatal(err)
	}

	// Patching root-ca to frontend-nodes for maintaining the connection.
	cn := nodes_dn.CommonName
	filename_fe := "os_fe.toml"
	remoteService = "frontend"

	// Creating and patching the required configurations.
	var config_fe string
	if nodeFlagObj.node != "" {
		config_fe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG_IGNORE_ROOT_CERT, cn)
	} else {
		config_fe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, rootCA, cn)
	}
	err = c.patchConfig(sshUtil, config_fe, filename_fe, timestamp, remoteService, infra, sshFlagObj, nodeFlagObj)
	if err != nil {
		log.Fatal(err)
	}
	return nil
}

// This function will patch the configurations to required nodes.
func (c *certRotateFlow) patchConfig(sshUtil SSHUtil, config, filename, timestamp, remoteService string, infra *AutomteHAInfraDetails, sshFlagObj *sshFlag, nodeFlagObj *nodeFlag) error {
	f, err := os.Create(filename)
	if err != nil {
		log.Fatal(err)
	}
	_, err = f.Write([]byte(config))
	if err != nil {
		log.Fatal(err)
	}
	f.Close()

	var ips []string
	if nodeFlagObj.node != "" && remoteService != "frontend" {
		isValid := c.validateEachIp(remoteService, infra, nodeFlagObj)
		if !isValid {
			return errors.New(fmt.Sprintf("Please Enter Valid %s IP", remoteService))
		}
		ips = append(ips, nodeFlagObj.node)
	} else {
		ips = c.getIps(remoteService, infra)
	}
	if len(ips) == 0 {
		return errors.New(fmt.Sprintf("No %s IPs are found", remoteService))
	}

	// Defining set of commands which run on particular remoteservice nodes
	var scriptCommands string
	if remoteService == "automate" || remoteService == "chefserver" || remoteService == "frontend" {
		scriptCommands = fmt.Sprintf(FRONTEND_COMMANDS, remoteService+timestamp, dateFormat)
	} else if remoteService == "postgresql" || remoteService == "opensearch" {
		scriptCommands = fmt.Sprintf(COPY_USER_CONFIG, remoteService+timestamp, remoteService)
	}
	err = c.copyAndExecute(ips, sshUtil, timestamp, remoteService, filename, scriptCommands, sshFlagObj, nodeFlagObj)
	if err != nil {
		log.Fatal(err)
	}
	return nil
}

// This function will rotate the root-ca in the ChefServer to maintain the connection
func (c *certRotateFlow) patchRootCAinCS(sshUtil SSHUtil, rootCA, timestamp string, infra *AutomteHAInfraDetails, sshFlagObj *sshFlag, nodeFlagObj *nodeFlag) error {

	fileName := "rotate-root_CA.toml"
	remoteService := "chefserver"
	cmd := `sudo chef-automate config show | grep fqdn | awk '{print $3}' | tr -d '"'`
	ips := c.getIps(remoteService, infra)
	if len(ips) == 0 {
		return errors.New(fmt.Sprintf("No %s IPs are found", remoteService))
	}
	sshUtil.getSSHConfig().hostIP = ips[0]
	fqdn, err := sshUtil.connectAndExecuteCommandOnRemote(cmd, true)
	if err != nil {
		log.Fatal(err)
	}

	config := fmt.Sprintf(CHEFSERVER_ROOTCA_CONFIG, strings.TrimSpace(string(fqdn)), rootCA)
	err = c.patchConfig(sshUtil, config, fileName, timestamp, remoteService, infra, sshFlagObj, nodeFlagObj)
	if err != nil {
		log.Fatal(err)
	}
	return nil
}

// This function will copy the toml file to each required node and then execute the set of commands.
func (c *certRotateFlow) copyAndExecute(ips []string, sshUtil SSHUtil, timestamp string, remoteService string, fileName string, scriptCommands string, sshFlagObj *sshFlag, nodeFlagObj *nodeFlag) error {

	var err error
	var tomlFilePath string
	for i := 0; i < len(ips); i++ {
		sshUtil.getSSHConfig().hostIP = ips[i]
		if (sshFlagObj.postgres || sshFlagObj.opensearch) && remoteService != "frontend" {
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

func (c *certRotateFlow) validateEachIp(remoteService string, infra *AutomteHAInfraDetails, nodeFlagObj *nodeFlag) bool {
	ips := c.getIps(remoteService, infra)
	for i := 0; i < len(ips); i++ {
		if ips[i] == nodeFlagObj.node {
			return true
		}
	}
	return false
}

// This function will return the SSH details.
func (c *certRotateFlow) getSshDetails(infra *AutomteHAInfraDetails) *SSHConfig {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshConfig
}

// This function will return the Ips based on the given remote service.
func (c *certRotateFlow) getIps(remoteService string, infra *AutomteHAInfraDetails) []string {
	if remoteService == "automate" {
		return infra.Outputs.AutomatePrivateIps.Value
	} else if remoteService == "chefserver" {
		return infra.Outputs.ChefServerPrivateIps.Value
	} else if remoteService == "postgresql" {
		return infra.Outputs.PostgresqlPrivateIps.Value
	} else if remoteService == "opensearch" {
		return infra.Outputs.OpensearchPrivateIps.Value
	} else if remoteService == "frontend" {
		return append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	}
	return []string{}
}

// This function will read the certificate paths, and then return the required certificates.
func (c *certRotateFlow) getCerts(infra *AutomteHAInfraDetails, sshFlagObj *sshFlag, certFlagsObj *certFlags, nodeFlagObj *nodeFlag) (string, string, string, string, string, error) {
	privateCertPath := certFlagsObj.privateCert
	publicCertPath := certFlagsObj.publicCert
	rootCaPath := certFlagsObj.rootCA
	adminCertPath := certFlagsObj.adminCert
	adminKeyPath := certFlagsObj.adminKey
	var rootCA, adminCert, adminKey []byte
	var err error

	const fileAccessErrorMsg string = "failed reading data from the given source, %s"

	if privateCertPath == "" || publicCertPath == "" {
		return "", "", "", "", "", errors.New("Please provide public and private cert paths")
	}

	privateCert, err := c.getCertFromFile(privateCertPath, infra)
	if err != nil {
		return "", "", "", "", "", status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf(fileAccessErrorMsg, err.Error()),
		)
	}

	publicCert, err := c.getCertFromFile(publicCertPath, infra)
	if err != nil {
		return "", "", "", "", "", status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf(fileAccessErrorMsg, err.Error()),
		)
	}

	// Root CA is mandatory for A2, PG and OS nodes. But root CA is ignored when node flag is provided
	if sshFlagObj.automate || sshFlagObj.postgres || sshFlagObj.opensearch {
		if rootCaPath == "" && nodeFlagObj.node == "" {
			return "", "", "", "", "", errors.New("Please provide rootCA path")
		}
		if rootCaPath != "" {
			rootCA, err = c.getCertFromFile(rootCaPath, infra)
			if err != nil {
				return "", "", "", "", "", status.Wrap(
					err,
					status.FileAccessError,
					fmt.Sprintf(fileAccessErrorMsg, err.Error()),
				)
			}
		}
	}

	// Admin Cert and Admin Key is mandatory for OS nodes.
	if sshFlagObj.opensearch {
		if (adminCertPath == "" || adminKeyPath == "") && nodeFlagObj.node == "" {
			return "", "", "", "", "", errors.New("Please provide Admin cert and Admin key paths")
		}
		if adminCertPath != "" && adminKeyPath != "" {
			adminCert, err = c.getCertFromFile(adminCertPath, infra)
			if err != nil {
				return "", "", "", "", "", status.Wrap(
					err,
					status.FileAccessError,
					fmt.Sprintf(fileAccessErrorMsg, err.Error()),
				)
			}

			adminKey, err = c.getCertFromFile(adminKeyPath, infra)
			if err != nil {
				return "", "", "", "", "", status.Wrap(
					err,
					status.FileAccessError,
					fmt.Sprintf(fileAccessErrorMsg, err.Error()),
				)
			}
		}
	}

	return string(rootCA), string(publicCert), string(privateCert), string(adminCert), string(adminKey), nil
}

// This function will read the certificate from the given path (local or remote).
func (c *certRotateFlow) getCertFromFile(certPath string, infra *AutomteHAInfraDetails) ([]byte, error) {
	certPath = strings.TrimSpace(certPath)
	// Checking if the given path is remote or local.
	if c.IsRemotePath(certPath) {
		remoteFilePath, fileName, hostIP, err := c.GetRemoteFileDetails(certPath)
		if err != nil {
			return nil, err
		}
		// Download certificate from remote host.
		sshConfig := c.getSshDetails(infra)
		sshConfig.hostIP = hostIP
		sshUtil := NewSSHUtil(sshConfig)

		filePath, err := sshUtil.copyFileFromRemote(remoteFilePath, fileName)
		if err != nil {
			return nil, errors.New(fmt.Sprintf("Unable to copy file from remote path: %v", certPath))
		}
		defer os.Remove(filePath)
		return c.FileUtils.ReadFile(filePath)
	}
	return c.FileUtils.ReadFile(certPath)
}

// Get the remote file details from path.
func (c *certRotateFlow) GetRemoteFileDetails(remotePath string) (string, string, string, error) {
	// Get Host IP from the given path and validate it.
	hostIP := c.GetIPV4(remotePath)
	if net.ParseIP(hostIP).To4() == nil {
		return "", "", "", errors.New(fmt.Sprintf("%v is not a valid IPv4 address", hostIP))
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

// Path should be in this format <IPv4>:<PathToFile>
// Example, 10.1.0.234:/home/ec2-user/certs/public.pem
func (c *certRotateFlow) IsRemotePath(path string) bool {
	pattern := regexp.MustCompile(`^` + IP_V4_REGEX + `:`)
	return pattern.MatchString(path)
}

func (c *certRotateFlow) GetIPV4(path string) string {
	pattern := regexp.MustCompile(IP_V4_REGEX)
	return pattern.FindString(path)
}

/*
	If we are working on backend service, then first we have to get the applied configurations

and then merge it with new configurations, then apply that configuration.
Because if we directly apply the new configurations, then the old applied configurations will be gone.
So, we have to retain the old configurations also.
This function will create the new toml file which includes old and new configurations.
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
	if remoteType == "opensearch" {
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
