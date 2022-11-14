package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var certFlags = struct {
	privateCert string
	publicCert  string
	rootCA      string
	adminCert   string
	adminKey    string
}{}

var nodeFlag = struct {
	node string
}{}

var sshFlag = struct {
	automate   bool
	chefserver bool
	postgres   bool
	opensearch bool
}{}

var certRotateCmd = &cobra.Command{
	Use:   "cert-rotate",
	Short: "Chef Automate rotate cert",
	Long:  "Chef Automate CLI command to rotate certificates",
	RunE:  certRotate,
}

func init() {
	RootCmd.AddCommand(certRotateCmd)

	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.automate, "automate", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.automate, "a2", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.chefserver, "chefserver", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.chefserver, "cs", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.postgres, "postgres", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.postgres, "pg", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.opensearch, "opensearch", false, "OS Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.opensearch, "os", false, "OS Certificate Rotation")

	certRotateCmd.PersistentFlags().StringVar(&certFlags.privateCert, "private-cert", "", "Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.publicCert, "public-cert", "", "Public certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.rootCA, "root-ca", "", "RootCA certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.adminCert, "admin-cert", "", "Admin certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.adminKey, "admin-key", "", "Admin Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&nodeFlag.node, "node", "", "Node Ip address")
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
		ssl_cert = """%v"""
		`

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

	OPENSEARCH_FRONTEND_CONFIG = `
	[global.v1.external.opensearch.ssl]
		root_cert = """%v"""
		server_name = "%v"`

	GET_USER_CONFIG = `
	sudo cat /hab/user/automate-ha-%s/config/user.toml`

	COPY_USER_CONFIG = `
	sudo systemctl stop hab-sup.service
	echo "y" | sudo cp /tmp/%s /hab/user/automate-ha-%s/config/user.toml
	sudo systemctl start hab-sup.service`
)

// This function will rotate the certificates of Automate, Chef Infra Server, Postgres and Opensearch.
func certRotate(cmd *cobra.Command, args []string) error {
	rootCA, publicCert, privateCert, adminCert, adminKey, err := getCerts()
	if err != nil {
		log.Fatal(err)
	}

	if isA2HARBFileExist() {
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}

		if rootCA != "" && nodeFlag.node != "" {
			writer.Warn("root-ca flag will be ignored when node flag is provided")
		}

		if sshFlag.automate || sshFlag.chefserver {
			err := certRotateFrontend(publicCert, privateCert, rootCA, infra)
			if err != nil {
				log.Fatal(err)
			}
		} else if sshFlag.postgres {
			err := certRotatePG(publicCert, privateCert, rootCA, infra)
			if err != nil {
				log.Fatal(err)
			}
		} else if sshFlag.opensearch {
			err := certRotateOS(publicCert, privateCert, rootCA, adminCert, adminKey, infra)
			if err != nil {
				log.Fatal(err)
			}
		}
	}
	return nil
}

// This function will rotate the certificates of Automate and Chef Infra Server,
func certRotateFrontend(publicCert, privateCert, rootCA string, infra *AutomteHAInfraDetails) error {
	fileName := "cert-rotate-fe.toml"
	timestamp := time.Now().Format("20060102150405")
	var remoteService string

	if sshFlag.automate {
		remoteService = "automate"
	} else if sshFlag.chefserver {
		remoteService = "chefserver"
	}
	// Creating and patching the required configurations.
	config := fmt.Sprintf(FRONTEND_CONFIG, publicCert, privateCert, publicCert, privateCert)
	err := patchConfig(config, fileName, timestamp, remoteService, infra)
	if err != nil {
		log.Fatal(err)
	}
	// If we pass root-ca in automate then we also need to update root-ca in the ChefServer to maintain the connection
	if sshFlag.automate {
		fileName = "rotate-root_CA.toml"
		remoteService = "chefserver"
		sshUser, sskKeyFile, sshPort := getSshDetails(infra)

		cmd := `sudo chef-automate config show | grep fqdn | awk '{print $3}' | tr -d '"'`
		ips := getIps(remoteService, infra)
		if len(ips) == 0 {
			return errors.New(fmt.Sprintf("No %s IPs are found", remoteService))
		}
		fqdn, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, ips[0], cmd)
		if err != nil {
			log.Fatal(err)
		}

		config = fmt.Sprintf(CHEFSERVER_ROOTCA_CONFIG, strings.TrimSpace(string(fqdn)), rootCA)
		err = patchConfig(config, fileName, timestamp, remoteService, infra)
		if err != nil {
			log.Fatal(err)
		}
	}
	return nil
}

// This function will rotate the certificates of Postgres
func certRotatePG(publicCert, privateCert, rootCA string, infra *AutomteHAInfraDetails) error {
	if isManagedServicesOn() {
		return errors.New("You can not rotate certs for AWS managed services")
	}
	fileName := "cert-rotate-pg.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := "postgresql"

	// Creating and patching the required configurations.
	var config string
	if nodeFlag.node != "" {
		config = fmt.Sprintf(POSTGRES_CONFIG_IGNORE_ISSUER_CERT, privateCert, publicCert)
	} else {
		config = fmt.Sprintf(POSTGRES_CONFIG, privateCert, publicCert, rootCA)
	}

	err := patchConfig(config, fileName, timestamp, remoteService, infra)
	if err != nil {
		log.Fatal(err)
	}

	// ignore patching of root-ca when node flag is provided
	if nodeFlag.node != "" {
		return nil
	}
	// Patching root-ca to frontend-nodes for maintaining the connection.
	filename_fe := "pg_fe.toml"
	remoteService = "frontend"
	// Creating and patching the required configurations.
	config_fe := fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, rootCA)
	err = patchConfig(config_fe, filename_fe, timestamp, remoteService, infra)
	if err != nil {
		log.Fatal(err)
	}
	return nil
}

// This function will rotate the certificates of OpenSearch
func certRotateOS(publicCert, privateCert, rootCA, adminCert, adminKey string, infra *AutomteHAInfraDetails) error {
	if isManagedServicesOn() {
		return errors.New("You can not rotate certs for AWS managed services")
	}
	fileName := "cert-rotate-os.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := "opensearch"

	e := existingInfra{}
	admin_dn, err := e.getDistinguishedNameFromKey(adminCert)
	if err != nil {
		return err
	}
	nodes_dn, err := e.getDistinguishedNameFromKey(publicCert)
	if err != nil {
		return err
	}

	// Creating and patching the required configurations.
	config := fmt.Sprintf(OPENSEARCH_CONFIG, rootCA, adminCert, adminKey, publicCert, privateCert, fmt.Sprintf("%v", admin_dn), fmt.Sprintf("%v", nodes_dn))
	err = patchConfig(config, fileName, timestamp, remoteService, infra)
	if err != nil {
		log.Fatal(err)
	}

	// Patching root-ca to frontend-nodes for maintaining the connection.
	cn := nodes_dn.CommonName
	filename_fe := "os_fe.toml"
	remoteService = "frontend"
	// Creating and patching the required configurations.
	config_fe := fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, rootCA, cn)
	err = patchConfig(config_fe, filename_fe, timestamp, remoteService, infra)
	if err != nil {
		log.Fatal(err)
	}
	return nil
}

// This function will patch the configurations to required nodes.
func patchConfig(config, filename, timestamp, remoteService string, infra *AutomteHAInfraDetails) error {
	sshUser, sskKeyFile, sshPort := getSshDetails(infra)
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
	if nodeFlag.node != "" {
		isValid := validateEachIp(remoteService, infra)
		if !isValid {
			return errors.New(fmt.Sprintf("Please Enter Valid %s IP", remoteService))
		}
		ips = append(ips, nodeFlag.node)
	} else {
		ips = getIps(remoteService, infra)
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
	err = copyAndExecute(ips, sshUser, sshPort, sskKeyFile, timestamp, remoteService, filename, scriptCommands)
	if err != nil {
		log.Fatal(err)
	}
	return nil
}

// This function will copy the toml file to each required node and then execute the set of commands.
func copyAndExecute(ips []string, sshUser string, sshPort string, sskKeyFile string, timestamp string, remoteService string, fileName string, scriptCommands string) error {

	var err error
	var tomlFilePath string
	for i := 0; i < len(ips); i++ {
		if (sshFlag.postgres || sshFlag.opensearch) && remoteService != "frontend" {
			tomlFilePath, err = getMerger(fileName, timestamp, remoteService, GET_USER_CONFIG, sshUser, sshPort, sskKeyFile, ips[i])
			if err != nil {
				return err
			}
			// Copying the new toml file which includes both old and new configurations (for backend nodes).
			err = copyFileToRemote(sskKeyFile, tomlFilePath, sshUser, ips[i], remoteService+timestamp, false)
		} else {
			// Copying the new toml file which includes new configurations (for frontend nodes).
			err = copyFileToRemote(sskKeyFile, fileName, sshUser, ips[i], remoteService+timestamp, false)
		}
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}

		fmt.Printf("Started Applying the Configurations in %s node: %s", remoteService, ips[i])
		output, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, ips[i], scriptCommands)
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}
		writer.Printf(output + "\n")
	}
	return nil
}

func validateEachIp(remoteService string, infra *AutomteHAInfraDetails) bool {
	ips := getIps(remoteService, infra)
	for i := 0; i < len(ips); i++ {
		if ips[i] == nodeFlag.node {
			return true
		}
	}
	return false
}

// This function will return the SSH details.
func getSshDetails(infra *AutomteHAInfraDetails) (string, string, string) {
	return infra.Outputs.SSHUser.Value, infra.Outputs.SSHKeyFile.Value, infra.Outputs.SSHPort.Value
}

// This function will return the Ips based on the given remote service.
func getIps(remoteService string, infra *AutomteHAInfraDetails) []string {
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
func getCerts() (string, string, string, string, string, error) {
	privateCertPath := certFlags.privateCert
	publicCertPath := certFlags.publicCert
	rootCaPath := certFlags.rootCA
	adminCertPath := certFlags.adminCert
	adminKeyPath := certFlags.adminKey
	var rootCABytes, adminCert, adminKey []byte
	var rootCA string
	var err error

	if privateCertPath == "" || publicCertPath == "" {
		return "", "", "", "", "", errors.New("Please provide public and private cert paths")
	}

	privateCert, err := ioutil.ReadFile(privateCertPath) // nosemgrep
	if err != nil {
		return "", "", "", "", "", status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf("failed reading data from file: %s", err.Error()),
		)
	}

	publicCert, err := ioutil.ReadFile(publicCertPath) // nosemgrep
	if err != nil {
		return "", "", "", "", "", status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf("failed reading data from file: %s", err.Error()),
		)
	}

	// Root CA is mandatory for A2, PG and OS nodes. But root CA is ignored when node flag is provided
	if sshFlag.automate || sshFlag.postgres || sshFlag.opensearch {
		if rootCaPath == "" && nodeFlag.node == "" {
			return "", "", "", "", "", errors.New("Please provide rootCA path")
		}
		if rootCaPath != "" {
			rootCABytes, err = ioutil.ReadFile(rootCaPath) // nosemgrep
			rootCA = string(rootCABytes)
			if err != nil {
				return "", "", "", "", "", status.Wrap(
					err,
					status.FileAccessError,
					fmt.Sprintf("failed reading data from file: %s", err.Error()),
				)
			}
		}
	}

	// Admin Cert and Admin Key is mandatory for OS nodes.
	if sshFlag.opensearch {
		if adminCertPath == "" || adminKeyPath == "" {
			return "", "", "", "", "", errors.New("Please provide Admin cert and Admin key paths")
		}
		adminCert, err = ioutil.ReadFile(adminCertPath) // nosemgrep
		if err != nil {
			return "", "", "", "", "", status.Wrap(
				err,
				status.FileAccessError,
				fmt.Sprintf("failed reading data from file: %s", err.Error()),
			)
		}

		adminKey, err = ioutil.ReadFile(adminKeyPath) // nosemgrep
		if err != nil {
			return "", "", "", "", "", status.Wrap(
				err,
				status.FileAccessError,
				fmt.Sprintf("failed reading data from file: %s", err.Error()),
			)
		}
	}

	return rootCA, string(publicCert), string(privateCert), string(adminCert), string(adminKey), nil
}

/* If we are working on backend service, then first we have to get the applied configurations
and then merge it with new configurations, then apply that configuration.
Because if we directly apply the new configurations, then the old applied configurations will be gone.
So, we have to retain the old configurations also.

This function will create the new toml file which includes old and new configurations.*/
func getMerger(fileName string, timestamp string, remoteType string, config string, sshUser string, sshPort string, sskKeyFile string, remoteIP string) (string, error) {
	tomlFile := fileName + timestamp
	scriptCommands := fmt.Sprintf(config, remoteType)
	rawOutput, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIP, scriptCommands)
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
