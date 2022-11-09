package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
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
}

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
	fileName := "cert-rotate.toml"
	timestamp := time.Now().Format("20060102150405")

	rootCA, publicCert, privateCert, adminCert, adminKey, err := getCerts()
	if err != nil {
		log.Fatal(err)
	}

	f, err := os.Create(fileName)
	if err != nil {
		log.Fatal(err)
	}

	if isA2HARBFileExist() {

		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		sshUser := infra.Outputs.SSHUser.Value
		sskKeyFile := infra.Outputs.SSHKeyFile.Value
		sshPort := infra.Outputs.SSHPort.Value

		if sshFlag.automate || sshFlag.chefserver {
			// Writing the required configurations in the toml file
			config := fmt.Sprintf(FRONTEND_CONFIG, string(publicCert), string(privateCert), string(publicCert), string(privateCert))
			_, err = f.Write([]byte(config))
			if err != nil {
				log.Fatal(err)
			}
			f.Close()

			var frontendIps []string
			var remoteService string
			if sshFlag.automate {
				frontendIps = infra.Outputs.AutomatePrivateIps.Value
				remoteService = "automate"
			} else if sshFlag.chefserver {
				frontendIps = infra.Outputs.ChefServerPrivateIps.Value
				remoteService = "chefserver"
			}
			if len(frontendIps) == 0 {
				return errors.New(fmt.Sprintf("No %s Ips found", remoteService))
			}

			// Defining set of commands which run on frontend nodes
			scriptCommands := fmt.Sprintf(FRONTEND_COMMANDS, remoteService+timestamp, dateFormat)
			err := copyAndExecute(frontendIps, sshUser, sshPort, sskKeyFile, timestamp, remoteService, fileName, scriptCommands)
			if err != nil {
				log.Fatal(err)
			}

		} else if sshFlag.postgres {
			// Writing the required configurations in the toml file
			config := fmt.Sprintf(POSTGRES_CONFIG, string(privateCert), string(publicCert), string(rootCA))
			_, err = f.Write([]byte(config))
			if err != nil {
				log.Fatal(err)
			}
			f.Close()

			postgresIps := infra.Outputs.PostgresqlPrivateIps.Value
			if len(postgresIps) == 0 {
				return errors.New("No Postgres IPs are found")
			}
			remoteService := "postgresql"

			// Defining set of commands which run on postgres nodes
			scriptCommands := fmt.Sprintf(COPY_USER_CONFIG, remoteService+timestamp, remoteService)
			err = copyAndExecute(postgresIps, sshUser, sshPort, sskKeyFile, timestamp, remoteService, fileName, scriptCommands)
			if err != nil {
				log.Fatal(err)
			}

			// Patching root-ca to frontend-nodes
			filename_fe := "pg_fe.toml"
			// Creating and Writing the required configurations in the toml file
			config_fe := fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, string(rootCA))
			fe, err := os.Create(filename_fe)
			if err != nil {
				log.Fatal(err)
			}
			_, err = fe.Write([]byte(config_fe))
			if err != nil {
				log.Fatal(err)
			}
			fe.Close()

			frontendIps := append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
			if len(frontendIps) == 0 {
				return errors.New("No frontend IPs are found")
			}
			remoteService = "frontend"

			// Defining set of commands which run on frontend nodes
			scriptCommands = fmt.Sprintf(FRONTEND_COMMANDS, remoteService+timestamp, dateFormat)
			err = copyAndExecute(frontendIps, sshUser, sshPort, sskKeyFile, timestamp, remoteService, filename_fe, scriptCommands)
			if err != nil {
				log.Fatal(err)
			}

		} else if sshFlag.opensearch {
			e := existingInfra{}
			admin_dn, err := e.getDistinguishedNameFromKey(string(adminCert))
			if err != nil {
				return err
			}
			nodes_dn, err := e.getDistinguishedNameFromKey(string(publicCert))
			if err != nil {
				return err
			}

			// Writing the required configurations in the toml file
			config := fmt.Sprintf(OPENSEARCH_CONFIG, string(rootCA), string(adminCert), string(adminKey), string(publicCert), string(privateCert), fmt.Sprintf("%v", admin_dn), fmt.Sprintf("%v", nodes_dn))
			_, err = f.Write([]byte(config))
			if err != nil {
				log.Fatal(err)
			}
			f.Close()

			opensearchIps := infra.Outputs.OpensearchPrivateIps.Value
			if len(opensearchIps) == 0 {
				return errors.New("No Opensearch IPs are found")
			}
			remoteService := "opensearch"

			// Defining set of commands which run on opensearch nodes
			scriptCommands := fmt.Sprintf(COPY_USER_CONFIG, remoteService+timestamp, remoteService)
			err = copyAndExecute(opensearchIps, sshUser, sshPort, sskKeyFile, timestamp, remoteService, fileName, scriptCommands)
			if err != nil {
				log.Fatal(err)
			}

			// Patching root-ca to frontend-nodes
			cn := nodes_dn.CommonName
			filename_fe := "os_fe.toml"
			// Creating and Writing the required configurations in the toml file
			config_fe := fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, string(rootCA), cn)
			fe, err := os.Create(filename_fe)
			if err != nil {
				log.Fatal(err)
			}
			_, err = fe.Write([]byte(config_fe))
			if err != nil {
				log.Fatal(err)
			}
			fe.Close()

			frontendIps := append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
			if len(frontendIps) == 0 {
				return errors.New("No frontend IPs are found")
			}
			remoteService = "frontend"

			// Defining set of commands which run on frontend nodes
			scriptCommands = fmt.Sprintf(FRONTEND_COMMANDS, remoteService+timestamp, dateFormat)
			err = copyAndExecute(frontendIps, sshUser, sshPort, sskKeyFile, timestamp, remoteService, filename_fe, scriptCommands)
			if err != nil {
				log.Fatal(err)
			}
		}
	}
	return nil
}

// This function will copy the toml file to each required node and then execute the set of commands.
func copyAndExecute(ips []string, sshUser string, sshPort string, sskKeyFile string, timestamp string, remoteService string, fileName string, scriptCommands string) error {

	var err error
	for i := 0; i < len(ips); i++ {
		if (sshFlag.postgres || sshFlag.opensearch) && remoteService != "frontend" {
			tomlFilePath, err := getMerger(fileName, timestamp, remoteService, GET_USER_CONFIG, sshUser, sshPort, sskKeyFile, ips[i])
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

		fmt.Printf("Started Applying the Configurations in %s node", remoteService)
		output, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, ips[i], scriptCommands)
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}
		writer.Printf(output + "\n")
	}
	return nil
}

// This function will read the certificate paths, and then return the required certificates.
func getCerts() (string, string, string, string, string, error) {
	privateCertPath := certFlags.privateCert
	publicCertPath := certFlags.publicCert
	rootCaPath := certFlags.rootCA
	adminCertPath := certFlags.adminCert
	adminKeyPath := certFlags.adminKey
	var rootCA, adminCert, adminKey []byte
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

	// Root CA is mandatory for PG and OS nodes.
	if sshFlag.postgres || sshFlag.opensearch {
		if rootCaPath == "" {
			return "", "", "", "", "", errors.New("Please provide rootCA path")
		}
		rootCA, err = ioutil.ReadFile(rootCaPath) // nosemgrep
		if err != nil {
			return "", "", "", "", "", status.Wrap(
				err,
				status.FileAccessError,
				fmt.Sprintf("failed reading data from file: %s", err.Error()),
			)
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
	return string(rootCA), string(publicCert), string(privateCert), string(adminCert), string(adminKey), nil
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
