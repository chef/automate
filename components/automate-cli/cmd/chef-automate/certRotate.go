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
}{}

var ipFlag = struct {
	postgresIp string
}{}

var sshFlag = struct {
	automate   bool
	chefserver bool
	postgres   bool
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

	certRotateCmd.PersistentFlags().StringVar(&certFlags.privateCert, "private-cert", "", "Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.publicCert, "public-cert", "", "Public certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.rootCA, "root-ca", "", "RootCA certificate")
	certRotateCmd.PersistentFlags().StringVar(&ipFlag.postgresIp, "ip", "", "Postgres Node IP")
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

	GET_USER_CONFIG = `
	sudo cat /hab/user/automate-ha-%s/config/user.toml`

	COPY_USER_CONFIG = `
	echo "y" | sudo cp /tmp/%s /hab/user/automate-ha-%s/config/user.toml`
)

func certRotate(cmd *cobra.Command, args []string) error {
	privateCertPath := certFlags.privateCert
	publicCertPath := certFlags.publicCert
	rootCaPath := certFlags.rootCA
	fileName := "cert-rotate.toml"
	timestamp := time.Now().Format("20060102150405")

	if privateCertPath == "" || publicCertPath == "" {
		return errors.New("Please provide public and private cert paths")
	}
	var rootCA []byte
	var err error
	if sshFlag.postgres {
		if rootCaPath == "" {
			return errors.New("Please provide rootCA path")
		}
		rootCA, err = ioutil.ReadFile(rootCaPath) // nosemgrep
		if err != nil {
			return status.Wrap(
				err,
				status.FileAccessError,
				fmt.Sprintf("failed reading data from file: %s", err.Error()),
			)
		}
	}

	privateCert, err := ioutil.ReadFile(privateCertPath) // nosemgrep
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf("failed reading data from file: %s", err.Error()),
		)
	}

	publicCert, err := ioutil.ReadFile(publicCertPath) // nosemgrep
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf("failed reading data from file: %s", err.Error()),
		)
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

			scriptCommands := fmt.Sprintf(FRONTEND_COMMANDS, remoteService+timestamp, dateFormat)
			for i := 0; i < len(frontendIps); i++ {
				err := copyFileToRemote(sskKeyFile, fileName, sshUser, frontendIps[i], remoteService+timestamp, false)
				if err != nil {
					writer.Errorf("%v", err)
					return err
				}
				output, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, frontendIps[i], scriptCommands)
				if err != nil {
					writer.Errorf("%v", err)
					return err
				}
				writer.Printf(output)
			}

		} else if sshFlag.postgres {
			config := fmt.Sprintf(POSTGRES_CONFIG, string(privateCert), string(publicCert), string(rootCA))
			_, err = f.Write([]byte(config))
			if err != nil {
				log.Fatal(err)
			}
			f.Close()

			remoteService := "postgresql"
			file := []string{fileName}
			tomlFilePath, err := getMergerTOMLPath(file, infra, timestamp, remoteService, GET_USER_CONFIG)
			if err != nil {
				return err
			}
			scriptCommands := fmt.Sprintf(COPY_USER_CONFIG, remoteService+timestamp, remoteService)

			var postgresIps []string
			if ipFlag.postgresIp != "" {
				postgresIps = append(postgresIps, ipFlag.postgresIp)

				//check for issuer_cert (root-ca)
				getPgConfigCmd := fmt.Sprintf(GET_CONFIG, remoteService)
				pgConfigRawOutput, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, ipFlag.postgresIp, getPgConfigCmd)
				if err != nil {
					return err
				}
				var pgConfig PostgresqlConfig
				if _, err := toml.Decode(cleanToml(pgConfigRawOutput), &pgConfig); err != nil {
					return err
				}

				if string(rootCA) != pgConfig.Ssl.IssuerCert {
					ok, err := writer.Confirm("apply root-ca to all nodes?")
					if err != nil || !ok {
						if !ok {
							err = errors.New("failed to update root-ca")
						}
						return err
					}
					postgresIps = infra.Outputs.PostgresqlPrivateIps.Value
				}
			} else {
				postgresIps = infra.Outputs.PostgresqlPrivateIps.Value
			}

			for i := 0; i < len(postgresIps); i++ {
				err := copyFileToRemote(sskKeyFile, tomlFilePath, sshUser, postgresIps[i], remoteService+timestamp, false)
				if err != nil {
					writer.Errorf("%v", err)
					return err
				}
				output, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, postgresIps[i], scriptCommands)
				if err != nil {
					writer.Errorf("%v", err)
					return err
				}
				writer.Printf(output)
			}

			// Patching root-ca to frontend-nodes
			filename_fe := "pg_fe.toml"
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

			scriptCommands = fmt.Sprintf(FRONTEND_COMMANDS, remoteService+timestamp, dateFormat)
			for i := 0; i < len(frontendIps); i++ {
				err := copyFileToRemote(sskKeyFile, filename_fe, sshUser, frontendIps[i], remoteService+timestamp, false)
				if err != nil {
					writer.Errorf("%v", err)
					return err
				}
				output, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, frontendIps[i], scriptCommands)
				if err != nil {
					writer.Errorf("%v", err)
					return err
				}
				writer.Printf(output)
			}
		}
	}
	return nil
}
