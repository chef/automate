package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var certFlags = struct {
	privateCert string
	publicCert  string
	rootCA      string
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
				err := copyFileToRemote(sskKeyFile, fileName, sshUser, frontendIps[i], remoteService+timestamp)
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
			tomlFilePath, err := getMergerTOMLPath(file, infra, timestamp, remoteService)
			if err != nil {
				return err
			}

			scriptCommands := fmt.Sprintf(BACKEND_COMMAND, dateFormat, remoteService, dateFormat, remoteService+timestamp)
			if len(infra.Outputs.PostgresqlPrivateIps.Value) > 0 {
				remoteIp := infra.Outputs.PostgresqlPrivateIps.Value[0]
				err := copyFileToRemote(sskKeyFile, tomlFilePath, sshUser, remoteIp, remoteService+timestamp)
				if err != nil {
					writer.Errorf("%v", err)
					return err
				}
				output, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIp, scriptCommands)
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
				err := copyFileToRemote(sskKeyFile, filename_fe, sshUser, frontendIps[i], remoteService+timestamp)
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
