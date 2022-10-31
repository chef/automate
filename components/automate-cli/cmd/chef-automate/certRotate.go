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
}{}

var sshFlag = struct {
	automate   bool
	chefserver bool
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

	certRotateCmd.PersistentFlags().StringVar(&certFlags.privateCert, "private-cert", "", "Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.publicCert, "public-cert", "", "Public certificate")
}

const (
	FRONTEND_CONFIG = `
	[[load_balancer.v1.sys.frontend_tls]]
		cert = """%v"""
		key = """%v"""
	[[global.v1.frontend_tls]]
		cert = """%v"""
		key = """%v"""`
)

func certRotate(cmd *cobra.Command, args []string) error {
	privateCertPath := certFlags.privateCert
	publicCertPath := certFlags.publicCert
	fileName := "cert-rotate.toml"
	timestamp := time.Now().Format("20060102150405")

	if privateCertPath == "" || publicCertPath == "" {
		return errors.New("Please provide public and private cert paths")
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

		}
	}
	return nil
}
