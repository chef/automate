package main

import (
	"bytes"
	"io/ioutil"
	"net"
	"strings"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"
)

var cleanupFlags = struct {
	onprem bool
}{}

type SshClient struct {
	sshUser    string
	sshPort    string
	sshKeyFile string
}

func init() {
	RootCmd.AddCommand(cleanupCmd)
	cleanupCmd.PersistentFlags().BoolVar(&cleanupFlags.onprem, "onprem", false, "Cleaning up all the instances related to onprem ")

}

var cleanupCmd = &cobra.Command{
	Use:   "cleanup",
	Short: "cleanup the Automate HA instances",
	Long:  "cleaning up the instance of all the Automate HA related Applications.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE:   runCleanupCmd,
	Hidden: true,
}

const (
	FRONTENDCLEANUP_COMMANDS = `
		sudo systemctl stop chef-automate;
		sudo rm -rf /hab;
		sudo rm -rf /var/automate-ha;
		`

	BACKENDCLEANUP_COMMANDS = `
		sudo systemctl stop hab-sup;
		sudo rm -rf /hab; 
		sudo rm -rf /var/automate-ha;
		`
)

func runCleanupCmd(cmd *cobra.Command, args []string) error {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	if infra != nil {
		sshInfo := SshClient{
			sshUser:    infra.Outputs.SSHUser.Value,
			sshPort:    infra.Outputs.SSHPort.Value,
			sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		}

		writer.Printf(strings.Join(args, ""))
		if isA2HARBFileExist() {
			if cleanupFlags.onprem {
				automateIps := infra.Outputs.AutomatePrivateIps.Value
				chefserverIps := infra.Outputs.ChefServerPrivateIps.Value
				postgresqlIps := infra.Outputs.PostgresqlPrivateIps.Value
				opensearchIps := infra.Outputs.OpensearchPrivateIps.Value
				for i := 0; i < len(automateIps); i++ {
					servername := "Automate"
					err := executeCleanupOnRemote(sshInfo, automateIps[i], servername, FRONTENDCLEANUP_COMMANDS)
					if err != nil {
						writer.Errorf("%s", err.Error())
						return err
					}
				}
				for i := 0; i < len(chefserverIps); i++ {
					servername := "chef server"
					err := executeCleanupOnRemote(sshInfo, chefserverIps[i], servername, FRONTENDCLEANUP_COMMANDS)
					if err != nil {
						writer.Error(err.Error())
						return err
					}
				}
				for i := 0; i < len(postgresqlIps); i++ {
					servername := "postgresql"
					err := executeCleanupOnRemote(sshInfo, postgresqlIps[i], servername, BACKENDCLEANUP_COMMANDS)
					if err != nil {
						writer.Error(err.Error())
						return err
					}
				}
				for i := 0; i < len(opensearchIps); i++ {
					servername := "opensearch"
					err := executeCleanupOnRemote(sshInfo, opensearchIps[i], servername, BACKENDCLEANUP_COMMANDS)
					if err != nil {
						writer.Error(err.Error())
						return err
					}
				}
				cleanUpScript := "hab pkg uninstall chef/automate-ha-deployment"
				args := []string{
					"-c",
					cleanUpScript,
				}
				err := executeCommand("/bin/sh", args, "")
				if err != nil {
					return err
				}
			}
		}
	} else {
		writer.Println("\nCleanup cannot be done")
	}
	return nil
}

func executeCleanupOnRemote(sshDetails SshClient, ip string, servername string, commands string) error {
	// read private key file
	pemBytes, err := ioutil.ReadFile(sshDetails.sshKeyFile) // nosemgrep
	if err != nil {
		writer.Errorf("Unable to read private key file for ssh connection: %s", err.Error())
		return err
	}
	// create signer
	signer, err := ssh.ParsePrivateKey(pemBytes)
	if err != nil {
		writer.Errorf("Unable to parse private key for ssh connection: %s", err.Error())
		return err
	}
	var (
		keyErr *knownhosts.KeyError
	)
	// Client config
	config := &ssh.ClientConfig{
		User: sshDetails.sshUser,
		Auth: []ssh.AuthMethod{ssh.PublicKeys(signer)},
		HostKeyCallback: ssh.HostKeyCallback(func(host string, remote net.Addr, pubKey ssh.PublicKey) error {
			kh := checkKnownHosts()
			hErr := kh(host, remote, pubKey)
			if errors.As(hErr, &keyErr) && len(keyErr.Want) > 0 {
				// Reference: https://www.godoc.org/golang.org/x/crypto/ssh/knownhosts#KeyError
				// if keyErr.Want slice is empty then host is unknown, if keyErr.Want is not empty
				// and if host is known then there is key mismatch the connection is then rejected.
				writer.Printf("WARNING: Given hostkeystring is not a key of %s, either a MiTM attack or %s has reconfigured the host pub key.", host, host)
				return keyErr
			} else if errors.As(hErr, &keyErr) && len(keyErr.Want) == 0 {
				// host key not found in known_hosts then give a warning and continue to connect.
				return addHostKey(host, remote, pubKey)
			}
			return nil
		}),
	}
	conn, err := ssh.Dial("tcp", net.JoinHostPort(ip, sshDetails.sshPort), config)
	if err != nil {
		writer.Errorf("dial failed:%s", err.Error())
		return err
	}
	defer conn.Close()
	// open session
	session, err := conn.NewSession()
	if err != nil {
		writer.Errorf("session failed:%s", err.Error())
		return err
	}
	var stdoutBuf bytes.Buffer
	session.Stdout = &stdoutBuf
	writer.Println("cleaning up the " + servername + " node on IP: " + ip + "\n")
	err = session.Run(commands)
	if err != nil {
		writer.Errorf("\nRun failed:%v", err)
		return err
	} else {
		writer.Success("Cleanup successful...\n")
	}
	defer session.Close()
	return err
}
