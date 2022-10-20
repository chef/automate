package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"
)

var certFlags = struct {
	privateCert string
	publicCert  string
}{}

var sshFlag = struct {
	automate bool
}{}

var certRotateCmd = &cobra.Command{
	Use:   "cert-rotate",
	Short: "Chef Automate rotate cert",
	Long:  "Chef Automate CLI command to rotate certificates",
	RunE:  certRotate,
}

const (
	FRONTEND_COMMANDS = `
	sudo chef-automate config patch /tmp/%s;
	export TIMESTAMP=$(date +'%s');
	sudo mv /etc/chef-automate/config.toml /etc/chef-automate/config.toml.$TIMESTAMP;
	sudo chef-automate config show > sudo /etc/chef-automate/config.toml`

	dateFormat = "%Y%m%d%H%M%S"
)

func init() {
	RootCmd.AddCommand(certRotateCmd)

	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.automate, "automate", false, "Automate ha server name to ssh")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.automate, "a2", false, "Automate ha server name to ssh")

	certRotateCmd.PersistentFlags().StringVar(&certFlags.privateCert, "private-cert", "", "Ha private certificate")
	certRotateCmd.PersistentFlags().StringVar(&certFlags.publicCert, "public-cert", "", "Ha public certificate")
}

func certRotate(cmd *cobra.Command, args []string) error {
	privateCertPath := certFlags.privateCert
	publicCertPath := certFlags.publicCert
	fileName := "cert.toml"

	if privateCertPath == "" || publicCertPath == "" {
		return errors.New("Please provide public and private cert paths")
	}
	privateCert, err := ioutil.ReadFile(privateCertPath) // nosemgrep
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf("failed reading data from file: %s", err),
		)
	}

	publicCert, err := ioutil.ReadFile(publicCertPath) // nosemgrep
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf("failed reading data from file: %s", err),
		)
	}

	f, err := os.Create(fileName)
	if err != nil {
		log.Fatal(err)
	}
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	sshUser := infra.Outputs.SSHUser.Value
	sskKeyFile := infra.Outputs.SSHKeyFile.Value
	sshPort := infra.Outputs.SSHPort.Value

	if isA2HARBFileExist() {
		if sshFlag.automate {
			config := fmt.Sprintf(`
			[[load_balancer.v1.sys.frontend_tls]]
				cert = """%v"""
				key = """%v"""
			[[global.v1.frontend_tls]]
				cert = """%v"""
				key = """%v"""
		`, string(publicCert), string(privateCert), string(publicCert), string(privateCert))

			_, err = f.Write([]byte(config))
			if err != nil {
				log.Fatal(err)
			}
			f.Close()
			automateIps := infra.Outputs.AutomatePrivateIps.Value
			scriptCommands := fmt.Sprintf(FRONTEND_COMMANDS, fileName, dateFormat)
			for i := 0; i < len(automateIps); i++ {
				connectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, automateIps[i], fileName, scriptCommands)
			}
		}
	}
	return nil
}

func connectAndExecuteCommandOnRemote(sshUser string, sshPort string, sshKeyFile string, hostIP string, tomlFilePath string, remoteCommands string) {

	pemBytes, err := ioutil.ReadFile(sshKeyFile) // nosemgrep
	if err != nil {
		writer.Errorf("Unable to read private key: %v", err)
		return
	}
	signer, err := ssh.ParsePrivateKey(pemBytes)
	if err != nil {
		writer.Errorf("Parsing key failed: %v", err)
		return
	}
	var (
		keyErr *knownhosts.KeyError
	)

	config := &ssh.ClientConfig{
		User: sshUser,
		Auth: []ssh.AuthMethod{ssh.PublicKeys(signer)},
		HostKeyCallback: ssh.HostKeyCallback(func(host string, remote net.Addr, pubKey ssh.PublicKey) error {
			kh := checkKnownHosts()
			hErr := kh(host, remote, pubKey)
			// Reference: https://blog.golang.org/go1.13-errors
			// To understand what errors.As is.
			if errors.As(hErr, &keyErr) && len(keyErr.Want) > 0 {
				// Reference: https://www.godoc.org/golang.org/x/crypto/ssh/knownhosts#KeyError
				// if keyErr.Want slice is empty then host is unknown, if keyErr.Want is not empty
				// and if host is known then there is key mismatch the connection is then rejected.
				writer.Printf("WARNING: Given hostkeystring is not a key of %s, either a MiTM attack or %s has reconfigured the host pub key.", host, host)
				return keyErr
			} else if errors.As(hErr, &keyErr) && len(keyErr.Want) == 0 {
				// host key not found in known_hosts then give a warning and continue to connect.
				writer.Printf("WARNING: %s is not trusted, adding this key to known_hosts file.", host)
				return addHostKey(host, remote, pubKey)
			}
			writer.Printf("Pub key exists for %s.", host)
			return nil
		}),
	}

	// Open connection
	conn, err := ssh.Dial("tcp", hostIP+":"+sshPort, config)
	if conn == nil || err != nil {
		writer.Errorf("dial failed:%v", err)
		return
	}
	defer conn.Close()

	// Open session
	session, err := conn.NewSession()
	if err != nil {
		writer.Errorf("session failed:%v", err)
		return
	}
	var stdoutBuf bytes.Buffer
	session.Stdout = &stdoutBuf

	cmd := "scp"
	exec_args := []string{"-i", sshKeyFile, "-r", tomlFilePath, sshUser + "@" + hostIP + ":/tmp/"}
	if err := exec.Command(cmd, exec_args...).Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}

	writer.StartSpinner()
	err = session.Run(remoteCommands)
	writer.StopSpinner()
	if err != nil {
		writer.Errorf("Run failed:%v", err)
		return
	}
	writer.Success("SCP successful...\n")

	defer session.Close()
	writer.Printf("\n%s\n", stdoutBuf.String())
}

func createKnownHosts() {
	f, fErr := os.OpenFile(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"), os.O_CREATE, 0600)
	if fErr != nil {
		writer.Errorf("%v", fErr)
		return
	}
	f.Close()
}

func checkKnownHosts() ssh.HostKeyCallback {
	createKnownHosts()
	kh, e := knownhosts.New(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"))
	if e != nil {
		writer.Errorf("%v", e)
		return nil
	}
	return kh
}

func addHostKey(host string, remote net.Addr, pubKey ssh.PublicKey) error {
	// add host key if host is not found in known_hosts, error object is return, if nil then connection proceeds,
	// if not nil then connection stops.
	khFilePath := filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts")

	f, fErr := os.OpenFile(khFilePath, os.O_APPEND|os.O_WRONLY, 0600)
	if fErr != nil {
		return fErr
	}
	defer f.Close()

	knownHosts := knownhosts.Normalize(remote.String())
	_, fileErr := f.WriteString(knownhosts.Line([]string{knownHosts}, pubKey))
	f.WriteString("/n")
	return fileErr
}
