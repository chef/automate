package sshutils

import (
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"os"
	"path/filepath"
	"strings"

	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"
)

type SSHConfig struct {
	SshUser    string
	SshPort    string
	SshKeyFile string
	HostIP     string
	Timeout    int
}

type SSHUtilImpl struct {
	SshConfig *SSHConfig
	logger    logger.Logger
	Ssh       *SshPkgDeps
}

type SshPkgDeps struct {
	Dial            func(network string, addr string, config *ssh.ClientConfig) (*ssh.Client, error)
	HostKeyCallback func(func(hostname string, remote net.Addr, key ssh.PublicKey) error)
	ParsePrivateKey func(pemBytes []byte) (ssh.Signer, error)
	NewSession      func() (*ssh.Session, error)
	PublicKey       func(signers ...ssh.Signer) ssh.AuthMethod
	Normalize       func(address string) string
	New             func(files ...string) (ssh.HostKeyCallback, error)
}

type SSHUtil interface {
	getClientConfig() (*ssh.ClientConfig, error)
	GetSSHConfig() *SSHConfig
	SetSSHConfig(sshConfig *SSHConfig)
	GetConnection() (*ssh.Client, error)
	ConnectAndExecuteCommandOnRemoteWithSudoPassword(*SSHConfig, string, string) (bool, error)
	checkKnownHosts() (ssh.HostKeyCallback, error)
	createKnownHosts()
	addHostKey(string, net.Addr, ssh.PublicKey) error
}

func NewSshPkgDeps() *SshPkgDeps {
	return &SshPkgDeps{
		Dial:            ssh.Dial,
		ParsePrivateKey: ssh.ParsePrivateKey,
		PublicKey:       ssh.PublicKeys,
		NewSession:      ssh.NewSession(),
		Normalize:       knownhosts.Normalize,
		New:             knownhosts.New,
	}
}

func NewSSHUtil(sshconfig *SSHConfig, ssh *SshPkgDeps, logger logger.Logger) SSHUtil {
	// Check if timeout is set, if not set it to default value.
	checkTimeout(sshconfig)
	return &SSHUtilImpl{
		SshConfig: sshconfig,
		logger:    logger,
		Ssh:       ssh,
	}
}

func checkTimeout(sshConfig *SSHConfig) {
	if sshConfig.Timeout == 0 {
		sshConfig.Timeout = 150
	}
}

func (s *SSHUtilImpl) GetSSHConfig() *SSHConfig {
	return s.SshConfig
}

func (s *SSHUtilImpl) SetSSHConfig(sshConfig *SSHConfig) {
	// Check if timeout is set, if not set it to default value.
	checkTimeout(sshConfig)
	s.SshConfig = sshConfig
}

func (s *SSHUtilImpl) GetConnection() (*ssh.Client, error) {
	config, err := s.getClientConfig()
	if err != nil {
		return nil, err
	}
	log.Println("Config:", config)
	// Open connection
	addr := s.SshConfig.HostIP
	if len(strings.TrimSpace(s.SshConfig.SshPort)) > 0 {
		addr = addr + ":" + s.SshConfig.SshPort
	} else {
		addr = addr + ":22"
	}
	log.Println("Address:", addr)
	conn, err := s.Ssh.Dial("tcp", addr, config)
	if conn == nil || err != nil {
		log.Printf("dial failed:%v\n", err)
		return nil, err
	}
	log.Print("The connetion Details: ", conn)
	return conn, err
}

func (s *SSHUtilImpl) getClientConfig() (*ssh.ClientConfig, error) {
	pemBytes, err := ioutil.ReadFile(s.SshConfig.SshKeyFile) //nosemgrep
	if err != nil {
		s.logger.Error("Unable to read private key: %v", err)
		return nil, err
	}
	signer, err := ssh.ParsePrivateKey(pemBytes)
	if err != nil {
		s.logger.Errorf("Parsing key failed: %v", err)
		return nil, err
	}
	var (
		keyErr *knownhosts.KeyError
	)
	// Client config
	return &ssh.ClientConfig{
		User: s.SshConfig.SshUser,
		Auth: []ssh.AuthMethod{ssh.PublicKeys(signer)},
		HostKeyCallback: ssh.HostKeyCallback(func(host string, remote net.Addr, pubKey ssh.PublicKey) error {
			kh, err := s.checkKnownHosts()
			if err != nil {
				s.logger.Errorf("Error while getting the list of knows Host:", err)
			}
			hErr := kh(host, remote, pubKey)
			// Reference: https://blog.golang.org/go1.13-errors
			// To understand what errors.As is.
			if errors.As(hErr, &keyErr) && len(keyErr.Want) > 0 {
				// Reference: https://www.godoc.org/golang.org/x/crypto/ssh/knownhosts#KeyError
				// if keyErr.Want slice is empty then host is unknown, if keyErr.Want is not empty
				// and if host is known then there is key mismatch the connection is then rejected.
				s.logger.Printf("WARNING: Given hostkeystring is not a key of %s, either a MiTM attack or %s has reconfigured the host pub key.", host, host)
				return keyErr
			} else if errors.As(hErr, &keyErr) && len(keyErr.Want) == 0 {
				// host key not found in known_hosts then give a warning and continue to connect.
				// writer.Printf("WARNING: %s is not trusted, adding this key to known_hosts file.\n", host)
				return s.addHostKey(host, remote, pubKey)
			}
			// writer.Printf("Pub key exists for %s.\n", host)
			return nil
		}),
	}, nil
}

func (s *SSHUtilImpl) ConnectAndExecuteCommandOnRemoteWithSudoPassword(SSHConfig *SSHConfig, sudoPassword string, SUDO_PASSWORD_CMD string) (bool, error) {
	//Setting the SSH Config received
	s.SetSSHConfig(SSHConfig)

	// Creating a SSH connection with the Config provided
	conn, err := s.GetConnection()
	if err != nil {
		return false, err
	}
	// Creating a session for the cammand execution
	session, err := conn.NewSession()
	if err != nil {
		s.logger.Error("Session Creation failed:%v\n", err)
		return false, err
	}
	defer session.Close()
	//Ran the command using the sudoPassword provided
	remoteCommands := fmt.Sprintf(SUDO_PASSWORD_CMD, sudoPassword)
	output, err := session.CombinedOutput(remoteCommands)
	if err != nil {
		s.logger.Error("Error while running cammand:", err)
		return false, err
	}
	s.logger.Debug("The output from the session:", output)
	return true, nil
}

func (s *SSHUtilImpl) createKnownHosts() {
	f, fErr := os.OpenFile(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"), os.O_CREATE, 0600)
	if fErr != nil {
		s.logger.Errorf("%v", fErr)
		return
	}
	f.Close()
}

func (s *SSHUtilImpl) checkKnownHosts() (ssh.HostKeyCallback, error) {
	s.createKnownHosts()
	kh, e := s.Ssh.New(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"))
	if e != nil {
		s.logger.Error("%v", e)
		return nil, e
	}
	return kh, nil
}

func (s *SSHUtilImpl) addHostKey(host string, remote net.Addr, pubKey ssh.PublicKey) error {
	// add host key if host is not found in known_hosts, error object is return, if nil then connection proceeds,
	// if not nil then connection stops.
	khFilePath := filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts")

	f, fErr := os.OpenFile(khFilePath, os.O_APPEND|os.O_WRONLY, 0600)
	if fErr != nil {
		return fErr
	}
	defer f.Close()

	knownHosts := s.Ssh.Normalize(remote.String())
	_, fileErr := f.WriteString(knownhosts.Line([]string{knownHosts}, pubKey))
	f.WriteString("\n")
	return fileErr
}
