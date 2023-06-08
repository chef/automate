package sshutils

import (
	"fmt"
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
	SshClient ISshClient
	logger    logger.Logger
}

type SSHUtil interface {
	getClientConfig() (*ssh.ClientConfig, error)
	SetSSHConfig(sshConfig *SSHConfig)
	GetConnection() (*ssh.Client, error)
	checkKnownHosts() (ssh.HostKeyCallback, error)
	createKnownHosts() error
	ConnectAndExecuteCommandOnRemoteWithSudoPassword(sshConfig *SSHConfig, sudoPassword string, sudoPasswordCmd string) (bool, error)
	addHostKey(string, net.Addr, ssh.PublicKey) error
}

func NewSSHUtil(sshconfig *SSHConfig, sshclient ISshClient, logger logger.Logger) *SSHUtilImpl {
	// Check if timeout is set, if not set it to default value.
	checkTimeout(sshconfig)
	return &SSHUtilImpl{
		SshConfig: sshconfig,
		SshClient: sshclient,
		logger:    logger,
	}
}

func checkTimeout(sshConfig *SSHConfig) {
	if sshConfig.Timeout == 0 {
		sshConfig.Timeout = 150
	}
}

func (s *SSHUtilImpl) SetSSHConfig(sshConfig *SSHConfig) {
	// Check if timeout is set, if not set it to default value.
	checkTimeout(sshConfig)
	s.SshConfig = sshConfig
}

func (s *SSHUtilImpl) GetConnection() (*ssh.Client, error) {
	config, err := s.getClientConfig()
	if err != nil {
		s.logger.Error("Error while generating the client config:", err)
		return nil, err
	}
	// Open connection
	addr := s.SshConfig.HostIP
	if len(strings.TrimSpace(s.SshConfig.SshPort)) > 0 {
		addr = addr + ":" + s.SshConfig.SshPort
	} else {
		addr = addr + ":22"
	}
	conn, err := s.SshClient.Dial("tcp", addr, config)
	if conn == nil || err != nil {
		s.logger.Error("dial failed:%v\n", err)
		return nil, err
	}
	return conn, err
}

func (s *SSHUtilImpl) getClientConfig() (*ssh.ClientConfig, error) {
	pemBytes, err := os.ReadFile(s.SshConfig.SshKeyFile)
	if err != nil {
		s.logger.Error("Unable to read private key: %v", err)
		return nil, err
	}
	signer, err := s.SshClient.ParsePrivateKey(pemBytes)
	if err != nil {
		s.logger.Errorf("Parsing key failed: %v", err)
		return nil, err
	}
	// Client config
	return &ssh.ClientConfig{
		User:            s.SshConfig.SshUser,
		Auth:            []ssh.AuthMethod{s.SshClient.PublicKey(signer)},
		HostKeyCallback: s.HostKeyCallback,
	}, nil
}
func (s *SSHUtilImpl) HostKeyCallback(host string, remote net.Addr, pubkey ssh.PublicKey) error {
	var keyErr *knownhosts.KeyError
	kh, err := s.checkKnownHosts()
	if err != nil {
		s.logger.Error("Error while getting the list of knows Host:", err)
		return err
	}
	hErr := kh(host, remote, pubkey)
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
		return s.addHostKey(host, remote, pubkey)
	}
	// writer.Printf("Pub key exists for %s.\n", host)
	return nil
}
func (s *SSHUtilImpl) ConnectAndExecuteCommandOnRemoteWithSudoPassword(sshConfig *SSHConfig, sudoPassword string, sudoPasswordCmd string) (bool, error) {
	//Ran the command using the sudoPassword provided
	s.SetSSHConfig(sshConfig)
	conn, err := s.GetConnection()
	if err != nil {
		return false, err
	}
	session, err := s.SshClient.NewSession(conn)
	if err != nil {
		s.logger.Error("Session Creation failed:", err)
		return false, err
	}
	remoteCommands := fmt.Sprintf(sudoPasswordCmd, sudoPassword)
	output, err := s.SshClient.CombinedOutput(remoteCommands, session)
	s.logger.Debug("The output for the execution of the command : ", output)
	if err != nil {
		s.logger.Error("Error while executing command on the remote host:", err)
		return false, err
	}
	s.logger.Debug("The output from the session:", output)
	defer s.SshClient.Close(session)
	return true, nil
}

func (s *SSHUtilImpl) createKnownHosts() error {
	f, fErr := os.OpenFile(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"), os.O_CREATE, 0600)
	if fErr != nil {
		return fErr
	}
	f.Close()
	return nil
}

func (s *SSHUtilImpl) checkKnownHosts() (ssh.HostKeyCallback, error) {
	err := s.createKnownHosts()
	if err != nil {
		return nil, err
	}
	kh, e := s.SshClient.New(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"))
	if e != nil {
		s.logger.Error("Error while getting the known host: %v", e)
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
		s.logger.Error("Error while opening file on the given path:", fErr)
		return fErr
	}
	defer f.Close()
	knownHosts := s.SshClient.Normalize(remote.String())
	_, fileErr := f.WriteString(knownhosts.Line([]string{knownHosts}, pubKey))
	f.WriteString("\n")
	return fileErr
}
