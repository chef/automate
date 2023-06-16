package sshutils

import (
	"net"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/platform/command"
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
	SshClient ISshClient
	logger    logger.Logger
	Exec      command.Executor
}

type SSHUtil interface {
	Execute(sshConfig SSHConfig, cmd string) (string, error)
	ExecuteConcurrently(sshConfig SSHConfig, cmd string, hostIPs []string) []Result
	CopyFileToRemote(sshConfig SSHConfig, srcFilePath string, destFileName string, removeFile bool) error
	CopyFileToRemoteConcurrently(sshConfig SSHConfig, srcFilePath string, destFileName string, removeFile bool, hostIPs []string) []Result
}

type Result struct {
	HostIP string
	Output string
	Error  error
}

func NewSSHUtil(sshclient ISshClient, logger logger.Logger) *SSHUtilImpl {
	return &SSHUtilImpl{
		SshClient: sshclient,
		logger:    logger,
	}
}

func NewSSHUtilWithCommandExecutor(sshclient ISshClient, logger logger.Logger, exec command.Executor) *SSHUtilImpl {
	return &SSHUtilImpl{
		SshClient: sshclient,
		logger:    logger,
		Exec:      exec,
	}
}

func NewSshConfigWithTimeout(ip string, port string, keyFile string, userName string, timeout int) SSHConfig {
	return SSHConfig{
		SshUser:    userName,
		SshPort:    port,
		SshKeyFile: keyFile,
		HostIP:     ip,
		Timeout:    timeout,
	}
}

func NewSshConfig(ip string, port string, keyFile string, userName string) SSHConfig {
	return SSHConfig{
		SshUser:    userName,
		SshPort:    port,
		SshKeyFile: keyFile,
		HostIP:     ip,
	}
}

func (s *SSHUtilImpl) GetConnection(sshConfig SSHConfig) (*ssh.Client, error) {
	config, err := s.GetClientConfig(sshConfig)
	if err != nil {
		s.logger.Error("Error while generating the client config:", err)
		return nil, err
	}
	// Open connection
	addr := sshConfig.HostIP
	if len(strings.TrimSpace(sshConfig.SshPort)) > 0 {
		addr = addr + ":" + sshConfig.SshPort
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

func (s *SSHUtilImpl) GetClientConfig(sshConfig SSHConfig) (*ssh.ClientConfig, error) {
	pemBytes, err := os.ReadFile(sshConfig.SshKeyFile)
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
		User:            sshConfig.SshUser,
		Auth:            []ssh.AuthMethod{s.SshClient.PublicKey(signer)},
		HostKeyCallback: s.HostKeyCallback,
	}, nil
}

func (s *SSHUtilImpl) HostKeyCallback(host string, remote net.Addr, pubkey ssh.PublicKey) error {
	var keyErr *knownhosts.KeyError
	kh, err := s.CheckKnownHosts()
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
		return s.AddHostKey(host, remote, pubkey)
	}
	return nil
}

func (s *SSHUtilImpl) Execute(sshConfig SSHConfig, cmd string) (string, error) {
	//Setting default timeout
	if sshConfig.Timeout == 0 {
		sshConfig.Timeout = 150
	}
	conn, err := s.GetConnection(sshConfig)
	if err != nil {
		return "Connection creation failed", err
	}
	session, err := s.SshClient.NewSession(conn)
	if err != nil {
		s.logger.Error("Session creation failed:", err)
		return "Session creation failed", err
	}
	var output string
	errCh := make(chan error)
	go func() {
		outputByte, err := s.SshClient.CombinedOutput(cmd, session)
		output = string(outputByte)

		if strings.Contains(output, "Sorry, try again.") || strings.Contains(output, "sudo: a password is required") {
			errCh <- errors.New("sudo password is incorrect")
			return
		}
		pattern := regexp.MustCompile(`^\[sudo\] password for .+: `)
		output = pattern.ReplaceAllString(output, "")
		// if sudo password is correct then replace password prompt with empty string from output

		if err != nil {
			if strings.Contains(output, "sudo: no tty present and no askpass program specified") {
				errCh <- errors.New("The sudo password is missing. Make sure to provide sudo_password as enviroment variable and pass -E option while running command.")
				return
			}
			errCh <- err
			return
		}
		errCh <- nil
	}()

	select {
	case <-time.After(time.Duration(sshConfig.Timeout) * time.Second):
		return "", errors.New("command timed out")
	case err := <-errCh:
		if err != nil {
			return output, err
		}
	}
	defer s.SshClient.Close(session)
	return output, nil
}

func (s *SSHUtilImpl) ExecuteConcurrently(sshConfig SSHConfig, cmd string, hostIPs []string) []Result {
	resultChan := make(chan Result, len(hostIPs))

	for _, hostIP := range hostIPs {
		sshConfig.HostIP = hostIP
		s.logger.Debugln("Connecting to the node : " + hostIP)

		go func(sshConfig SSHConfig, cmd string, resultChan chan Result) {
			rc := Result{sshConfig.HostIP, "", nil}

			output, err := s.Execute(sshConfig, cmd)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}

			if strings.Contains(strings.ToUpper(strings.TrimSpace(output)), "ERROR") {
				rc.Error = errors.New(output)
				resultChan <- rc
				return
			}

			rc.Output = output
			resultChan <- rc
		}(sshConfig, cmd, resultChan)
	}

	var results []Result

	for i := 0; i < len(hostIPs); i++ {
		result := <-resultChan

		if result.Error != nil {
			s.logger.Errorf("Remote executation failed on node : %s with error: %v\n", result.HostIP, result.Error)
		} else {
			s.logger.Debugf("Remote executation is completed. Output for Host IP %s : \n%s\n", result.HostIP, result.Output)
		}

		results = append(results, result)
	}
	close(resultChan)
	return results
}

func (s *SSHUtilImpl) CopyFileToRemote(sshConfig SSHConfig, srcFilePath string, destFileName string, removeFile bool) error {
	cmd := "scp"
	args := []string{"-P " + sshConfig.SshPort, "-o StrictHostKeyChecking=no", "-i", sshConfig.SshKeyFile, "-r", srcFilePath, sshConfig.SshUser + "@" + sshConfig.HostIP + ":/tmp/" + destFileName}
	if err := s.Exec.Run(cmd, command.Args(args...)); err != nil {
		s.logger.Errorf("Failed to copy file %s to remote with error: %v\n", srcFilePath, err)
		if srcFilePath == "/usr/bin/chef-automate" {
			s.logger.Errorln("Please copy your chef-automate binary to /usr/bin")
		}
		return err
	}
	if removeFile {
		cmd := "rm"
		args := []string{"-rf", srcFilePath}
		if err := s.Exec.Run(cmd, command.Args(args...)); err != nil {
			s.logger.Errorf("Failed to remove source file with error: %v\n", err)
			return err
		}
	}
	return nil
}

func (s *SSHUtilImpl) CopyFileToRemoteConcurrently(sshConfig SSHConfig, srcFilePath string, destFileName string, removeFile bool, hostIPs []string) []Result {
	resultChan := make(chan Result, len(hostIPs))

	for _, hostIP := range hostIPs {
		sshConfig.HostIP = hostIP
		s.logger.Debugln("Copying to the node : " + hostIP)

		go func(sshConfig SSHConfig, srcFilePath string, destFileName string, removeFile bool, resultChan chan Result) {
			rc := Result{sshConfig.HostIP, "", nil}

			err := s.CopyFileToRemote(sshConfig, srcFilePath, destFileName, removeFile)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}

			resultChan <- rc
		}(sshConfig, srcFilePath, destFileName, removeFile, resultChan)
	}

	var results []Result

	for i := 0; i < len(hostIPs); i++ {
		result := <-resultChan

		if result.Error != nil {
			s.logger.Error("Remote copying failed on node : " + result.HostIP + " with error:\n" + result.Error.Error() + "\n")
		} else {
			s.logger.Debugf("Remote copying of file %s is completed on node at %s:/tmp/%s\n", srcFilePath, result.HostIP, destFileName)
		}

		results = append(results, result)
	}
	close(resultChan)
	return results
}

func (s *SSHUtilImpl) CreateKnownHosts() error {
	f, fErr := os.OpenFile(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"), os.O_CREATE, 0600)
	if fErr != nil {
		return fErr
	}
	f.Close()
	return nil
}

func (s *SSHUtilImpl) CheckKnownHosts() (ssh.HostKeyCallback, error) {
	err := s.CreateKnownHosts()
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

func (s *SSHUtilImpl) AddHostKey(host string, remote net.Addr, pubKey ssh.PublicKey) error {
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
