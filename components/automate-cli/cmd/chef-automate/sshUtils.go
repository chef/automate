package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
	"unicode"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"
)

type SSHConfig struct {
	sshUser    string
	sshPort    string
	sshKeyFile string
	hostIP     string
	timeout    int
}

type SSHUtil interface {
	getSSHConfig() *SSHConfig
	setSSHConfig(sshConfig *SSHConfig)
	getClientConfig() (*ssh.ClientConfig, error)
	getConnection() (*ssh.Client, error)
	connectAndExecuteCommandOnRemote(remoteCommands string, spinner bool) (string, error)
	connectAndExecuteCommandOnRemoteSteamOutput(remoteCommands string) (string, error)
	copyFileToRemote(srcFilePath string, destFileName string, removeFile bool) error
	copyFileFromRemote(remoteFilePath string, outputFileName string) (string, error)
}

type SSHUtilImpl struct {
	SshConfig *SSHConfig
}

func NewSSHUtil(sshconfig *SSHConfig) SSHUtil {
	return &SSHUtilImpl{
		SshConfig: sshconfig,
	}
}

func (s *SSHUtilImpl) getSSHConfig() *SSHConfig {
	return s.SshConfig
}

func (s *SSHUtilImpl) setSSHConfig(sshConfig *SSHConfig) {
	s.SshConfig = sshConfig
}

func (s *SSHUtilImpl) getClientConfig() (*ssh.ClientConfig, error) {
	pemBytes, err := ioutil.ReadFile(s.SshConfig.sshKeyFile) // nosemgrep
	if err != nil {
		writer.Errorf("Unable to read private key: %v", err)
		return nil, err
	}
	signer, err := ssh.ParsePrivateKey(pemBytes)
	if err != nil {
		writer.Errorf("Parsing key failed: %v", err)
		return nil, err
	}
	var (
		keyErr *knownhosts.KeyError
	)
	// Client config
	return &ssh.ClientConfig{
		User: s.SshConfig.sshUser,
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
				// writer.Printf("WARNING: %s is not trusted, adding this key to known_hosts file.\n", host)
				return addHostKey(host, remote, pubKey)
			}
			// writer.Printf("Pub key exists for %s.\n", host)
			return nil
		}),
	}, nil
}

func (s *SSHUtilImpl) getConnection() (*ssh.Client, error) {
	config, err := s.getClientConfig()
	if err != nil {
		return nil, err
	}
	// Open connection
	addr := s.SshConfig.hostIP
	if len(strings.TrimSpace(s.SshConfig.sshPort)) > 0 {
		addr = addr + ":" + s.SshConfig.sshPort
	} else {
		addr = addr + ":22"
	}
	conn, err := ssh.Dial("tcp", addr, config)
	if conn == nil || err != nil {
		writer.Errorf("dial failed:%v\n", err)
		return nil, err
	}
	return conn, err
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
	f.WriteString("\n")
	return fileErr
}

func (s *SSHUtilImpl) connectAndExecuteCommandOnRemote(remoteCommands string, spinner bool) (string, error) {
	logrus.Debug("Executing command ......")
	logrus.Debug(remoteCommands)

	// Set timeout. Default is 1 hour
	timeout := 3600
	if s.SshConfig.timeout > 0 {
		timeout = s.SshConfig.timeout
	}

	conn, err := s.getConnection()
	if err != nil {
		return "", err
	}
	defer conn.Close()

	// Open session
	session, err := conn.NewSession()
	if err != nil {
		writer.Errorf("session failed:%v\n", err)
		return "", err
	}
	defer session.Close()

	if spinner {
		writer.StartSpinner()
	}

	var output []byte
	errCh := make(chan error)
	go func() {
		output, err = session.CombinedOutput(remoteCommands)
		if err != nil {
			errCh <- err
			return
		}
		errCh <- nil
	}()

	select {
	case <-time.After(time.Duration(timeout) * time.Second):
		return "", errors.New("command timed out")
	case err := <-errCh:
		if err != nil {
			if spinner {
				writer.StopSpinner()
			}
			return "", err
		}
	}

	if spinner {
		writer.StopSpinner()
	}

	logrus.Debug("Execution of command done......")
	return string(output), nil
}

func (s *SSHUtilImpl) connectAndExecuteCommandOnRemoteSteamOutput(remoteCommands string) (string, error) {
	logrus.Debug("Executing command ......")
	logrus.Debug(remoteCommands)
	conn, err := s.getConnection()
	if err != nil {
		return "", err
	}
	defer conn.Close()

	// Open session
	session, err := conn.NewSession()
	if err != nil {
		writer.Errorf("session failed:%v", err)
		return "", err
	}
	defer session.Close()
	/////////////////////
	cmdWriter, err := session.StdinPipe()
	if err != nil {
		writer.Println(err.Error())
	}
	cmdReader, err := session.StdoutPipe()
	if err != nil {
		writer.Printf("Error creating StdoutPipe for Cmd %s \n", err.Error())
		os.Exit(1)
	}
	cmdError, err := session.StderrPipe()
	if err != nil {
		writer.Println(err.Error())
	}

	spinnerChannel := make(chan string)
	go showSpinner(spinnerChannel)
	spinnerChannel <- "start"
	wr := make(chan []byte, 10)
	go stdInRoutine(wr, cmdWriter)
	outchan := make(chan string)
	go stdOutRoutine(outchan, cmdReader)
	go stdErrRoutine(spinnerChannel, cmdError)

	err = session.Start(remoteCommands)
	if err != nil {
		writer.Printf("Error starting Cmd %s \n", err.Error())
		os.Exit(1)
	}

	err = session.Wait()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error waiting for Cmd", err)
		os.Exit(1)
	}
	spinnerChannel <- "stop"
	output := <-outchan
	close(spinnerChannel)
	close(wr)
	close(outchan)
	logrus.Debug("Execution of command done......")
	return output, nil
}

func (s *SSHUtilImpl) copyFileToRemote(srcFilePath string, destFileName string, removeFile bool) error {
	cmd := "scp"
	exec_args := []string{"-P " + s.SshConfig.sshPort, "-o StrictHostKeyChecking=no", "-i", s.SshConfig.sshKeyFile, "-r", srcFilePath, s.SshConfig.sshUser + "@" + s.SshConfig.hostIP + ":/tmp/" + destFileName}
	if err := exec.Command(cmd, exec_args...).Run(); err != nil {
		writer.Printf("Failed to copy config file to remote %s\n", err.Error())
		return err
	}
	if removeFile {
		cmd := "rm"
		exec_args := []string{"-rf", srcFilePath}
		if err := exec.Command(cmd, exec_args...).Run(); err != nil {
			writer.Printf("Failed to copy file to remote %s\n", err.Error())
			return err
		}
	}
	return nil
}

// This function will copy file from remote to local and return new local file path
func (s *SSHUtilImpl) copyFileFromRemote(remoteFilePath string, outputFileName string) (string, error) {
	writer.Printf("Downloading file %s from remote node %s \n", remoteFilePath, s.SshConfig.hostIP)
	cmd := "scp"
	ts := time.Now().Format("20060102150405")
	destFileName := "/tmp/" + ts + "_" + outputFileName
	execArgs := []string{"-P " + s.SshConfig.sshPort, "-o StrictHostKeyChecking=no", "-o ConnectTimeout=30", "-i", s.SshConfig.sshKeyFile, "-r", s.SshConfig.sshUser + "@" + s.SshConfig.hostIP + ":" + remoteFilePath, destFileName}
	if err := exec.Command(cmd, execArgs...).Run(); err != nil {
		writer.Printf("Failed to copy file from remote %s\n", err.Error())
		return "", err
	}
	writer.Printf("File downloaded %s \n", destFileName)
	return destFileName, nil
}

func stdInRoutine(wr chan []byte, cmdWriter io.WriteCloser) {
	for {
		select {
		case d := <-wr:
			_, err := cmdWriter.Write(d)
			if err != nil {
				fmt.Println(err.Error())
			}
		}
	}
}

func stdOutRoutine(out chan string, cmdReader io.Reader) {
	scanner := bufio.NewScanner(cmdReader)
	var sb strings.Builder
	//var isStringAlphabetic = regexp.MustCompile(`*[a-zA-Z0-9_:]*`).MatchString
	for {
		if tkn := scanner.Scan(); tkn {
			rcv := scanner.Bytes()

			raw := make([]byte, len(rcv))
			copy(raw, rcv)
			t := strings.TrimFunc(string(raw), func(r rune) bool {
				return !unicode.IsGraphic(r)
			})
			writer.Println(t)
			sb.WriteString(t)
			sb.WriteString("\n")
		} else {
			if scanner.Err() != nil {
				fmt.Println(scanner.Err().Error())
			}
			out <- sb.String()
			return
		}
	}
}

func stdErrRoutine(sr chan string, cmdError io.Reader) {
	scanner := bufio.NewScanner(cmdError)

	for scanner.Scan() {
		t := scanner.Text()
		if len(strings.TrimSpace(t)) > 0 {
			sr <- "stop"
		}
		fmt.Println(t)
	}
}

func showSpinner(ch chan string) {
	for {
		state := <-ch
		if strings.EqualFold(strings.TrimSpace(state), "start") {
			writer.StartSpinner()
		}
		if strings.EqualFold(strings.TrimSpace(state), "stop") {
			writer.StopSpinner()
		}
	}
}
