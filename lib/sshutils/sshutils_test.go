package sshutils_test

import (
	"errors"
	"io/ioutil"
	"net"
	"path/filepath"

	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/sshutils"
	"golang.org/x/crypto/ssh"
)

const (
	testfile                   = `./testfiles/ssh`
	sudoPassword               = "123456"
	sudoPasswordCmd            = `echo 123456 | sudo -S ls -l`
	dialFailed                 = `dial failed:ssh: handshake failed: ssh: unable to authenticate, attempted methods [none publickey], no supported methods remain`
	nodeIp1                    = "1.1.1.1"
	nodeIp2                    = "1.1.1.2"
	nodeIp3                    = "1.1.1.3"
	userName                   = "ubuntu"
	port                       = "22"
	timeout                    = 150
	totalZero                  = "total 0"
	sessionCreationFailedError = "Session creation failed"
	srcFilePath                = "/some/file/path"
	destFileName               = "abc.txt"
	remoteCopyError            = "error while copying file to remote"
	outputWithError            = "some error output"
)

var sshConfig = sshutils.SSHConfig{
	SshUser:    "ubuntu",
	SshPort:    "22",
	SshKeyFile: testfile,
	HostIP:     nodeIp1,
	Timeout:    150,
}

type netAddressTest struct {
	Address string
}

func (n *netAddressTest) Network() string {
	return n.Address
}

func (n *netAddressTest) String() string {
	return n.Address
}

type sshPublicKeyTest struct {
	key  string
	data []byte
}

func (pub *sshPublicKeyTest) Type() string {
	return pub.key
}

func (pub *sshPublicKeyTest) Marshal() []byte {
	return pub.data
}

func (pub *sshPublicKeyTest) Verify([]byte, *ssh.Signature) error {
	return nil
}

func TestGetConnection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)
	type args struct {
		sshConfig     sshutils.SSHConfig
		MockSshClient sshutils.ISshClient
	}
	tests := []struct {
		description string
		args        args
		wantErr     error
	}{
		{
			description: "If the connection was successfully done",
			args: args{
				sshConfig: sshConfig,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network string, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
				},
			},
			wantErr: nil,
		},
		{
			description: "If the connection was not successfully done",
			args: args{
				sshConfig: sshConfig,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return nil, errors.New(dialFailed)
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
				},
			},
			wantErr: errors.New(dialFailed),
		},
		{
			description: "If the Client Config was not generated as expected",
			args: args{
				sshConfig: sshConfig,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return nil, errors.New("Error while generating the client config: Unable to read private key: no such file or directory")
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
				},
			},
			wantErr: errors.New("Error while generating the client config: Unable to read private key: no such file or directory"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			_, got := sshu.GetConnection(tt.args.sshConfig)
			assert.Equal(t, got, tt.wantErr)
		})
	}
}

func TestGetClientConfig(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)
	type args struct {
		sshConfig     sshutils.SSHConfig
		MockSshClient sshutils.ISshClient
	}
	tests := []struct {
		description string
		args        args
		wantedErr   error
	}{
		{
			description: "If the client config creation was successful",
			args: args{
				sshConfig: sshConfig,
				MockSshClient: &sshutils.MockSshClient{
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return nil, nil
					},
					Normalizefunc: func(address string) string {
						return ""
					},
				},
			},
			wantedErr: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			_, got := sshu.GetClientConfig(tt.args.sshConfig)
			assert.Equal(t, got, tt.wantedErr)
		})
	}
}

func TestExecute(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)

	type args struct {
		sshConfig     sshutils.SSHConfig
		cmd           string
		MockSshClient sshutils.ISshClient
	}

	tests := []struct {
		description string
		args        args
		want        string
		wantErr     error
	}{
		{
			description: "Connection got eastablish and sudo password pass the check",
			args: args{
				sshConfig: sshConfig,
				cmd:       sudoPasswordCmd,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
					NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
						return nil, nil
					},
					CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
						return []byte(totalZero), nil
					},
					Closefunc: func(s *ssh.Session) error {
						return nil
					},
				},
			},
			want:    totalZero,
			wantErr: nil,
		},
		{
			description: "SSH Connection fails",
			args: args{
				sshConfig: sshConfig,
				cmd:       sudoPasswordCmd,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return nil, errors.New("dial failed:ssh: handshake failed: ssh: unable to authenticate,attempted methods [none publickey], no supported methods remain")
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
					NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
						return nil, nil
					},
					CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
						return nil, nil
					},
					Closefunc: func(s *ssh.Session) error {
						return nil
					},
				},
			},
			want:    "Connection creation failed",
			wantErr: errors.New("dial failed:ssh: handshake failed: ssh: unable to authenticate,attempted methods [none publickey], no supported methods remain"),
		},
		{
			description: "SSH Session Creation failed",
			args: args{
				sshConfig: sshConfig,
				cmd:       sudoPasswordCmd,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
					NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
						return nil, errors.New(sessionCreationFailedError)
					},
					CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
						return nil, nil
					},
					Closefunc: func(s *ssh.Session) error {
						return nil
					},
				},
			},
			want:    sessionCreationFailedError,
			wantErr: errors.New(sessionCreationFailedError),
		},
		{
			description: "Combined Output not able to produce the desired output",
			args: args{
				sshConfig: sshConfig,
				cmd:       sudoPassword,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
					NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
						return nil, nil
					},
					CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
						return []byte("Sorry try again"), errors.New("Error while executing command on the remote host:Process exited with status 1")
					},
					Closefunc: func(s *ssh.Session) error {
						return nil
					},
				},
			},
			want:    "Sorry try again",
			wantErr: errors.New("Error while executing command on the remote host:Process exited with status 1"),
		},
		{
			description: "Combined Ouput not able to execute the command",
			args: args{
				sshConfig: sshConfig,
				cmd:       sudoPasswordCmd,
				MockSshClient: &sshutils.MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
					ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
						return nil, nil
					},
					PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
						return nil
					},
					NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
						return nil, nil
					},
					CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
						return []byte("sudo: no tty present and no askpass program specified"), errors.New("The sudo password is missing. Make sure to provide sudo_password as enviroment variable and pass -E option while running command.")
					},
					Closefunc: func(s *ssh.Session) error {
						return nil
					},
				},
			},
			want:    "sudo: no tty present and no askpass program specified",
			wantErr: errors.New("The sudo password is missing. Make sure to provide sudo_password as enviroment variable and pass -E option while running command."),
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			got, gotError := sshu.Execute(tt.args.sshConfig, tt.args.cmd)
			assert.Equal(t, got, tt.want)
			if gotError != nil {
				assert.EqualError(t, gotError, tt.wantErr.Error())
			}
		})
	}
}

func TestExecuteConcurrently(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshUtil := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)

	tests := []struct {
		description   string
		sshConfig     sshutils.SSHConfig
		cmd           string
		hostIPs       []string
		MockSshClient sshutils.ISshClient
		want          string
		wantErr       error
	}{
		{
			description: "Execute command concurrently - success",
			sshConfig:   sshConfig,
			cmd:         "sudo whoami",
			hostIPs:     []string{nodeIp1, nodeIp2, nodeIp3},
			MockSshClient: &sshutils.MockSshClient{
				Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
					return &ssh.Client{}, nil
				},
				ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
					return nil, nil
				},
				PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
					return nil
				},
				NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
					return nil, nil
				},
				CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
					return []byte("root"), nil
				},
				Closefunc: func(s *ssh.Session) error {
					return nil
				},
			},
			want:    "root",
			wantErr: nil,
		},
		{
			description: "Execute command concurrently - failure",
			sshConfig:   sshConfig,
			cmd:         "some command",
			hostIPs:     []string{nodeIp1, nodeIp2, nodeIp3},
			MockSshClient: &sshutils.MockSshClient{
				Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
					return &ssh.Client{}, nil
				},
				ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
					return nil, nil
				},
				PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
					return nil
				},
				NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
					return nil, nil
				},
				CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
					return []byte(""), errors.New("error while executing command on the remote host:Process exited with status 1")
				},
				Closefunc: func(s *ssh.Session) error {
					return nil
				},
			},
			want:    "",
			wantErr: errors.New("error while executing command on the remote host:Process exited with status 1"),
		},
		{
			description: "Execute command concurrently - error in output",
			sshConfig:   sshConfig,
			cmd:         "sudo whoami",
			hostIPs:     []string{nodeIp1, nodeIp2, nodeIp3},
			MockSshClient: &sshutils.MockSshClient{
				Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
					return &ssh.Client{}, nil
				},
				ParsePrivateKeyfunc: func(pemBytes []byte) (ssh.Signer, error) {
					return nil, nil
				},
				PublicKeyfunc: func(signers ssh.Signer) ssh.AuthMethod {
					return nil
				},
				NewSessionfunc: func(c *ssh.Client) (*ssh.Session, error) {
					return nil, nil
				},
				CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
					return []byte(outputWithError), nil
				},
				Closefunc: func(s *ssh.Session) error {
					return nil
				},
			},
			want:    outputWithError,
			wantErr: errors.New(outputWithError),
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshUtil.SshClient = tt.MockSshClient
			results := sshUtil.ExecuteConcurrently(tt.sshConfig, tt.cmd, tt.hostIPs)
			for _, result := range results {
				if tt.wantErr != nil {
					assert.ErrorContains(t, result.Error, tt.wantErr.Error())
				}
				assert.Equal(t, result.Output, tt.want)
			}
		})
	}

}

func TestCopyFileToRemote(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshUtil := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)

	tests := []struct {
		description  string
		sshConfig    sshutils.SSHConfig
		exec         command.Executor
		srcFilePath  string
		destFileName string
		removeFile   bool
		wantErr      error
	}{
		{
			description: "copy file to remote - success",
			sshConfig:   sshConfig,
			exec: &command.MockExecutorImpl{
				RunFunc: func(cmd string, opts ...command.Opt) error {
					return nil
				},
			},
			srcFilePath:  srcFilePath,
			destFileName: destFileName,
			removeFile:   false,
			wantErr:      nil,
		},
		{
			description: "copy file to remote - failure",
			sshConfig:   sshConfig,
			exec: &command.MockExecutorImpl{
				RunFunc: func(cmd string, opts ...command.Opt) error {
					return errors.New(remoteCopyError)
				},
			},
			srcFilePath:  "/usr/bin/chef-automate",
			destFileName: destFileName,
			removeFile:   false,
			wantErr:      errors.New(remoteCopyError),
		},
		{
			description: "copy file to remote and delete source file - success",
			sshConfig:   sshConfig,
			exec: &command.MockExecutorImpl{
				RunFunc: func(cmd string, opts ...command.Opt) error {
					return nil
				},
			},
			srcFilePath:  srcFilePath,
			destFileName: destFileName,
			removeFile:   true,
			wantErr:      nil,
		},
		{
			description: "copy file to remote and delete source file - failure",
			sshConfig:   sshConfig,
			exec: &command.MockExecutorImpl{
				RunFunc: func(cmd string, opts ...command.Opt) error {
					if cmd == "rm" {
						return errors.New(remoteCopyError)
					}
					return nil
				},
			},
			srcFilePath:  srcFilePath,
			destFileName: destFileName,
			removeFile:   true,
			wantErr:      errors.New(remoteCopyError),
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshUtil.Exec = tt.exec
			err := sshUtil.CopyFileToRemote(tt.sshConfig, tt.srcFilePath, tt.destFileName, tt.removeFile)
			assert.Equal(t, err, tt.wantErr)
		})
	}
}

func TestCopyFileToRemoteConcurrently(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshUtil := sshutils.NewSSHUtilWithCommandExecutor(&sshutils.SshClient{}, log, command.NewMockExecutor(t))

	tests := []struct {
		description  string
		sshConfig    sshutils.SSHConfig
		exec         command.Executor
		srcFilePath  string
		destFileName string
		removeFile   bool
		hostIPs      []string
		wantErr      error
	}{
		{
			description: "copy file to remote concurrently - success",
			sshConfig:   sshConfig,
			exec: &command.MockExecutorImpl{
				RunFunc: func(cmd string, opts ...command.Opt) error {
					return nil
				},
			},
			srcFilePath:  srcFilePath,
			destFileName: destFileName,
			removeFile:   false,
			hostIPs:      []string{nodeIp1, nodeIp2, nodeIp3},
			wantErr:      nil,
		},
		{
			description: "copy file to remote concurrently - failure",
			sshConfig:   sshConfig,
			exec: &command.MockExecutorImpl{
				RunFunc: func(cmd string, opts ...command.Opt) error {
					return errors.New(remoteCopyError)
				},
			},
			srcFilePath:  srcFilePath,
			destFileName: destFileName,
			removeFile:   false,
			hostIPs:      []string{nodeIp1, nodeIp2, nodeIp3},
			wantErr:      errors.New(remoteCopyError),
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshUtil.Exec = tt.exec
			results := sshUtil.CopyFileToRemoteConcurrently(tt.sshConfig, tt.srcFilePath, tt.destFileName, tt.removeFile, tt.hostIPs)
			for _, result := range results {
				if tt.wantErr != nil {
					assert.ErrorContains(t, result.Error, tt.wantErr.Error())
				}
			}
		})
	}
}

func TestCheckKnownHosts(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)
	type args struct {
		MockSshClient sshutils.ISshClient
	}
	tests := []struct {
		description string
		args        args
		wantedError error
	}{
		{
			description: "If checking the known Host",
			args: args{
				MockSshClient: &sshutils.MockSshClient{
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return func(host string, remote net.Addr, pubkey ssh.PublicKey) error {
							return nil
						}, nil
					},
				},
			},
			wantedError: nil,
		},
		{
			description: "If the checking in known Host fails",
			args: args{
				MockSshClient: &sshutils.MockSshClient{
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return nil, errors.New("Error while getting the known host: ")
					},
				},
			},
			wantedError: errors.New("Error while getting the known host: "),
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			khdir := t.TempDir()
			_, gotError := sshu.CheckKnownHosts(filepath.Join(khdir, sshutils.AUTOMATE_KNOWN_HOSTS))
			assert.Equal(t, gotError, tt.wantedError)
		})
	}
}

func TestCreateKnownHosts(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)
	tests := []struct {
		description string
		wantedError error
	}{
		{
			description: "If creation of known Host was successfully done",
			wantedError: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			khdir := t.TempDir()
			gotError := sshu.CreateKnownHosts(filepath.Join(khdir, sshutils.AUTOMATE_KNOWN_HOSTS))
			assert.Equal(t, gotError, tt.wantedError)
		})
	}
}

func TestAddHostKey(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)
	type args struct {
		host            string
		remote          *netAddressTest
		pubkey          ssh.PublicKey
		MockSshClient   sshutils.ISshClient
		existingContent string
		expectedContent string
	}
	tests := []struct {
		description string
		args        args
		wantErr     error
	}{
		{
			description: "If the Adding host key was successfully done, empty known host file",
			args: args{
				host: nodeIp1,
				remote: &netAddressTest{
					Address: nodeIp1,
				},
				pubkey: &sshPublicKeyTest{
					key:  "test-key",
					data: []byte("test-data"),
				},
				MockSshClient: &sshutils.MockSshClient{
					Normalizefunc: func(address string) string {
						return nodeIp1
					},
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return func(host string, remote net.Addr, pubkey ssh.PublicKey) error {
							return nil
						}, nil
					},
				},
				existingContent: "",
				expectedContent: nodeIp1 + " test-key dGVzdC1kYXRh",
			},
			wantErr: nil,
		},
		{
			description: "If the Adding host key was successfully done, existing known host file",
			args: args{
				host: nodeIp1,
				remote: &netAddressTest{
					Address: nodeIp1,
				},
				pubkey: &sshPublicKeyTest{
					key:  "test-key",
					data: []byte("test-data"),
				},
				MockSshClient: &sshutils.MockSshClient{
					Normalizefunc: func(address string) string {
						return nodeIp1
					},
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return func(host string, remote net.Addr, pubkey ssh.PublicKey) error {
							return nil
						}, nil
					},
				},
				existingContent: nodeIp2 + " test-key dGVzdC1kYXRh",
				expectedContent: nodeIp2 + " test-key dGVzdC1kYXRh\n" + nodeIp1 + " test-key dGVzdC1kYXRh",
			},
			wantErr: nil,
		},
		{
			description: "If the Adding host key was successfully done, existing known host file with new line at end",
			args: args{
				host: nodeIp1,
				remote: &netAddressTest{
					Address: nodeIp1,
				},
				pubkey: &sshPublicKeyTest{
					key:  "test-key",
					data: []byte("test-data"),
				},
				MockSshClient: &sshutils.MockSshClient{
					Normalizefunc: func(address string) string {
						return nodeIp1
					},
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return func(host string, remote net.Addr, pubkey ssh.PublicKey) error {
							return nil
						}, nil
					},
				},
				existingContent: nodeIp2 + " test-key dGVzdC1kYXRh\n",
				expectedContent: nodeIp2 + " test-key dGVzdC1kYXRh\n" + nodeIp1 + " test-key dGVzdC1kYXRh",
			},
			wantErr: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			khdir := t.TempDir()
			f, err := ioutil.TempFile(khdir, "")
			assert.NoError(t, err)
			content := []byte(tt.args.existingContent)
			_, err = f.Write(content)
			assert.NoError(t, err)
			gotError := sshu.AddHostKey(tt.args.host, f.Name(), tt.args.remote, tt.args.pubkey)
			assert.Equal(t, gotError, tt.wantErr)
			content, err = ioutil.ReadFile(f.Name())
			assert.NoError(t, err)
			actualContent := string(content)
			assert.Equal(t, tt.args.expectedContent, actualContent)
		})
	}
}

func TestHostCallKeyBack(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := sshutils.NewSSHUtil(&sshutils.SshClient{}, log)
	type args struct {
		host          string
		remote        *netAddressTest
		pubkey        ssh.PublicKey
		MockSshClient sshutils.ISshClient
	}
	tests := []struct {
		description string
		args        args
		wantErr     error
	}{
		{
			description: "Client Config creation is a success",
			args: args{
				host: nodeIp1,
				remote: &netAddressTest{
					Address: nodeIp1,
				},
				pubkey: &sshPublicKeyTest{
					key:  "",
					data: nil,
				},
				MockSshClient: &sshutils.MockSshClient{
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return func(host string, remote net.Addr, pubkey ssh.PublicKey) error {
							return nil
						}, nil
					},
					Normalizefunc: func(address string) string {
						return ""
					},
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			gotError := sshu.HostKeyCallback(tt.args.host, tt.args.remote, tt.args.pubkey)
			assert.Equal(t, gotError, tt.wantErr)
		})
	}
}

func TestNewSshConfigWithTimeout(t *testing.T) {
	type args struct {
		ip       string
		port     string
		keyfile  string
		userName string
		timeout  int
	}
	var arg args = args{
		ip:       nodeIp1,
		port:     port,
		keyfile:  testfile,
		userName: userName,
		timeout:  timeout,
	}
	want := sshutils.SSHConfig{
		SshUser:    "ubuntu",
		SshPort:    "22",
		SshKeyFile: testfile,
		HostIP:     nodeIp1,
		Timeout:    150,
	}
	response := sshutils.NewSshConfigWithTimeout(arg.ip, arg.port, arg.keyfile, arg.userName, arg.timeout)
	assert.Equal(t, response, want)
}

func TestNewSshConfig(t *testing.T) {
	type args struct {
		ip       string
		port     string
		keyfile  string
		userName string
	}
	var arg args = args{
		ip:       nodeIp1,
		port:     port,
		keyfile:  testfile,
		userName: userName,
	}
	want := sshutils.SSHConfig{
		SshUser:    "ubuntu",
		SshPort:    "22",
		SshKeyFile: testfile,
		HostIP:     nodeIp1,
	}
	response := sshutils.NewSshConfig(arg.ip, arg.port, arg.keyfile, arg.userName)
	assert.Equal(t, response, want)
}
