package sshutils_test

import (
	"errors"
	"net"

	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
	"golang.org/x/crypto/ssh"
)

const (
	testfile        = `./testfiles/ssh`
	sudoPassword    = "123456"
	sudoPasswordCmd = `echo 123456 | sudo -S ls -l`
	dialFailed      = `dial failed:ssh: handshake failed: ssh: unable to authenticate, attempted methods [none publickey], no supported methods remain`
	nodeIp          = "1.1.1.1"
	userName        = "ubuntu"
	port            = "22"
	timeout         = 150
)

var sshConfig = sshutils.SSHConfig{
	SshUser:    "ubuntu",
	SshPort:    "22",
	SshKeyFile: testfile,
	HostIP:     "1.1.1.1",
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
						return []byte("total 0"), nil
					},
					Closefunc: func(s *ssh.Session) error {
						return nil
					},
				},
			},
			want:    "total 0",
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
						return nil, errors.New("Session creation failed")
					},
					CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
						return nil, nil
					},
					Closefunc: func(s *ssh.Session) error {
						return nil
					},
				},
			},
			want:    "Session creation failed",
			wantErr: errors.New("Session creation failed"),
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
			_, gotError := sshu.CheckKnownHosts()
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
			gotError := sshu.CreateKnownHosts()
			assert.Equal(t, gotError, tt.wantedError)
		})
	}
}

func TestAddHostKey(t *testing.T) {
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
			description: "If the Adding host key was successfully done",
			args: args{
				host: nodeIp,
				remote: &netAddressTest{
					Address: nodeIp,
				},
				pubkey: &sshPublicKeyTest{
					key:  "",
					data: nil,
				},
				MockSshClient: &sshutils.MockSshClient{
					Normalizefunc: func(address string) string {
						return ""
					},
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return func(host string, remote net.Addr, pubkey ssh.PublicKey) error {
							return nil
						}, nil
					},
				},
			},
			wantErr: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			gotError := sshu.AddHostKey(tt.args.host, tt.args.remote, tt.args.pubkey)
			assert.Equal(t, gotError, tt.wantErr)
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
				host: nodeIp,
				remote: &netAddressTest{
					Address: nodeIp,
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
		ip:       nodeIp,
		port:     port,
		keyfile:  testfile,
		userName: userName,
		timeout:  timeout,
	}
	want := sshutils.SSHConfig{
		SshUser:    "ubuntu",
		SshPort:    "22",
		SshKeyFile: testfile,
		HostIP:     "1.1.1.1",
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
		ip:       nodeIp,
		port:     port,
		keyfile:  testfile,
		userName: userName,
	}
	want := sshutils.SSHConfig{
		SshUser:    "ubuntu",
		SshPort:    "22",
		SshKeyFile: testfile,
		HostIP:     "1.1.1.1",
	}
	response := sshutils.NewSshConfig(arg.ip, arg.port, arg.keyfile, arg.userName)
	assert.Equal(t, response, want)
}