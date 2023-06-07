package sshutils

import (
	"errors"

	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/logger"
	"golang.org/x/crypto/ssh"
)

const (
	testfile        = `./testfiles/ssh`
	sudoPassword    = "123456"
	sudoPasswordCmd = `echo "%s" | sudo -S ls -l`
	dialFailed      = `dial failed:ssh: handshake failed: ssh: unable to authenticate, attempted methods [none publickey], no supported methods remain`
	nodeIp          = "1.1.1.1"
)

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
	sshu := NewSSHUtil(&SSHConfig{
		SshKeyFile: testfile,
	}, &sshClient{}, log)
	type args struct {
		MockSshClient ISshClient
	}
	tests := []struct {
		description string
		args        args
		wantErr     error
	}{
		{
			description: "If the connection was successfully done",
			args: args{
				MockSshClient: &MockSshClient{
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
				MockSshClient: &MockSshClient{
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
				MockSshClient: &MockSshClient{
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
			_, got := sshu.GetConnection()
			assert.Equal(t, got, tt.wantErr)
		})
	}
}

func TestConnectAndExecuteCommandOnRemoteWithSudoPassword(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := NewSSHUtil(&SSHConfig{
		SshUser:    "ubuntu",
		SshPort:    "22",
		SshKeyFile: testfile,
		HostIP:     nodeIp,
		Timeout:    120,
	}, &sshClient{}, log)
	type args struct {
		sshConfig     *SSHConfig
		sudoPass      string
		sudoPassCmd   string
		MockSshClient ISshClient
	}
	tests := []struct {
		description    string
		args           args
		wantedResponse bool
		wantedErr      error
	}{
		{
			description: "If the connection was successfully",
			args: args{
				sshConfig: &SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testfile,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				sudoPass:    sudoPassword,
				sudoPassCmd: sudoPasswordCmd,
				MockSshClient: &MockSshClient{
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
						return nil, nil
					},
					Closefunc: func(session *ssh.Session) error {
						return nil
					},
				},
			},
			wantedResponse: true,
			wantedErr:      nil,
		},
		{
			description: "If the connection with tcp gets interrupted",
			args: args{
				sshConfig: &SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testfile,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				sudoPass:    sudoPassword,
				sudoPassCmd: sudoPasswordCmd,
				MockSshClient: &MockSshClient{
					Dialfunc: func(network, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
						return nil, errors.New("dial failed:ssh: handshake failed: ssh: unable to authenticate, attempted methods [none publickey], no supported methods remain")
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
					Closefunc: func(session *ssh.Session) error {
						return nil
					},
				},
			},
			wantedResponse: false,
			wantedErr:      errors.New("dial failed:ssh: handshake failed: ssh: unable to authenticate, attempted methods [none publickey], no supported methods remain"),
		},
		{
			description: "If the tcp connection was successsfull but creation of session got failed",
			args: args{
				sshConfig: &SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testfile,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				sudoPass:    sudoPassword,
				sudoPassCmd: sudoPasswordCmd,
				MockSshClient: &MockSshClient{
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
						return nil, errors.New("Session Creation failed:")
					},
					CombinedOutputfunc: func(cmd string, session *ssh.Session) ([]byte, error) {
						return nil, nil
					},
					Closefunc: func(session *ssh.Session) error {
						return nil
					},
				},
			},
			wantedResponse: false,
			wantedErr:      errors.New("Session Creation failed:"),
		},
		{
			description: "If the cammand execution on the Remote session fails",
			args: args{
				sshConfig: &SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testfile,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				sudoPass:    sudoPassword,
				sudoPassCmd: sudoPasswordCmd,
				MockSshClient: &MockSshClient{
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
						return nil, errors.New("Error while executing command on the remote host:")
					},
					Closefunc: func(session *ssh.Session) error {
						return nil
					},
				},
			},
			wantedResponse: false,
			wantedErr:      errors.New("Error while executing command on the remote host:"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			gotResponse, gotError := sshu.ConnectAndExecuteCommandOnRemoteWithSudoPassword(tt.args.sshConfig, tt.args.sudoPass, tt.args.sudoPassCmd)
			sshu.logger.Debug("the value of gotResponse = ", gotResponse)
			assert.Equal(t, gotResponse, tt.wantedResponse)
			assert.Equal(t, gotError, tt.wantedErr)
		})
	}
}

func TestGetClientConfig(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := NewSSHUtil(&SSHConfig{
		SshKeyFile: testfile,
	}, &sshClient{}, log)
	type args struct {
		MockSshClient ISshClient
	}
	tests := []struct {
		description string
		args        args
		wantedErr   error
	}{
		{
			description: "If the client config creation was successful",
			args: args{
				MockSshClient: &MockSshClient{
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
			_, got := sshu.getClientConfig()
			assert.Equal(t, got, tt.wantedErr)
		})
	}
}

func TestCheckKnownHosts(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := NewSSHUtil(&SSHConfig{}, &sshClient{}, log)
	type args struct {
		MockSshClient ISshClient
	}
	tests := []struct {
		description string
		args        args
		wantedError error
	}{
		{
			description: "If checking the known Host",
			args: args{
				MockSshClient: &MockSshClient{
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return nil, nil
					},
				},
			},
			wantedError: nil,
		},
		{
			description: "If the checking in known Host fails",
			args: args{
				MockSshClient: &MockSshClient{
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
			_, gotError := sshu.checkKnownHosts()
			assert.Equal(t, gotError, tt.wantedError)
		})
	}
}

func TestCreateKnownHosts(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := NewSSHUtil(&SSHConfig{}, &sshClient{}, log)
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
			gotError := sshu.createKnownHosts()
			assert.Equal(t, gotError, tt.wantedError)
		})
	}
}

func TestAddHostKey(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sshu := NewSSHUtil(&SSHConfig{}, &sshClient{}, log)
	type args struct {
		host          string
		remote        *netAddressTest
		pubkey        ssh.PublicKey
		MockSshClient ISshClient
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
				MockSshClient: &MockSshClient{
					Normalizefunc: func(address string) string {
						return ""
					},
					Newfunc: func(files ...string) (ssh.HostKeyCallback, error) {
						return nil, nil
					},
				},
			},
			wantErr: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			sshu.SshClient = tt.args.MockSshClient
			gotError := sshu.addHostKey(tt.args.host, tt.args.remote, tt.args.pubkey)
			sshu.logger.Debug("The debug log = ", gotError)
			assert.Equal(t, gotError, tt.wantErr)
		})
	}
}
