package sshutils

import (
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"
)

type ISshClient interface {
	Dial(network string, addr string, config *ssh.ClientConfig) (*ssh.Client, error)
	NewSession(*ssh.Client) (*ssh.Session, error)
	ParsePrivateKey(pemBytes []byte) (ssh.Signer, error)
	PublicKey(signers ssh.Signer) ssh.AuthMethod
	CombinedOutput(cmd string, session *ssh.Session) ([]byte, error)
	Close(*ssh.Session) error
	Normalize(address string) string
	New(files ...string) (ssh.HostKeyCallback, error)
}

type SshClient struct{}

func NewSshClient() ISshClient {
	return &SshClient{}
}

func (sc *SshClient) Dial(network string, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
	return ssh.Dial(network, addr, config)
}

func (sc *SshClient) NewSession(conn *ssh.Client) (*ssh.Session, error) {
	return conn.NewSession()
}

func (sc *SshClient) ParsePrivateKey(pemBytes []byte) (ssh.Signer, error) {
	return ssh.ParsePrivateKey(pemBytes)
}

func (sc *SshClient) PublicKey(signers ssh.Signer) ssh.AuthMethod {
	return ssh.PublicKeys(signers)
}

func (sc *SshClient) CombinedOutput(cmd string, session *ssh.Session) ([]byte, error) {
	return session.CombinedOutput(cmd)
}

func (sc *SshClient) Close(session *ssh.Session) error {
	return session.Close()
}

func (sc *SshClient) Normalize(address string) string {
	return knownhosts.Normalize(address)
}

func (sc *SshClient) New(files ...string) (ssh.HostKeyCallback, error) {
	return knownhosts.New(files...)
}
