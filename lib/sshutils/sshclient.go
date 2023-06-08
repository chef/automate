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

type sshClient struct{}

func NewSshClient() ISshClient {
	return &sshClient{}
}

func (sc *sshClient) Dial(network string, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
	return ssh.Dial(network, addr, config)
}

func (sc *sshClient) NewSession(conn *ssh.Client) (*ssh.Session, error) {
	return conn.NewSession()
}

func (sc *sshClient) ParsePrivateKey(pemBytes []byte) (ssh.Signer, error) {
	return ssh.ParsePrivateKey(pemBytes)
}

func (sc *sshClient) PublicKey(signers ssh.Signer) ssh.AuthMethod {
	return ssh.PublicKeys(signers)
}

func (sc *sshClient) CombinedOutput(cmd string, session *ssh.Session) ([]byte, error) {
	return session.CombinedOutput(cmd)
}

func (sc *sshClient) Close(session *ssh.Session) error {
	return session.Close()
}

func (sc *sshClient) Normalize(address string) string {
	return knownhosts.Normalize(address)
}

func (sc *sshClient) New(files ...string) (ssh.HostKeyCallback, error) {
	return knownhosts.New(files...)
}
