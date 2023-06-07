package sshutils

import (
	"golang.org/x/crypto/ssh"
	"net"
)

type MockSSHUtilsImpl struct {
	GetSSHConfigfunc                                     func() (sshConfig *SSHConfig)
	SetSSHConfigfunc                                     func(sshConfig *SSHConfig)
	getClientConfigfunc                                  func() (*ssh.ClientConfig, error)
	GetConnectionfunc                                    func() (*ssh.Client, error)
	ConnectAndExecuteCommandOnRemoteWithSudoPasswordfunc func(*SSHConfig, string, string) (bool, error)
	checkKnownHostsfunc                                  func() (ssh.HostKeyCallback, error)
	createKnownHostsfunc                                 func() error
	addHostKeyfunc                                       func(host string, remote net.Addr, pubKey ssh.PublicKey) error
}

func (msu *MockSSHUtilsImpl) GetSSHConfig() *SSHConfig {
	return msu.GetSSHConfigfunc()
}

func (msu *MockSSHUtilsImpl) SetSSHConfig(sshConfig *SSHConfig) {
}

func (msu *MockSSHUtilsImpl) getClientConfig() (*ssh.ClientConfig, error) {
	return msu.getClientConfigfunc()
}

func (msu *MockSSHUtilsImpl) GetConnection() (*ssh.Client, error) {
	return msu.GetConnectionfunc()
}

func (msu *MockSSHUtilsImpl) ConnectAndExecuteCommandOnRemoteWithSudoPassword(sshConfig *SSHConfig, sudoPassword string, sudoPasswordCmd string) (bool, error) {
	return msu.ConnectAndExecuteCommandOnRemoteWithSudoPasswordfunc(sshConfig, sudoPassword, sudoPasswordCmd)
}

func (msu *MockSSHUtilsImpl) checkKnownHosts() (ssh.HostKeyCallback, error) {
	return msu.checkKnownHostsfunc()
}

func (msu *MockSSHUtilsImpl) createKnownHosts() error {
	return msu.createKnownHostsfunc()
}

func (msu *MockSSHUtilsImpl) addHostKey(host string, remote net.Addr, pubKey ssh.PublicKey) error {
	return msu.addHostKeyfunc(host, remote, pubKey)
}

type MockSshClient struct {
	Dialfunc            func(network string, addr string, config *ssh.ClientConfig) (*ssh.Client, error)
	NewSessionfunc      func(*ssh.Client) (*ssh.Session, error)
	ParsePrivateKeyfunc func(pemBytes []byte) (ssh.Signer, error)
	PublicKeyfunc       func(signers ssh.Signer) ssh.AuthMethod
	CombinedOutputfunc  func(cmd string, session *ssh.Session) ([]byte, error)
	Closefunc           func(*ssh.Session) error
	Normalizefunc       func(address string) string
	Newfunc             func(files ...string) (ssh.HostKeyCallback, error)
}

func (mssh *MockSshClient) Dial(network string, addr string, config *ssh.ClientConfig) (*ssh.Client, error) {
	return mssh.Dialfunc(network, addr, config)
}

func (mssh *MockSshClient) NewSession(client *ssh.Client) (*ssh.Session, error) {
	return mssh.NewSessionfunc(client)
}

func (mssh *MockSshClient) ParsePrivateKey(pemBytes []byte) (ssh.Signer, error) {
	return mssh.ParsePrivateKeyfunc(pemBytes)
}

func (mssh *MockSshClient) PublicKey(signers ssh.Signer) ssh.AuthMethod {
	return mssh.PublicKeyfunc(signers)
}

func (mssh *MockSshClient) CombinedOutput(cmd string, session *ssh.Session) ([]byte, error) {
	return mssh.CombinedOutputfunc(cmd, session)
}

func (mssh *MockSshClient) Close(session *ssh.Session) error {
	return mssh.Closefunc(session)
}

func (mssh *MockSshClient) Normalize(address string) string {
	return mssh.Normalizefunc(address)
}

func (mssh *MockSshClient) New(files ...string) (ssh.HostKeyCallback, error) {
	return mssh.Newfunc(files...)
}
