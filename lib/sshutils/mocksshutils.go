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
	createKnownHostsfunc                                 func()
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

func (msu *MockSSHUtilsImpl) createKnownHosts() {
	return
}

func (msu *MockSSHUtilsImpl) addHostKey(host string, remote net.Addr, pubKey ssh.PublicKey) error {
	return msu.addHostKeyfunc(host, remote, pubKey)
}
