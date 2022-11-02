package main

import (
	"bytes"
	"io/ioutil"
	"net"

	"github.com/pkg/errors"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"
)

func ConnectAndExecuteCommandOnRemote(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error) {

	pemBytes, err := ioutil.ReadFile(sshKeyFile) // nosemgrep
	if err != nil {
		writer.Errorf("Unable to read private key: %v", err)
		return "", err
	}
	signer, err := ssh.ParsePrivateKey(pemBytes)
	if err != nil {
		writer.Errorf("Parsing key failed: %v", err)
		return "", err
	}
	var (
		keyErr *knownhosts.KeyError
	)

	// Client config
	config := &ssh.ClientConfig{
		User: sshUser,
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
	}

	// Open connection
	conn, err := ssh.Dial("tcp", hostIP+":"+sshPort, config)
	if conn == nil || err != nil {
		writer.Errorf("dial failed:%v", err)
		return "", err
	}
	defer conn.Close()

	// Open session
	session, err := conn.NewSession()
	if err != nil {
		writer.Errorf("session failed:%v", err)
		return "", err
	}
	var stdoutBuf bytes.Buffer
	session.Stdout = &stdoutBuf

	writer.StartSpinner()
	err = session.Run(remoteCommands)

	writer.StopSpinner()
	if err != nil {
		writer.Errorf("Run failed:%v", err)
		return "", err
	}
	defer session.Close()
	return stdoutBuf.String(), nil
}
