package nodes

import (
	"errors"
	"io/ioutil"
)

// PrepareSSHPrivateKey takes the contents of a SSH key and saves them into a temporary file
func PrepareSSHPrivateKey(keyContent string) (string, error) {
	// Specify "" for the temp dir as ioutil will pick TMPDIR or OS default
	keyPath, err := ioutil.TempFile("", ".inspec")
	if err != nil {
		return "", err
	}

	err = ioutil.WriteFile(keyPath.Name(), []byte(keyContent), 0400)
	if err != nil {
		return "", errors.New("Failed to write inspec private key " + keyPath.Name() + ": " + err.Error())
	}
	return keyPath.Name(), nil
}
