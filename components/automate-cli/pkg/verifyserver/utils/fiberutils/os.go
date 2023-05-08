package fiberutils

import "os/exec"

func ExecuteShellCommand(name string, arg []string) ([]byte, error) {
	return exec.Command(name, arg...).CombinedOutput()
}
