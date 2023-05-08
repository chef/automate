package fiberutils

import "os/exec"

func ExecuteShellCommand(cmd string) ([]byte, error) {
	return exec.Command("/bin/sh", "-c", cmd).CombinedOutput()
}
