package fiberutils

import (
	"os/exec"
)

func ExecuteShellCommand(cmd string) ([]byte, error) {
	return exec.Command("/bin/sh", "-c", cmd).CombinedOutput()
}

func CheckPath(cmd string) (string, error) {
	return exec.LookPath(cmd)
}

type ExecCmdService interface {
	Command(name string, args []string) ([]byte, error)
}

type ExecCmdServiceImp struct{}

func NewExecCmdServiceImp() ExecCmdService {
	return &ExecCmdServiceImp{}
}

func (s *ExecCmdServiceImp) Command(name string, args []string) ([]byte, error) {
	return exec.Command(name, args...).Output()
}
