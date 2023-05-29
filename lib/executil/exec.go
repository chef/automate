package executil

import "os/exec"

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