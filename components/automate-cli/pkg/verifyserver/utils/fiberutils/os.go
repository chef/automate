package fiberutils

import (
	"os/exec"
	"os/user"
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

type UserCmdService interface {
	Lookup(name string) (*user.User, error)
	LookupGroup(name string) (*user.Group, error)
	LookupGroupId(name string) (*user.Group, error)
}

type UserCmdServiceImp struct{}

func NewUserCmdServiceImp() UserCmdService {
	return &UserCmdServiceImp{}
}

func (s *UserCmdServiceImp) Lookup(name string) (*user.User, error) {
	return user.Lookup(name)
}

func (s *UserCmdServiceImp) LookupGroup(name string) (*user.Group, error) {
	return user.LookupGroup(name)
}

func (s *UserCmdServiceImp) LookupGroupId(name string) (*user.Group, error) {
	return user.LookupGroupId(name)
}
