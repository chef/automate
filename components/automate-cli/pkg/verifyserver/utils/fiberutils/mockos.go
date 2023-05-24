package fiberutils

import "os/user"

type ExecCmdServiceMock struct {
	CommandFunc func(name string, args []string) ([]byte, error)
}

func (s *ExecCmdServiceMock) Command(name string, args []string) ([]byte, error) {
	return s.CommandFunc(name, args)
}

type UserCmdServiceMock struct {
	LookupFunc        func(name string) (*user.User, error)
	LookupGroupFunc   func(name string) (*user.Group, error)
	LookupGroupIdFunc func(name string) (*user.Group, error)
}

func (mock *UserCmdServiceMock) Lookup(name string) (*user.User, error) {
	return mock.LookupFunc(name)
}

func (mock *UserCmdServiceMock) LookupGroup(name string) (*user.Group, error) {
	return mock.LookupGroupFunc(name)
}

func (mock *UserCmdServiceMock) LookupGroupId(name string) (*user.Group, error) {
	return mock.LookupGroupIdFunc(name)
}
