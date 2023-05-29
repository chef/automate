package userutils

import "os/user"

type UserUtilMock struct {
	LookupFunc        func(name string) (*user.User, error)
	LookupGroupFunc   func(name string) (*user.Group, error)
	LookupGroupIdFunc func(name string) (*user.Group, error)
}

func (mock *UserUtilMock) Lookup(name string) (*user.User, error) {
	return mock.LookupFunc(name)
}

func (mock *UserUtilMock) LookupGroup(name string) (*user.Group, error) {
	return mock.LookupGroupFunc(name)
}

func (mock *UserUtilMock) LookupGroupId(name string) (*user.Group, error) {
	return mock.LookupGroupIdFunc(name)
}
