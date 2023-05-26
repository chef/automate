package userutils

import "os/user"

type UserUtil interface {
	Lookup(name string) (*user.User, error)
	LookupGroup(name string) (*user.Group, error)
	LookupGroupId(name string) (*user.Group, error)
}

type UserUtilImp struct{}

func NewUserUtilImp() UserUtil {
	return &UserUtilImp{}
}

func (s *UserUtilImp) Lookup(name string) (*user.User, error) {
	return user.Lookup(name)
}

func (s *UserUtilImp) LookupGroup(name string) (*user.Group, error) {
	return user.LookupGroup(name)
}

func (s *UserUtilImp) LookupGroupId(name string) (*user.Group, error) {
	return user.LookupGroupId(name)
}