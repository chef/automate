package user

import (
	"errors"
	"fmt"
	"os/exec"
	gouser "os/user"
	"strconv"
	"strings"
)

var DefaultLookupProvider LookupProvider = &chainLookupProvider{
	providers: []LookupProvider{
		goLookupProvider{},
		getentLookupProvider{},
	},
}

type LookupProvider interface {
	Lookup(username string) (*gouser.User, error)
	LookupId(uid string) (*gouser.User, error)
	LookupGroup(groupname string) (*gouser.Group, error)
	LookupGroupId(gid string) (*gouser.Group, error)
}

func Lookup(username string) (*gouser.User, error) {
	return DefaultLookupProvider.Lookup(username)
}

func LookupId(uid string) (*gouser.User, error) {
	return DefaultLookupProvider.LookupId(uid)
}

func LookupGroup(groupname string) (*gouser.Group, error) {
	return DefaultLookupProvider.LookupGroup(groupname)
}

func LookupGroupId(gid string) (*gouser.Group, error) {
	return DefaultLookupProvider.LookupGroupId(gid)
}

type chainLookupProvider struct {
	providers []LookupProvider
}

func (c *chainLookupProvider) Lookup(username string) (*gouser.User, error) {
	var lastErr error
	for _, provider := range c.providers {
		u, err := provider.Lookup(username)
		if err == nil {
			return u, nil
		}
		lastErr = err
	}
	return nil, lastErr
}

func (c *chainLookupProvider) LookupId(uid string) (*gouser.User, error) {
	var lastErr error
	for _, provider := range c.providers {
		u, err := provider.LookupId(uid)
		if err == nil {
			return u, nil
		}
		lastErr = err
	}
	return nil, lastErr
}

func (c *chainLookupProvider) LookupGroup(groupname string) (*gouser.Group, error) {
	var lastErr error
	for _, provider := range c.providers {
		g, err := provider.LookupGroup(groupname)
		if err == nil {
			return g, nil
		}
		lastErr = err
	}
	return nil, lastErr
}

func (c *chainLookupProvider) LookupGroupId(gid string) (*gouser.Group, error) {
	var lastErr error
	for _, provider := range c.providers {
		g, err := provider.LookupGroupId(gid)
		if err == nil {
			return g, nil
		}
		lastErr = err
	}
	return nil, lastErr
}

type goLookupProvider struct {
}

func (goLookupProvider) Lookup(username string) (*gouser.User, error) {
	return gouser.Lookup(username)
}

func (goLookupProvider) LookupId(uid string) (*gouser.User, error) {
	return gouser.LookupId(uid)
}

func (goLookupProvider) LookupGroup(groupname string) (*gouser.Group, error) {
	return gouser.LookupGroup(groupname)
}

func (goLookupProvider) LookupGroupId(gid string) (*gouser.Group, error) {
	return gouser.LookupGroupId(gid)
}

type getentLookupProvider struct{}

func (getentLookupProvider) Lookup(username string) (*gouser.User, error) {
	parts, err := getent("passwd", username, 7)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				return nil, gouser.UnknownUserError(username)
			}
		}
		return nil, err
	}
	return &gouser.User{
		Username: parts[0],
		Uid:      parts[2],
		Gid:      parts[3],
		HomeDir:  parts[5],
	}, nil
}

func (getentLookupProvider) LookupId(uid string) (*gouser.User, error) {
	parts, err := getent("passwd", uid, 7)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				uidNum, _ := strconv.Atoi(uid) // nolint: errcheck
				return nil, gouser.UnknownUserIdError(uidNum)
			}
		}
		return nil, err
	}
	return &gouser.User{
		Username: parts[0],
		Uid:      parts[2],
		Gid:      parts[3],
		HomeDir:  parts[5],
	}, nil
}

func (getentLookupProvider) LookupGroup(groupname string) (*gouser.Group, error) {
	parts, err := getent("group", groupname, 4)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				return nil, gouser.UnknownGroupError(groupname)
			}
		}
		return nil, err
	}

	return &gouser.Group{
		Gid:  parts[2],
		Name: parts[0],
	}, nil
}

func (getentLookupProvider) LookupGroupId(gid string) (*gouser.Group, error) {
	parts, err := getent("group", gid, 4)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				return nil, gouser.UnknownGroupIdError(gid)
			}
		}
		return nil, err
	}

	return &gouser.Group{
		Gid:  parts[2],
		Name: parts[0],
	}, nil
}

func getent(database string, name string, numParts int) ([]string, error) {
	cmd := exec.Command("getent", database, name)
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}
	parts := strings.Split(string(output), ":")
	if len(parts) < numParts {
		return nil, fmt.Errorf("'getent %s %s' invalid output: %s", database, name, output)
	}
	return parts, nil
}
