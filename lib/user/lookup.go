package user

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	gouser "os/user"
	"path/filepath"
	"strconv"
	"strings"
)

type UnknownGroupIdError = gouser.UnknownGroupIdError
type UnknownGroupError = gouser.UnknownGroupError
type UnknownUserError = gouser.UnknownUserError
type UnknownUserIdError = gouser.UnknownUserIdError

type User = gouser.User
type Group = gouser.Group

var DefaultLookupProvider LookupProvider = &chainLookupProvider{
	providers: []LookupProvider{
		goLookupProvider{},
		newGetentLookupProvider(),
	},
}

type LookupProvider interface {
	Lookup(username string) (*User, error)
	LookupId(uid string) (*User, error)
	LookupGroup(groupname string) (*Group, error)
	LookupGroupId(gid string) (*Group, error)
}

func Lookup(username string) (*User, error) {
	return DefaultLookupProvider.Lookup(username)
}

func LookupId(uid string) (*User, error) {
	return DefaultLookupProvider.LookupId(uid)
}

func LookupGroup(groupname string) (*Group, error) {
	return DefaultLookupProvider.LookupGroup(groupname)
}

func LookupGroupId(gid string) (*Group, error) {
	return DefaultLookupProvider.LookupGroupId(gid)
}

type chainLookupProvider struct {
	providers []LookupProvider
}

func (c *chainLookupProvider) Lookup(username string) (*User, error) {
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

func (c *chainLookupProvider) LookupId(uid string) (*User, error) {
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

func (c *chainLookupProvider) LookupGroup(groupname string) (*Group, error) {
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

func (c *chainLookupProvider) LookupGroupId(gid string) (*Group, error) {
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

func (goLookupProvider) Lookup(username string) (*User, error) {
	return gouser.Lookup(username)
}

func (goLookupProvider) LookupId(uid string) (*User, error) {
	return gouser.LookupId(uid)
}

func (goLookupProvider) LookupGroup(groupname string) (*Group, error) {
	return gouser.LookupGroup(groupname)
}

func (goLookupProvider) LookupGroupId(gid string) (*gouser.Group, error) {
	return gouser.LookupGroupId(gid)
}

type getentLookupProvider struct {
	getentPath string
}

func newGetentLookupProvider() getentLookupProvider {
	getentPath := "getent"
	if p := os.Getenv("CHEF_AUTOMATE_GETENT_PATH"); p != "" {
		getentPath = p
	} else {
		// Find a system getent
		for _, d := range []string{"/bin", "/usr/bin"} {
			p := filepath.Join(d, "getent")
			d, err := os.Stat(p)
			if err != nil {
				continue
			}
			if m := d.Mode(); !m.IsDir() && m&0111 != 0 {
				getentPath = p
				break
			}
		}
	}

	return getentLookupProvider{
		getentPath: getentPath,
	}
}

func (g getentLookupProvider) Lookup(username string) (*User, error) {
	parts, err := g.getent("passwd", username, 7)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				return nil, UnknownUserError(username)
			}
		}
		return nil, err
	}
	return &User{
		Username: parts[0],
		Uid:      parts[2],
		Gid:      parts[3],
		HomeDir:  parts[5],
	}, nil
}

func (g getentLookupProvider) LookupId(uid string) (*User, error) {
	parts, err := g.getent("passwd", uid, 7)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				uidNum, _ := strconv.Atoi(uid) // nolint: errcheck
				return nil, UnknownUserIdError(uidNum)
			}
		}
		return nil, err
	}
	return &User{
		Username: parts[0],
		Uid:      parts[2],
		Gid:      parts[3],
		HomeDir:  parts[5],
	}, nil
}

func (g getentLookupProvider) LookupGroup(groupname string) (*Group, error) {
	parts, err := g.getent("group", groupname, 4)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				return nil, UnknownGroupError(groupname)
			}
		}
		return nil, err
	}

	return &Group{
		Gid:  parts[2],
		Name: parts[0],
	}, nil
}

func (g getentLookupProvider) LookupGroupId(gid string) (*Group, error) {
	parts, err := g.getent("group", gid, 4)
	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			if exitError.ExitCode() == 2 {
				return nil, UnknownGroupIdError(gid)
			}
		}
		return nil, err
	}

	return &Group{
		Gid:  parts[2],
		Name: parts[0],
	}, nil
}

func (g getentLookupProvider) getent(database string, name string, numParts int) ([]string, error) {
	cmd := exec.Command(g.getentPath, database, name)
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
