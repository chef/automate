package fiberutils_test

import (
	"os/user"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/stretchr/testify/assert"
)

func TestCommand(t *testing.T) {
	f := &fiberutils.ExecCmdServiceImp{}

	output, err := f.Command("echo", []string{"Hello"})
	assert.Contains(t, string(output), "Hello")
	assert.Equal(t, nil, err)
}

func TestLookup(t *testing.T) {
	s := &fiberutils.UserCmdServiceImp{}
	username := "hab"
	u, err := s.Lookup(username)
	if u == nil {
		assert.Equal(t, user.UnknownUserError(username), err)
	}
}

func TestLookupGroup(t *testing.T) {
	s := &fiberutils.UserCmdServiceImp{}
	groupname := "hab"
	u, err := s.LookupGroup(groupname)
	if u == nil {
		assert.Equal(t, user.UnknownGroupError("hab"), err)
	}
}

func TestExecuteShellCommand(t *testing.T) {
	out, err := fiberutils.ExecuteShellCommand("echo 'Hello World'")
	assert.NoError(t, err)
	assert.Equal(t, "Hello World\n", string(out))
}

func TestCheckPath(t *testing.T) {
	type args struct {
		cmd string
	}
	tests := []struct {
		description      string
		args             args
		expectedResponse string
		expectedError    string
		wantErr          bool
	}{
		{
			description: "If the command is present on the path",
			args: args{
				cmd: "mkdir",
			},
			expectedResponse: "/bin/mkdir",
			expectedError:    "",
			wantErr:          false,
		},
		{
			description: "If the command is not present on the path",
			args: args{
				cmd: "wrongcommand",
			},
			expectedResponse: "",
			expectedError:    `exec: "wrongcommand": executable file not found in $PATH`,
			wantErr:          true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got, err := fiberutils.CheckPath(tt.args.cmd)
			if tt.wantErr {
				assert.Equal(t, tt.expectedError, err.Error())
				return
			}
			assert.Contains(t, got, tt.expectedResponse)
		})
	}
}
