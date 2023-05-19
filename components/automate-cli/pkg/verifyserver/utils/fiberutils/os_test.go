package fiberutils_test

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/stretchr/testify/assert"
)

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
