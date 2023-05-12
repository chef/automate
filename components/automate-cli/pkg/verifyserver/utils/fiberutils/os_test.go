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
	}{
		{
			description: "If the cammand is present on the path",
			args: args{
				cmd: "mkdir",
			},
			expectedResponse: "/bin/mkdir",
			expectedError: "",
		},
		{
			description: "If the cammand is not present on the path",
			args: args{
				cmd: "wrongcammand",
			},
			expectedResponse: "",
			expectedError: `exec: "wrongcammand": executable file not found in $PATH`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got, err := fiberutils.CheckPath(tt.args.cmd)
			if (err != nil) {
				assert.Equal(t,tt.expectedError,err.Error())
				return
			}
			assert.Equal(t,got,tt.expectedResponse)
		})
	}
}
