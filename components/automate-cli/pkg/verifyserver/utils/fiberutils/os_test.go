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
