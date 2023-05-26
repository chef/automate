package executil_test

import (
	"testing"

	"github.com/chef/automate/lib/executil"
	"github.com/stretchr/testify/assert"
)


func TestCommand(t *testing.T) {
	f := &executil.ExecCmdServiceImp{}

	output, err := f.Command("echo", []string{"Hello"})
	assert.Contains(t, string(output), "Hello")
	assert.Equal(t, nil, err)
}