package workflow_server

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}
