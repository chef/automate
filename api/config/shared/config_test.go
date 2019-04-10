package shared

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAddKeyMissing(t *testing.T) {
	c := NewInvalidConfigError()
	c.AddMissingKey("foo")

	assert.EqualValues(t, c.MissingKeys(), []string{"foo"})
}

func TestAddInvalidValue(t *testing.T) {
	c := NewInvalidConfigError()
	c.AddInvalidValue("foo", "bar")

	assert.EqualValues(t, c.InvalidValues(), map[string]string{"foo": "bar"})
}
