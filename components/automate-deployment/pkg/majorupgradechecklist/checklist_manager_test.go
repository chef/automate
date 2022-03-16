package majorupgradechecklist

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestChecklistManager(t *testing.T) {
	_, err := NewChecklistManager(nil, "3.0.0")
	assert.NoError(t, err)
	_, err = NewChecklistManager(nil, "20220221183851")
	assert.Error(t, err)
	assert.Equal(t, "invalid major version", err.Error())
}
