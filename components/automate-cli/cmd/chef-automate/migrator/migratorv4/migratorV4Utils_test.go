package migratorV4

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPatchOpensearchConfig(t *testing.T) {
	impl := NewMigratorV4Utils()
	_, _, err := impl.PatchOpensearchConfig(&ESSettings{})
	assert.Error(t, err)
}
