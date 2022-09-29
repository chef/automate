package migratorv4

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPatchOpensearchConfig(t *testing.T) {
	impl := NewMigratorV4Utils()
	_, _, err := impl.PatchOpensearchConfig(&ESSettings{})
	assert.Error(t, err)
}

func TestAutomateFQDN(t *testing.T) {
	impl := NewMigratorV4Utils()
	str := impl.GetAutomateFQDN(10)
	assert.Equal(t, "http://path.local.automate.instance.io", str)
}
func TestExecCommand(t *testing.T) {
	impl := NewMigratorV4Utils()
	err := impl.ExecuteCommand("/dsds", []string{"a"}, "")
	assert.Error(t, err)
}
func TestUpdatePostCheclist(t *testing.T) {
	impl := NewMigratorV4Utils()
	err := impl.UpdatePostChecklistFile("dsdsd", "/hab/fd")
	assert.Error(t, err)
}
