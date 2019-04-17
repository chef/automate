package generator

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLoadProfileFile(t *testing.T) {
	f, err := ioutil.TempFile("", "TestLoadProfileFile-*.toml")
	require.NoError(t, err)
	defer os.Remove(f.Name())

	f.WriteString(builtinConfigToml)
	f.Close()

	profileCfg, err := ProfileFromFile(f.Name())
	require.NoError(t, err)

	assert.IsType(t, &LoadProfileCfg{}, profileCfg)
}

func TestDefaultProfileIsViable(t *testing.T) {
	profileCfg, err := BuiltinConfig()
	require.NoError(t, err)
	runner, err := profileCfg.BuildRunner()
	require.NoError(t, err)

	assert.Len(t, runner.SupervisorGroups, 1)

	supGroup := runner.SupervisorGroups[0]

	require.Len(t, supGroup.MessagePrototypes, 3)

	uuid := "00000000-0000-0000-0000-000000000000"

	msg := supGroup.MessagePrototypes[0].CreateMessage(uuid)

	expectedRelease := now.Format("20060102150405")

	expected := &applications.HabService{
		SupervisorId: uuid,
		Group:        "default",
		PkgIdent: &applications.PackageIdent{
			Origin:  "example",
			Name:    "service-1",
			Version: "0.1.0",
			Release: expectedRelease,
		},
	}

	assert.Equal(t, expected, msg)
}
