package generator

import (
	"io/ioutil"
	"os"
	"reflect"
	"strings"
	"testing"

	"github.com/chef/automate/api/external/habitat"
	"github.com/golang/protobuf/ptypes/duration"
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

	expected := &habitat.HealthCheckEvent{
		EventMetadata: &habitat.EventMetadata{
			SupervisorId: uuid,
			Application:  "example-app",
			Environment:  "qa",
			Fqdn:         "00000000-0000-0000-0000-000000000000.example",
			Site:         "test",
		},
		ServiceMetadata: &habitat.ServiceMetadata{
			ServiceGroup: "service-1.default",
			PackageIdent: "example/service-1/0.1.0/" + expectedRelease,
			UpdateConfig: &habitat.UpdateConfig{
				Strategy: habitat.UpdateStrategy_AtOnce,
				Channel:  "stable",
			},
		},
		Result:    habitat.HealthCheckResult_Warning,
		Execution: &duration.Duration{},
	}

	assert.EqualValues(t, expected, msg)

	// Test to make sure that the generator is filling in all of the values in
	// the messages. Use reflection to get an object we can interrogate about the
	// struct fields; later, we'll check if the values are the zero values for
	// those types
	msgAsValue := reflect.ValueOf(*msg)

	// These fields are expected to be the zero values for their types
	exceptions := []string{"HealthCheck", "Status"}

	for i := 0; i < msgAsValue.NumField(); i++ {
		// Attempting to access a private struct field via reflection panics, so
		// skip things we are not allowed to see:
		if msgAsValue.Field(i).CanInterface() {
			fieldName := msgAsValue.Type().Field(i).Name
			// skip internal protobuf fields that start with "XXX_"
			if !strings.HasPrefix(fieldName, "XXX_") {

				isException := false
				for _, exception := range exceptions {
					if fieldName == exception {
						isException = true
						break
					}
				}
				if isException {
					continue
				}

				// the value of the struct field
				fieldValue := msgAsValue.Field(i).Interface()
				// get a zero value for that type
				valType := msgAsValue.Field(i).Type()
				zeroForType := reflect.Zero(valType).Interface()

				// If the value of the field is the zero value, and it's not in our
				// exceptions list, then we probably added a new field to
				// habitat.HealthCheckEvent and forgot to update the load generator code
				// to match it.
				assert.NotEqualf(t,
					zeroForType, fieldValue,
					"Field %q of the HealthCheckEvent message was set to the zero value for its type (%T); MessagePrototype.CreateMessage() may need to be updated", fieldName, fieldValue)
			}
		}
	}
}
