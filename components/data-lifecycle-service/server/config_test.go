package server

import (
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/data-lifecycle-service/storage"
)

func TestLoadValidConfigFromToml(t *testing.T) {
	conf, err := ConfigFromToml(pathFor("sample-config-valid.toml"))
	require.NoError(t, err)
	assert.Equal(t, "1.2.3.4", conf.ListenAddress)
	assert.Equal(t, uint16(1234), conf.Port)
	assert.Equal(t, "error", conf.LogLevel)
	assert.Equal(
		t,
		map[string]storage.ServiceConfig{
			"foo": storage.ServiceConfig{
				Address: "4.3.2.1:9999",
			},
			"data-lifecycle-service": storage.ServiceConfig{
				Address: "1.2.3.4:1234",
				Secure:  true,
			},
		},
		conf.ManagedServices)
	assert.Nil(t, conf.DataLifeCycleInfo)
}

func TestLoadValidConfigWithDataLifecyclePolicyFromToml(t *testing.T) {
	conf, err := ConfigFromToml(pathFor("sample-config-valid-with-data-lifecycle-policy.toml"))
	require.NoError(t, err)
	assert.NotNil(t, conf.DataLifeCycleInfo)
	assert.Equal(t, int32(10), conf.DataLifeCycleInfo.PurgeOlderThanDays)
}

func TestValidTimeOfDay(t *testing.T) {
	conf, err := ConfigFromToml(pathFor("sample-config-valid-time-of-day.toml"))
	require.NoError(t, err)
	assert.Equal(t, 23, conf.DailyRunAt.Hour())
	assert.Equal(t, 11, conf.DailyRunAt.Min())
	assert.Equal(t, 59, conf.DailyRunAt.Sec())
}

func TestInvalidTimeOfDay(t *testing.T) {
	_, err := ConfigFromToml(pathFor("sample-config-invalid-time.toml"))
	require.EqualError(t, err, "failed to parse config file: Invalid hour value 25")
}

func pathFor(filename string) string {
	return path.Join("testdata", filename)
}
