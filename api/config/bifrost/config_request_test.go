package bifrost

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestDefaultConfigRequest(t *testing.T) {
	defaultVal := DefaultConfigRequest()

	require.Equal(t, int32(10202), defaultVal.GetV1().GetSys().GetNetwork().GetPort().GetValue())
	require.Equal(t, string("127.0.0.1"), defaultVal.GetV1().GetSys().GetNetwork().GetListenIp().GetValue())

	require.Equal(t, string("info"), defaultVal.GetV1().GetSys().GetLog().GetLevel().GetValue())
	require.Equal(t, int64(104857600), defaultVal.GetV1().GetSys().GetLog().GetRotationMaxBytes().GetValue())
	require.Equal(t, int32(10), defaultVal.GetV1().GetSys().GetLog().GetRotationMaxFiles().GetValue())
	require.Equal(t, int32(1000), defaultVal.GetV1().GetSys().GetLog().GetMaxErrorLogsPerSecond().GetValue())
	require.Equal(t, bool(true), defaultVal.GetV1().GetSys().GetLog().GetExtendedPerfLog().GetValue())

	require.Equal(t, int32(5000), defaultVal.GetV1().GetSys().GetSql().GetTimeout().GetValue())
	require.Equal(t, int32(10), defaultVal.GetV1().GetSys().GetSql().GetPoolInitSize().GetValue())
	require.Equal(t, int32(20), defaultVal.GetV1().GetSys().GetSql().GetPoolMaxSize().GetValue())
	require.Equal(t, int32(50), defaultVal.GetV1().GetSys().GetSql().GetPoolQueueMax().GetValue())
	require.Equal(t, int32(2000), defaultVal.GetV1().GetSys().GetSql().GetPoolQueueTimeout().GetValue())
}
