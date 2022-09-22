package upgradeinspectorv4

import (
	"errors"
	"io"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDisableSharding(t *testing.T) {
	tw := NewTestWriter()
	ds := NewDisableShardingInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return []byte{}, nil
		},
	})
	err := ds.Inspect()
	assert.NoError(t, err)
}

func TestDisableShardingError(t *testing.T) {
	tw := NewTestWriter()
	ds := NewDisableShardingInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return nil, errors.New("Unreachable")
		},
	})
	err := ds.Inspect()
	assert.Error(t, err)
}
