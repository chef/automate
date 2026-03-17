package upgradeinspectorv5

import (
	"errors"
	"io"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestDisableSharding(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewDisableShardingInspection(tw.CliWriter, &MockUpgradeV5UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return []byte{}, nil
		},
	})
	err := ds.Inspect()
	assert.NoError(t, err)
}

func TestDisableShardingError(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewDisableShardingInspection(tw.CliWriter, &MockUpgradeV5UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return nil, errors.New("Unreachable")
		},
	})
	err := ds.Inspect()
	assert.Error(t, err)
}

func TestDisableShardingErrorExitHandlerMessage(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewDisableShardingInspection(tw.CliWriter, &MockUpgradeV5UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return nil, errors.New("Unreachable")
		},
	})
	err := ds.Inspect()
	assert.Error(t, err)
	err = ds.ExitHandler()
	assert.NoError(t, err)
	expected := `[Error] Failed to disable sharding: Unreachable`
	assert.Contains(t, tw.Output(), expected)
}
