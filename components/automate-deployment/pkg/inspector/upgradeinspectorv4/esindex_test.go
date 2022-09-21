package upgradeinspectorv4

import (
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func IsExternal() bool {
	return false
}

func ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error) {
	if strings.Contains(url, "index.version") {
		return []byte(`{"node-attribute":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},"comp-2-run-info":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
	} else if strings.Contains(url, "indices") {
		return []byte(`comp-2-run-info
	node-attribute
	node-state-7
	node-1-run-info
	comp-3-profiles
	eventfeed-2-feeds`), nil
	} else {
		return []byte{}, nil
	}
}

func GetESBasePath(timeout int64) string {
	return "http://localhost:10144/"
}

func TestShowInfo(t *testing.T) {
	tw := NewTestWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequest,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	index := 3
	ei.ShowInfo(&index)
	expected := ""
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 3, index)
}

func TestFetchOldIndexInfo(t *testing.T) {
	tw := NewTestWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequest,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	indexArr, err := ei.fetchOldIndexInfo()
	allIndexArr := []IndexData{{Name: "node-attribute", MajorVersion: 5, CreatedString: "5.8.23", IsDeleted: false}, {Name: "comp-2-run-info", MajorVersion: 5, CreatedString: "5.8.23", IsDeleted: false}}
	assert.NoError(t, err)
	assert.Equal(t, allIndexArr, indexArr)
}

func TestGetShortInfo(t *testing.T) {
	tw := NewTestWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequest,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	msgESIndex := ei.GetShortInfo()
	expected := []string{"Elasticsearch indices are in version 6"}
	assert.Equal(t, expected, msgESIndex)
}

func TestShowErrorListOldAutomateIndices(t *testing.T) {
	tw := NewTestWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequest,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	ei.automateOldIndices = []string{"1", "2", "3", "4"}
	ei.showErrorListOldAutomateIndices()
	expected := "[Error] Below indices are from an older version of Elasticsearch from Chef Automate 1\n        1\n        2\n        3\n        4\n\n"
	assert.Equal(t, expected, tw.Output())
}

func TestShowErrorListOldOtherIndices(t *testing.T) {
	tw := NewTestWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequest,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	ei.otherOldIndices = []string{"1", "2", "3", "4"}
	ei.showErrorListOldOtherIndices()
	expected := "[Error] Below indices are from an older version of Elasticsearch\n        1\n        2\n        3\n        4\n\n"
	assert.Equal(t, expected, tw.Output())
}
