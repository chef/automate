package upgradeinspectorv4

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

const (
	INDEX_LIST = `comp-2-run-info
node-attribute
node-state-7
node-1-run-info
comp-3-profiles
eventfeed-2-feeds`
	TAB          = "                "
	INSIDETAB    = "                        "
	INDEX_PROMPT = `
%[1]vPlease choose from options below:
%[1]v1. Delete these indices and proceed with upgrade.
%[1]v2. Exit the upgrade process, manually re-index the indices and upgrade Chef Automate later on.

%[1]vFor more information on reindexing, visit: https://www. elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html

%[1]vEnter your choice (1/2):`
	FOUND_OLD    = "✖  [Failed]\tElasticsearch indices are in version 6\n\n                [Error] Below indices are from an older version of Elasticsearch"
	FOUND_OLD_A1 = "✖  [Failed]\tElasticsearch indices are in version 6\n\n                [Error] Below indices are from an older version of Elasticsearch from Chef Automate 1"
)

func IsExternal(timeout int64) bool {
	return false
}

func ExecRequestNonAutomate(url, methodType string, requestBody io.Reader) ([]byte, error) {
	if strings.Contains(url, "index.version") {
		return []byte(`{"node-attribute":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},"comp-2-run-info":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
	} else if strings.Contains(url, "_cluster/stats") {
		return []byte(`{"indices":{"shards":{"total":51}}}`), nil
	} else if strings.Contains(url, "indices") {
		return []byte(INDEX_LIST), nil
	} else {
		return []byte{}, nil
	}
}

func ExecRequestOldAutomate(url, methodType string, requestBody io.Reader) ([]byte, error) {
	if strings.Contains(url, "index.version") {
		return []byte(`{".automate":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},".locky":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
	} else if strings.Contains(url, "indices") {
		return []byte(INDEX_LIST), nil
	} else {
		return []byte{}, nil
	}
}

func ExecRequestOldAutomateAndNonAutomate(url, methodType string, requestBody io.Reader) ([]byte, error) {
	if strings.Contains(url, "index.version") {
		return []byte(`{".non-automate":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},".automate":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},".locky":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
	} else if strings.Contains(url, "indices") {
		return []byte(INDEX_LIST), nil
	} else {
		return []byte{}, nil
	}
}

func ExecRequestNoOldIndex(url, methodType string, requestBody io.Reader) ([]byte, error) {
	if strings.Contains(url, "index.version") {
		return []byte(`{"node-attribute":{"settings":{"index":{"version":{"created_string":"6.8.23","created":"6082399"}}}},"comp-2-run-info":{"settings":{"index":{"version":{"created_string":"6.8.23","created":"6082399"}}}}}`), nil
	} else if strings.Contains(url, "indices") {
		return []byte(INDEX_LIST), nil
	} else {
		return []byte{}, nil
	}
}

func ExecRequestError(url, methodType string, requestBody io.Reader) ([]byte, error) {
	return nil, errors.New("Unreachable")
}

func GetESBasePath(timeout int64) string {
	return "http://localhost:10144/"
}

func TestShowInfo(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNonAutomate,
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
	tw := majorupgrade_utils.NewCustomWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNonAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	indexArr, err := ei.fetchOldIndexInfo()
	indexRespByte, err := json.Marshal(indexArr)
	assert.NoError(t, err)
	indexByte, err := json.Marshal(IndexData{Name: "comp-2-run-info", MajorVersion: 5, CreatedString: "5.8.23", IsDeleted: false})
	assert.NoError(t, err)
	assert.Contains(t, string(indexRespByte), string(indexByte))
}

func TestGetShortInfo(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNonAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	msgESIndex := ei.GetShortInfo()
	expected := []string{"Elasticsearch indices are in version 6"}
	assert.Equal(t, expected, msgESIndex)
}

func TestShowErrorListOldAutomateIndices(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNonAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	ei.automateOldIndices = []string{"1", "2", "3", "4"}
	ei.showErrorListOldAutomateIndices()
	expected := fmt.Sprintf("%[1]v[Error] Below indices are from an older version of Elasticsearch from Chef Automate 1\n%[2]v1\n%[2]v2\n%[2]v3\n%[2]v4\n\n", TAB, INSIDETAB)
	assert.Equal(t, expected, tw.Output())
}

func TestShowErrorListOldOtherIndices(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNonAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	ei.otherOldIndices = []string{"1", "2", "3", "4"}
	ei.showErrorListOldOtherIndices()
	expected := fmt.Sprintf("%[1]v[Error] Below indices are from an older version of Elasticsearch\n%[2]v1\n%[2]v2\n%[2]v3\n%[2]v4\n\n", TAB, INSIDETAB)
	assert.Equal(t, expected, tw.Output())
}

func TestInspectWithOldNonAutomateIndicesWithExit(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriterWithInputs("2")
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNonAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	expectederr := UPGRADE_TERMINATED
	err := ei.Inspect()
	if assert.Error(t, err) {
		assert.EqualError(t, err, expectederr)
	}
	expectedout1 := "✖  [Failed]\tElasticsearch indices are in version 6\n\n                [Error] Below indices are from an older version of Elasticsearch"
	expectedout2 := fmt.Sprintf(INDEX_PROMPT, TAB)
	expectedout3 := `node-attribute`
	expectedout4 := `comp-2-run-info`

	assert.Contains(t, tw.Output(), expectedout1)
	assert.Contains(t, tw.Output(), expectedout2)
	assert.Contains(t, tw.Output(), expectedout3)
	assert.Contains(t, tw.Output(), expectedout4)
}

func TestInspectWithOldAutomateIndicesWithExit(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriterWithInputs("2")
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestOldAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	expectederr := UPGRADE_TERMINATED
	err := ei.Inspect()
	if assert.Error(t, err) {
		assert.EqualError(t, err, expectederr)
	}
	expectedout1 := FOUND_OLD
	expectedout2 := fmt.Sprintf(INDEX_PROMPT, TAB)
	expectedout3 := `.automate`
	expectedout4 := `.locky`

	assert.Contains(t, tw.Output(), expectedout1)
	assert.Contains(t, tw.Output(), expectedout2)
	assert.Contains(t, tw.Output(), expectedout3)
	assert.Contains(t, tw.Output(), expectedout4)
}

func TestInspectWithOldAutomateAndNonAutomateIndicesWithExit(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriterWithInputs("2")
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestOldAutomateAndNonAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))
	expectederr := UPGRADE_TERMINATED
	err := ei.Inspect()
	if assert.Error(t, err) {
		assert.EqualError(t, err, expectederr)
	}
	expectedout1 := `✖  [Failed]	Elasticsearch indices are in version 6

                [Error] Below indices are from an older version of Elasticsearch from Chef Automate 1
`
	expectedout2 := `
                Please choose from options below:
                1. Delete these indices and proceed with upgrade.
                2. Exit the upgrade process, manually re-index the indices and upgrade Chef Automate later on.

                For more information on reindexing, visit: https://www. elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html

                Enter your choice (1/2):`
	expectedout3 := `.automate`
	expectedout4 := `.locky`
	expectedout5 := `.non-automate`

	assert.Contains(t, tw.Output(), expectedout1)
	assert.Contains(t, tw.Output(), expectedout2)
	assert.Contains(t, tw.Output(), expectedout3)
	assert.Contains(t, tw.Output(), expectedout4)
	assert.Contains(t, tw.Output(), expectedout5)
}

func TestInspectWithOldIndicesAndDelete(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriterWithInputs("1")
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNonAutomate,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))

	err := ei.Inspect()
	assert.NoError(t, err)
	expectedout := "✔ Old Elasticsearch indices deleted successfully\n\n ✔  [Passed]\tElasticsearch indices are in version 6"
	assert.Contains(t, tw.Output(), expectedout)
}

func TestInspectWithNoOldIndices(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestNoOldIndex,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))

	err := ei.Inspect()
	assert.NoError(t, err)
	expectedout := "[Passed]\tElasticsearch indices are in version 6"
	assert.Contains(t, tw.Output(), expectedout)
}

func TestInspectWithFailedApiCalls(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc:             ExecRequestError,
		GetESBasePathFunc:           GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))

	err := ei.Inspect()
	assert.Error(t, err)
	expectedout := "error while getting list of indices"
	assert.Contains(t, err.Error(), expectedout)
}

func TestInspectWithFailedDelete(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriterWithInputs("1")
	mockUtil := &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: IsExternal,
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			if methodType == "DELETE" {
				return nil, errors.New("unexpected")
			} else if strings.Contains(url, "index.version") {
				return []byte(`{".non-automate":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},".automate":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},".locky":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
			} else if strings.Contains(url, "indices") {
				return []byte(INDEX_LIST), nil
			} else {
				return []byte{}, nil
			}
		},
		GetESBasePathFunc: GetESBasePath,
	}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))

	err := ei.Inspect()
	assert.Error(t, err)
	err = ei.ExitHandler()
	assert.NoError(t, err)
	expectedout := `[Error] Failed to delete index: unexpected`
	assert.Contains(t, tw.Output(), expectedout)
}

func TestShouldDelete(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriterWithInputs("y", "w", "1")
	mockUtil := &MockUpgradeV4UtilsImp{}
	ei := NewESIndexInspection(tw.CliWriter, mockUtil, GetESBasePath(10))

	response := ei.shouldDelete()
	t.Log(tw.Output())
	assert.True(t, response)
}
