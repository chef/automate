package majorupgradechecklist

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetMajorVersion(t *testing.T) {
	data := `{"automate-2":{"settings":{"index":{"version":{"created_string":"5.6.2","created":"5060299"}}}},".watches":{"settings":{"index":{"version":{"created_string":"5.6.2","created":"5060299"}}}},"node-attribute":{"settings":{"index":{"version":{"created_string":"6.8.23","created":"6082399"}}}}}`

	indexInfo, err := getOldIndexInfo([]byte(data))
	assert.NoError(t, err)
	assert.Equal(t, 1, len(indexInfo))
	assert.Equal(t, "automate-2", indexInfo[0].Name)
	assert.Equal(t, int64(5), indexInfo[0].MajorVersion)
	assert.Equal(t, "5.6.2", indexInfo[0].CreatedString)
}

func TestFormErrorMsg(t *testing.T) {
	IndexDetailsArray := []indexData{
		{Name: "abc", MajorVersion: 5, CreatedString: "5.6.4"},
		{Name: "def", MajorVersion: 4, CreatedString: "4.3.1"},
		{Name: "abc", MajorVersion: 5, CreatedString: "5.6.2"},
		{Name: "def", MajorVersion: 5, CreatedString: "5.6.4", IsDeleted: true},
	}
	errMsg := formErrorMsg(IndexDetailsArray)
	assert.Error(t, errMsg)
	assert.Equal(t, "\nUnsupported index versions. To continue with the upgrade, please reindex the indices shown below to version 6.\n- Index Name: abc, Version: 5.6.4 \n- Index Name: def, Version: 4.3.1 \n- Index Name: abc, Version: 5.6.2 \n\nFollow the guide below to learn more about reindexing:\nhttps://www.elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html", errMsg.Error())
}

func TestFindMatch(t *testing.T) {
	sourceList := []string{".automate", ".locky", "saved-searches", ".tasks"}
	targetList := []string{".automate-23423274", "automate-saved-searches", "temp.tasks", "test.locky", "comp_info.automate"}
	resp := findMatch(sourceList, targetList)
	assert.Equal(t, 5, len(resp))
}
