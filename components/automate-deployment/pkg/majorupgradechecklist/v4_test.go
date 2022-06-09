package majorupgradechecklist

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetMajorVersion(t *testing.T) {
	versionData := []byte(`{"comp-2-run-info":{"settings":{"index":{"creation_date_string":"2022-06-06T10:50:55.477Z","refresh_interval":"1s","number_of_shards":"5","provided_name":"comp-2-run-info","creation_date":"1654512655477","number_of_replicas":"1","uuid":"cYlQeyHNRn2mnsvIRZ1-rg","version":{"created_string":"1.2.4","created":"135238227"}}}}}`)
	majorVersion, version, err := getMajorVersion(versionData, "comp-2-run-info")
	assert.NoError(t, err)
	assert.Equal(t, int64(1), majorVersion)
	assert.Equal(t, "1.2.4", version)

	versionData = []byte(`{"comp-2-run-info":{"settings":{"index":{"creation_date_string":"2022-06-06T10:50:55.477Z","refresh_interval":"1s","number_of_shards":"5","provided_name":"comp-2-run-info","creation_date":"1654512655477","number_of_replicas":"1","uuid":"cYlQeyHNRn2mnsvIRZ1-rg","version":{"created_string":"7.2.4","created":"135238227"}}}}}`)
	majorVersion, version, err = getMajorVersion(versionData, "comp-2-run-info")
	assert.NoError(t, err)
	assert.Equal(t, int64(7), majorVersion)
	assert.Equal(t, "7.2.4", version)

	versionData = []byte(`{"comp-2-run-info":{"settings":{"index":{"creation_date_string":"2022-06-06T10:50:55.477Z","refresh_interval":"1s","number_of_shards":"5","provided_name":"comp-2-run-info","creation_date":"1654512655477","number_of_replicas":"1","uuid":"cYlQeyHNRn2mnsvIRZ1-rg","version":{"created":"135238227"}}}}}`)
	majorVersion, version, err = getMajorVersion(versionData, "comp-2-run-info")
	assert.Error(t, err)
	assert.Equal(t, int64(-1), majorVersion)
	assert.Equal(t, "", version)

	versionData = []byte(`{"comp-2-run-info":{"settings":{"index":{"creation_date_string":"2022-06-06T10:50:55.477Z","refresh_interval":"1s","number_of_shards":"5","provided_name":"comp-2-run-info","creation_date":"1654512655477","number_of_replicas":"1","uuid":"cYlQeyHNRn2mnsvIRZ1-rg"}}}}`)
	majorVersion, version, err = getMajorVersion(versionData, "comp-2-run-info")
	assert.Error(t, err)
	assert.Equal(t, int64(-1), majorVersion)
	assert.Equal(t, "", version)
}

func TestFormErrorMsg(t *testing.T) {
	IndexDetailsArray := []indexDetails{
		{Name: "abc", Version: "5.6.4"},
		{Name: "def", Version: "4.3.1"},
		{Name: "abc", Version: "5.6.2"},
	}
	errMsg := formErrorMsg(IndexDetailsArray)
	assert.Error(t, errMsg)
	assert.Equal(t, "\nUnsupported index versions. To continue with the upgrade, please reindex the indices shown below to version 6.\n- Index Name: abc, Version: 5.6.4 \n- Index Name: def, Version: 4.3.1 \n- Index Name: abc, Version: 5.6.2 \n\nFollow the guide below to learn more about reindexing:\nhttps://www.elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html", errMsg.Error())
}
