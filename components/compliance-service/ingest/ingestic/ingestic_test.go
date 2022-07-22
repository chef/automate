package ingestic

import (
	"testing"
	"time"

	elastic "github.com/olivere/elastic/v7"
	"github.com/stretchr/testify/assert"
)

func TestScriptForUpdatingControlIndexStatusAndEndTime(t *testing.T) {
	controlStatus := "failed"
	nodeStatus := "passed"
	nodeEndTime := time.Now().UTC()

	params := make(map[string]interface{})
	var newStatus string
	params["node_end_time"] = nodeEndTime
	params["newStatus"] = newStatus

	expectedScript := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	if(params.newStatus){
		ctx._source.status = params.newStatus
	}
	`).Params(params)
	script := scriptForUpdatingControlIndexStatusAndEndTime(controlStatus, nodeStatus, nodeEndTime)

	assert.Equal(t, expectedScript, script)
}

func TestScriptForUpdatingControlIndexStatusAndEndStatus(t *testing.T) {
	controlStatus := "waived"
	nodeStatus := "skipped"
	nodeEndTime := time.Now().UTC()

	params := make(map[string]interface{})
	newStatus := "skipped"
	params["node_end_time"] = nodeEndTime
	params["newStatus"] = newStatus

	expectedScript := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	if(params.newStatus){
		ctx._source.status = params.newStatus
	}
	`).Params(params)
	script := scriptForUpdatingControlIndexStatusAndEndTime(controlStatus, nodeStatus, nodeEndTime)

	assert.Equal(t, expectedScript, script)
}
