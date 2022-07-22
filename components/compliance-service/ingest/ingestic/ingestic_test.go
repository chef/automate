package ingestic

import (
	"reflect"
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

func Test_scriptForUpdatingControlIndexStatusAndEndTime(t *testing.T) {
	params := make(map[string]interface{})
	params["node_end_time"] = time.Now().UTC()

	expectedScript1 := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	if(params.newStatus){
		ctx._source.status = params.newStatus
	}
	`).Params(params)
	
	expectedScript2 := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	if(params.newStatus){
		ctx._source.status = params.newStatus
	}
	`).Params(params)

	type args struct {
		controlStatus string
		nodeStatus    string
		nodeEndtime   time.Time
	}
	tests := []struct {
		name string
		args args
		want *elastic.Script
	}{
		// TODO: Add test cases.
		{name: "test 1", args: args{controlStatus: "", nodeStatus: "", nodeEndtime: time.Now()}, want: expectedScript1},
		{name: "test 2", args: args{controlStatus: "", nodeStatus: "", nodeEndtime: time.Now()}, want: expectedScript2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := scriptForUpdatingControlIndexStatusAndEndTime(tt.args.controlStatus, tt.args.nodeStatus, tt.args.nodeEndtime); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("scriptForUpdatingControlIndexStatusAndEndTime() = %v, want %v", got, tt.want)
			}
		})
	}
}
