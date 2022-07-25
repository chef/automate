package ingestic

import (
	"reflect"
	"testing"
	"time"

	elastic "github.com/olivere/elastic/v7"
)

func TestScriptForUpdatingControlIndexStatusAndEndTime(t *testing.T) {
	nodeEndTime := time.Now().UTC()
	params := make(map[string]interface{})
	params["node_end_time"] = nodeEndTime
	script := elastic.NewScript(`ctx._source.end_time = params.node_end_time`).Params(params)
	newStatus := "failed"
	params1 := make(map[string]interface{})
	params1["node_end_time"] = nodeEndTime
	params1["newStatus"] = newStatus
	scriptNew := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	ctx._source.status = params.newStatus`).Params(params1)
	newStatus1 := "skipped"
	params2 := make(map[string]interface{})
	params2["node_end_time"] = nodeEndTime
	params2["newStatus"] = newStatus1
	expectedScript := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	ctx._source.status = params.newStatus`).Params(params2)
	newStatus2 := "skipped"
	params3 := make(map[string]interface{})
	params3["node_end_time"] = nodeEndTime
	params3["newStatus"] = newStatus2
	newScript := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	ctx._source.status = params.newStatus`).Params(params3)

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
		{name: "test 1", args: args{controlStatus: "failed", nodeStatus: "passed", nodeEndtime: nodeEndTime}, want: script},
		{name: "test 2", args: args{controlStatus: "passed", nodeStatus: "failed", nodeEndtime: nodeEndTime}, want: scriptNew},
		{name: "test 3", args: args{controlStatus: "passed", nodeStatus: "skipped", nodeEndtime: nodeEndTime}, want: expectedScript},
		{name: "test 4", args: args{controlStatus: "waived", nodeStatus: "skipped", nodeEndtime: nodeEndTime}, want: newScript},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := scriptForUpdatingControlIndexStatusAndEndTime(tt.args.controlStatus, tt.args.nodeStatus, tt.args.nodeEndtime); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("scriptForUpdatingControlIndexStatusAndEndTime() = %v, want %v", got, tt.want)
			}
		})
	}
}
