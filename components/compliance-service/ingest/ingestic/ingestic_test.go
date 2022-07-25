package ingestic

import (
	"reflect"
	"testing"
	"time"

	elastic "github.com/olivere/elastic/v7"
)

func TestScriptForUpdatingControlIndexStatusAndEndTime(t *testing.T) {
	newStatus := ""
	nodeEndTime := time.Now().UTC()
	params := make(map[string]interface{})
	params["node_end_time"] = nodeEndTime
	params["newStatus"] = newStatus
	script := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	if(len(params.newStatus)==0){
		ctx._source.status = params.newStatus
	}
	`).Params(params)
	newStatus1 := "failed"
	params1 := make(map[string]interface{})
	params1["node_end_time"] = nodeEndTime
	params1["newStatus"] = newStatus1
	scriptNew := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	if(len(params.newStatus)==0){
		ctx._source.status = params.newStatus
	}
	`).Params(params1)
	newStatus2 := "skipped"
	params2 := make(map[string]interface{})
	params2["node_end_time"] = nodeEndTime
	params2["newStatus"] = newStatus2
	expectedScript := elastic.NewScript(`ctx._source.end_time = params.node_end_time;
	if(len(params.newStatus)==0){
		ctx._source.status = params.newStatus
	}
	`).Params(params2)

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
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := scriptForUpdatingControlIndexStatusAndEndTime(tt.args.controlStatus, tt.args.nodeStatus, tt.args.nodeEndtime); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("scriptForUpdatingControlIndexStatusAndEndTime() = %v, want %v", got, tt.want)
			}
		})
	}
}
