package trigger

import (
	"reflect"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

func TestRunCheckWithEndPointSpecified(t *testing.T) {
	type args struct {
		endPoint string
		log      logger.Logger
		reqList  []models.NodeIpRequest
		method   string
	}
	tests := []struct {
		name string
		args args
		want []models.CheckTriggerResponse
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := RunCheckWithEndPointSpecified(tt.args.endPoint, tt.args.log, tt.args.reqList, tt.args.method); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("RunCheckWithEndPointSpecified() = %v, want %v", got, tt.want)
			}
		})
	}
}
