package trigger

import (
	"net/http"
	reflect "reflect"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

func TestCheckTrigger_HardwareResourceCountCheck(t *testing.T) {
	type fields struct {
		Logger logger.ILogger
	}
	type args struct {
		config models.Config
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   models.CheckTriggerResponse
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hrc := &CheckTrigger{
				Logger: tt.fields.Logger,
			}
			if got := hrc.HardwareResourceCountCheck(tt.args.config); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("CheckTrigger.HardwareResourceCountCheck() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_post(t *testing.T) {
	type args struct {
		url  string
		body interface{}
	}
	tests := []struct {
		name    string
		args    args
		want    *http.Response
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := post(tt.args.url, tt.args.body)
			if (err != nil) != tt.wantErr {
				t.Errorf("post() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("post() = %v, want %v", got, tt.want)
			}
		})
	}
}
