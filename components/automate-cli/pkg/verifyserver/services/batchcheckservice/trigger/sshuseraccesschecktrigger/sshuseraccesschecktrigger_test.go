package trigger

import (
	reflect "reflect"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

func TestNewSshUserAccessCheck(t *testing.T) {
	type args struct {
		log  logger.Logger
		port string
	}
	tests := []struct {
		name string
		args args
		want *SshUserAccessCheck
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := NewSshUserAccessCheck(tt.args.log, tt.args.port); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("NewSshUserAccessCheck() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSshUserAccessCheck_Run(t *testing.T) {
	type fields struct {
		host string
		port string
		log  logger.Logger
	}
	type args struct {
		config models.Config
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   map[string]models.CheckTriggerResponse
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ss := &SshUserAccessCheck{
				host: tt.fields.host,
				port: tt.fields.port,
				log:  tt.fields.log,
			}
			if got := ss.Run(tt.args.config); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("SshUserAccessCheck.Run() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSshUserAccessCheck_TriggerCheckAndFormatOutput(t *testing.T) {
	type fields struct {
		host string
		port string
		log  logger.Logger
	}
	type args struct {
		host   string
		body   interface{}
		output chan<- models.CheckTriggerResponse
	}
	tests := []struct {
		name   string
		fields fields
		args   args
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ss := &SshUserAccessCheck{
				host: tt.fields.host,
				port: tt.fields.port,
				log:  tt.fields.log,
			}
			ss.TriggerCheckAndFormatOutput(tt.args.host, tt.args.body, tt.args.output)
		})
	}
}

func TestSshUserAccessCheck_TriggerSshUserAccessCheck(t *testing.T) {
	type fields struct {
		host string
		port string
		log  logger.Logger
	}
	type args struct {
		body interface{}
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *models.CheckTriggerResponse
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ss := &SshUserAccessCheck{
				host: tt.fields.host,
				port: tt.fields.port,
				log:  tt.fields.log,
			}
			got, err := ss.TriggerSshUserAccessCheck(tt.args.body)
			if (err != nil) != tt.wantErr {
				t.Errorf("SshUserAccessCheck.TriggerSshUserAccessCheck() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("SshUserAccessCheck.TriggerSshUserAccessCheck() = %v, want %v", got, tt.want)
			}
		})
	}
}
