package status

import (
	"context"
	"github.com/chef/automate/api/interservice/compliance/status"
	"github.com/chef/automate/components/compliance-service/dao/testDB"
	pb "github.com/golang/protobuf/ptypes/empty"
	"reflect"
	"testing"
)

func TestServer_GetControlIndexMigrationStatus(t *testing.T) {
	type fields struct {
		MigrationStatus         *status.MigrationStatus
		MigrationChannel        chan status.LogEntry
		DB                      testDB.UpgradeDBTest
		EnableEnhancedReporting bool
	}
	type args struct {
		ctx   context.Context
		empty *pb.Empty
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *status.ControlIndexMigrationStatus
		wantErr bool
	}{
		{
			name: "Test Not configured status from db",
			fields: fields{
				EnableEnhancedReporting: false,
			},
			args: args{
				ctx:   context.Background(),
				empty: nil,
			},
			want:    &status.ControlIndexMigrationStatus{Status: status.ControlIndexMigrationStatus_NOTCONFIGURED},
			wantErr: false,
		},
		{
			name: "Test Not started status from db",
			fields: fields{
				EnableEnhancedReporting: true,
				DB:                      testDB.UpgradeDBTest{NeedNotStartedStatus: true},
			},
			args: args{
				ctx:   context.Background(),
				empty: nil,
			},
			want:    &status.ControlIndexMigrationStatus{Status: status.ControlIndexMigrationStatus_NOTSTARTED},
			wantErr: false,
		},
		{
			name: "Test In Progress status from db",
			fields: fields{
				EnableEnhancedReporting: true,
				DB:                      testDB.UpgradeDBTest{NeedInProgressStatus: true},
			},
			args: args{
				ctx:   context.Background(),
				empty: nil,
			},
			want:    &status.ControlIndexMigrationStatus{Status: status.ControlIndexMigrationStatus_INPROGRESS},
			wantErr: false,
		},
		{
			name: "Test Completed status from db",
			fields: fields{
				EnableEnhancedReporting: true,
				DB:                      testDB.UpgradeDBTest{NeedCompletedStatus: true},
			},
			args: args{
				ctx:   context.Background(),
				empty: nil,
			},
			want:    &status.ControlIndexMigrationStatus{Status: status.ControlIndexMigrationStatus_COMPLETED},
			wantErr: false,
		},
		{
			name: "Test Error status from db",
			fields: fields{
				EnableEnhancedReporting: true,
				DB:                      testDB.UpgradeDBTest{NeedError: true},
			},
			args: args{
				ctx:   context.Background(),
				empty: nil,
			},
			want:    nil,
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			srv := &Server{
				MigrationStatus:         tt.fields.MigrationStatus,
				MigrationChannel:        tt.fields.MigrationChannel,
				DB:                      tt.fields.DB,
				EnableEnhancedReporting: tt.fields.EnableEnhancedReporting,
			}
			got, err := srv.GetControlIndexMigrationStatus(tt.args.ctx, tt.args.empty)
			if (err != nil) != tt.wantErr {
				t.Errorf("GetControlIndexMigrationStatus() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("GetControlIndexMigrationStatus() got = %v, want %v", got, tt.want)
			}
		})
	}
}
