package pipeline

import (
	"context"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/testDB"
	"reflect"
	"testing"
)

func TestStoreOrg(t *testing.T) {
	type args struct {
		ctx      context.Context
		st       storage.Storage
		org      Org
		serverID string
	}
	tests := []struct {
		name  string
		args  args
		want  error
		want1 ActionOps
	}{
		// TODO: Add test cases.
		{name: "Test Delete Org", args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: Org{Name: "org1", FullName: "Org 1", ActionOps: Delete}, serverID: "server1"}, want: nil, want1: Delete},
		{name: "Test Store Org", args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: Org{Name: "org1", FullName: "Org 1", ActionOps: Insert}, serverID: "server1"}, want: nil, want1: Insert},
		{name: "Test Edit Org", args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: Org{Name: "org1", FullName: "Org 1", ActionOps: Update}, serverID: "server1"}, want: nil, want1: Update},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, got1 := StoreOrg(tt.args.ctx, tt.args.st, tt.args.org, tt.args.serverID)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("StoreOrg() got = %v, want %v", got, tt.want)
			}
			if got1 != tt.want1 {
				t.Errorf("StoreOrg() got1 = %v, want %v", got1, tt.want1)
			}
		})
	}
}
