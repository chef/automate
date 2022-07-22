package migrations

import (
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestPollDataForFlags(t *testing.T) {

	type args struct {
		upgrade   Upgrade
		NeedError bool
	}

	tests := []struct {
		name      string
		args      args
		wantError bool
		error     error
	}{
		{
			name: "Test False status from db",
			args: args{upgrade: Upgrade{
				storage:         &pgdb.UpgradeDbTest{},
				cerealInterface: &CerealInterfaceTest{NeedError: false},
			}},
			wantError: false,
		},
		{
			name: "Test error for day latest flag from db",
			args: args{upgrade: Upgrade{
				storage:         &pgdb.UpgradeDbTest{Error: true},
				cerealInterface: &CerealInterfaceTest{NeedError: false},
			}},
			wantError: true,
			error:     errors.New("Unable to get the status of day latest flag: Unable to fetch status from database"),
		},
		{
			name: "Test error for control index flag from db",
			args: args{upgrade: Upgrade{
				storage:         &pgdb.UpgradeDbTest{ControlError: true},
				cerealInterface: &CerealInterfaceTest{NeedError: false},
			}},
			wantError: true,
			error:     errors.New("Unable to get the status of control index flag: Unable to fetch status from database"),
		},
		{
			name: "Test true status from database",
			args: args{upgrade: Upgrade{
				storage:         &pgdb.UpgradeDbTest{NeedStatus: true},
				cerealInterface: &CerealInterfaceTest{NeedError: false},
			}},
			wantError: false,
		},
		{
			name: "Test error from cereal manager for daily latest flag",
			args: args{upgrade: Upgrade{
				storage:         &pgdb.UpgradeDbTest{NeedStatus: true},
				cerealInterface: &CerealInterfaceTest{NeedError: true},
			}},
			wantError: true,
			error:     errors.New("Unable to enqueue the message in the flow for daily latest flag: Unable to enqueue workflow for day latest flag"),
		},
		{
			name: "Test error from cereal manager for control index flag",
			args: args{upgrade: Upgrade{
				storage:         &pgdb.UpgradeDbTest{NeedStatus: true},
				cerealInterface: &CerealInterfaceTest{NeedErrorForControl: true},
			}},
			wantError: true,
			error:     errors.New("Unable to enqueue the message in the flow for control index flag: Unable to enqueue workflow for control index flag"),
		},
	}

	for _, test := range tests {
		got := test.args.upgrade.PollForUpgradeFlagDayLatest()

		if test.wantError {
			assert.Equal(t, got.Error(), test.error.Error())
		}
	}

}
