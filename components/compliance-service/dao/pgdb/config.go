package pgdb

import (
	"context"
	"errors"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/sirupsen/logrus"
)

const (
	PolicyNameUnreachable = "unreachable_assets"
)

func (db *DB) GetConfigs(ctx context.Context) (*reporting.ComplianceConfigResponse, error) {
	resp := reporting.ComplianceConfigResponse{}

	row := db.QueryRow("SELECT policy_name, value FROM compliance_lifecycle;")
	if err := row.Scan(&resp.PolicyName, &resp.Value); err != nil {
		logrus.Errorf("error while scanning fields: %+v", err)
		return nil, err
	}
	return &resp, nil
}

func (db *DB) SetConfigs(ctx context.Context, in *reporting.ComplianceConfigRequest) error {

	row, err := db.Exec("UPDATE compliance_lifecycle SET value=$1 WHERE policy_name=$2;", in.Value, PolicyNameUnreachable)
	if err != nil {
		logrus.Errorf("error while executing the conf UPDATE command: %+v", err)
		return err
	}

	affected, err := row.RowsAffected()
	if err != nil {
		logrus.Errorf("cannot get the affected rows: %+v", err)
		return err
	}
	if affected == 0 {
		logrus.Warnf("the policy doesn't exists")
		return errors.New("didn't update any row as the policy doesn't exists")
	}

	return nil
}
