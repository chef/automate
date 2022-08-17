package pgdb

import (
	"context"
	"errors"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/sirupsen/logrus"
)

func (db *DB) GetConfigs(ctx context.Context) (*reporting.ComplianceConfig, error) {
	resp := reporting.ComplianceConfig{}

	row := db.QueryRow("SELECT policy_name, older_than_days FROM compliance_lifecycle;")
	if err := row.Scan(&resp.PolicyName, &resp.OlderThanDays); err != nil {
		logrus.Errorf("error while scanning fields: %+v", err)
		return nil, err
	}
	return &resp, nil
}

func (db *DB) SetConfigs(ctx context.Context, in *reporting.ComplianceConfig) error {

	row, err := db.Exec("UPDATE compliance_lifecycle SET older_than_days=$1 WHERE policy_name=$2;", in.OlderThanDays, in.PolicyName)
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
		return errors.New("didn't update any row")
	}

	return nil
}
