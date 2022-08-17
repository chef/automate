package pgdb

import (
	"context"
	"errors"

	"github.com/sirupsen/logrus"
)

// TODO: Update based on the API call
func (db *DB) GetConfigs(ctx context.Context) error {
	pName := ""
	days := 0

	row := db.QueryRow("SELECT policy_name, older_than_days FROM compliance_lifecycle;")
	if err := row.Scan(&pName, &days); err != nil {
		return err
	}
	logrus.Infof("PNAME: %v", pName)
	logrus.Infof("DAYS: %v", days)
	return nil
}

// TODO: Update based on the API call
func (db *DB) SetConfigs(ctx context.Context) error {
	pName := ""
	days := 0

	row, err := db.Exec("UPDATE compliance_lifecycle SET older_than_days=$1 WHERE policy_name=$2;", days, pName)
	if err != nil {
		return err
	}

	affected, err := row.RowsAffected()
	if err != nil {
		return err
	}
	if affected == 0 {
		return errors.New("didn't update any row")
	}

	return nil
}
