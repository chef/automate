package postgres

import (
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/pkg/errors"
)

const (
	selectDeploymentsTotalCount = `
SELECT COUNT(*) FROM (SELECT DISTINCT application, environment FROM service_full) AS d;
`
)

// GetDeployment returns a deployment from the database
func (db *Postgres) GetDeployment(id int32) (*storage.Deployment, error) {
	d, err := db.getDeployment(id)
	if err != nil {
		return nil, err
	}

	return &storage.Deployment{
		ID:          d.ID,
		Application: d.AppName,
		Environment: d.Environment,
	}, nil
}

func (db *Postgres) getDeployment(id int32) (*deployment, error) {
	var d deployment
	err := db.SelectOne(&d, "SELECT id, app_name, environment FROM deployment WHERE id = $1", id)
	if err != nil {
		return nil, errors.Wrap(err, "unable to retrieve deployment from the database")
	}

	return &d, nil
}

func (db *Postgres) GetDeploymentsCount() (int32, error) {
	count, err := db.DbMap.SelectInt(selectDeploymentsTotalCount)
	return int32(count), err
}
