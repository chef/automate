package postgres

import (
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/pkg/errors"
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

func (db *Postgres) getDeploymentID(app, env string) (int32, bool) {
	var id int32
	err := db.SelectOne(&id,
		"SELECT id FROM deployment WHERE app_name = $1 AND environment = $2",
		app, env)
	if err != nil {
		return id, false
	}

	return id, true
}
