package postgres

import (
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/pkg/errors"
)

// TODO: delete if unused
// GetSupervisor returns a supervisor from the database
func (db *Postgres) GetSupervisor(id int32) (*storage.Supervisor, error) {
	sup, err := db.getSupervisor(id)
	if err != nil {
		return nil, err
	}

	return &storage.Supervisor{
		ID:       sup.ID,
		MemberID: sup.MemberID,
		Fqdn:     sup.Fqdn,
		Site:     sup.Site,
	}, nil
}

// TODO: delete if unused
func (db *Postgres) getSupervisor(id int32) (*supervisor, error) {
	var sup supervisor
	err := db.SelectOne(&sup,
		"SELECT id, member_id, fqdn, site FROM supervisor WHERE id = $1", id)
	if err != nil {
		return nil, errors.Wrap(err, "unable to retrieve supervisor from the database")
	}

	return &sup, nil
}

func (db *Postgres) GetSupervisorsCount() (int32, error) {
	count, err := db.DbMap.SelectInt("SELECT COUNT(DISTINCT supervisor_id) FROM service_full;")
	return int32(count), err
}

// TODO: delete if unused
func (db *Postgres) getSupervisorID(member string) (int32, bool) {
	var sid int32
	err := db.SelectOne(&sid,
		"SELECT id FROM supervisor WHERE member_id = $1",
		member,
	)
	if err != nil {
		return sid, false
	}

	return sid, true
}
