package postgres

const selectService = `
SELECT * FROM service
WHERE origin = $1
  AND name = $2
  AND sup_id IN (
    SELECT id FROM supervisor
    WHERE member_id = $3
  )
`

// getServiceFromUniqueFields retreives a service from the db without the need of an id
func (db *postgres) getServiceFromUniqueFields(origin, name, member string) (*service, bool) {
	var svc service
	err := db.SelectOne(&svc, selectService, origin, name, member)
	if err != nil {
		return nil, false
	}

	return &svc, true
}
