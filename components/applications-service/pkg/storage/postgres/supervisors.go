package postgres

func (db *Postgres) GetSupervisorsCount() (int32, error) {
	count, err := db.DbMap.SelectInt("SELECT COUNT(DISTINCT supervisor_id) FROM service_full;")
	return int32(count), err
}
