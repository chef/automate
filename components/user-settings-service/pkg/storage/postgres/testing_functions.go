package postgres

// EmptyStorage deletes all the data from the database
// @afiune This function is only used by our Integration Test framework
func (db *Postgres) EmptyStorage() error {
	_, err := db.Exec("DELETE FROM service_full")
	return err
}
