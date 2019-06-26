package postgres

const (
	selectSupervisorsTotalCount = `
SELECT count(*)
  FROM supervisor;
`
)

func (db *Postgres) GetSupervisorsCount() (int32, error) {
	count, err := db.DbMap.SelectInt(selectSupervisorsTotalCount)
	return int32(count), err
}
