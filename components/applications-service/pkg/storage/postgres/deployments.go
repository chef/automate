package postgres

const (
	selectDeploymentsTotalCount = `
SELECT COUNT(*) FROM (SELECT DISTINCT application, environment FROM service_full) AS d;
`
)

func (db *Postgres) GetDeploymentsCount() (int32, error) {
	count, err := db.DbMap.SelectInt(selectDeploymentsTotalCount)
	return int32(count), err
}
