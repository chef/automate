package pgdb

type agent struct {
	ID     string `db:"id"`
	Type   string `db:"type"`
	Status string `db:"status"`
}
