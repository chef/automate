package dao

type Destination struct {
	ID     int64  `db:"id"`
	Name   string `db:"name"`
	URL    string `db:"url"`
	Secret string `db:"secret"`
}
