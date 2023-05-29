package db

import (
	"database/sql"

	_ "github.com/lib/pq"
)

type DBImpl struct {
}
type DB interface {
	InitPostgresDB(con string) error
}

func NewDBImpl() DB {
	return &DBImpl{}
}

func (di *DBImpl) InitPostgresDB(con string) error {

	db, err := sql.Open("postgres", con)
	if err != nil {
		return err
	}

	defer db.Close()

	err = db.Ping()
	if err != nil {
		return err
	}
	return nil
}
