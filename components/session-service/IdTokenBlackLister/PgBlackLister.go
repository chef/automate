package IdTokenBlackLister

import (
	"database/sql"
)

type PgBlackLister struct {
	pgConn *sql.DB
}

func NewPgBlackLister(pgConn *sql.DB) PgBlackLister {
	return PgBlackLister{pgConn}
}

func (a PgBlackLister) InsertToBlackListedStore(idToken string) error {
	sqlStatement := `
		INSERT INTO blacklisted_id_tokens (token)
		VALUES ($1)`

	_, err := a.pgConn.Exec(sqlStatement, idToken) // Dump id_token in blacklisted_id_tokens
	return err
}

func (a PgBlackLister) IsIdTokenBlacklisted(idToken string) (bool, error) {
	const (
		getIdTokenKey = `SELECT token FROM blacklisted_id_tokens WHERE token = ($1);`
	)

	pgIdTokenRow := a.pgConn.QueryRow(getIdTokenKey, idToken)
	var token string
	err := pgIdTokenRow.Scan(&token)
	if err != nil && err != sql.ErrNoRows {
		return false, err
	}

	return token == idToken, nil
}
