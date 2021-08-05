package server

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/chef/automate/api/interservice/id_token"
)

type IdTokenValidator struct {
	pgDB *sql.DB
}

const (
	getIdTokenKey = `SELECT token FROM blacklisted_id_tokens WHERE token = ($1);`
)

func (s *IdTokenValidator) ValidateIdToken(ctx context.Context,
	req *id_token.IdTokenReq) (*id_token.IdTokenResp, error) {

	// This is for tests which runs InMemory
	if s.pgDB == nil {
		return &id_token.IdTokenResp{Exist: false}, nil
	}

	idToken := s.pgDB.QueryRow(getIdTokenKey, req.Token)
	var token string
	err := idToken.Scan(&token)
	if err != nil && err != sql.ErrNoRows {
		// log the error
		fmt.Println(err, "idTokenErr")
		return nil, err
	}

	if token == req.Token {
		return &id_token.IdTokenResp{Exist: true}, nil
	}
	return &id_token.IdTokenResp{Exist: false}, nil
}
