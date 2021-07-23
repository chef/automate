package server

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/chef/automate/api/interservice/session"
)

type SessionCookieValidator struct {
	pgDB *sql.DB
}

const (
	getSessionKey = `SELECT * FROM sessions WHERE token = (Key);`
)

func (s *SessionCookieValidator) ValidateSessionCookie(ctx context.Context,
	req *session.SessionKeyReq) (*session.SessionKeyResp, error) {

	// This is for tests which runs InMemory
	if s.pgDB == nil {
		return &session.SessionKeyResp{Valid: false, Exist: false}, nil
	}

	fmt.Println("REQ::", req)
	sessionKey := s.pgDB.QueryRow(getSessionKey, req.Key)
	var token string
	var expiry time.Time
	err := sessionKey.Scan(&token, &expiry)

	if err != nil && err != sql.ErrNoRows {
		// log the error
		fmt.Println(err, "sessionKeyErr")
		return nil, err
	}

	diff := expiry.Sub(time.Now())
	var valid bool
	if diff.Minutes() < 17 {
		valid = false
	} else {
		valid = true
	}

	if token != req.Key {
		return &session.SessionKeyResp{Valid: valid, Exist: false}, nil
	}
	fmt.Println(sessionKey, "sessionKey")
	return &session.SessionKeyResp{Valid: valid, Exist: true}, nil

}
