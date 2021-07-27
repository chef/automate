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

const expiryComparisionTime = 2
const (
	getSessionKey = `SELECT token, expiry FROM sessions WHERE token = ($1);`
)

func (s *SessionCookieValidator) ValidateSessionCookie(ctx context.Context,
	req *session.SessionKeyReq) (*session.SessionKeyResp, error) {

	// This is for tests which runs InMemory
	if s.pgDB == nil {
		return &session.SessionKeyResp{Valid: false, Exist: false}, nil
	}

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
	// By default session expiry is 24hrs which gets updated every minute
	// So for the sake of comparision taking 2 minutes difference
	// we can think of different number too
	if diff.Minutes() < 1440-expiryComparisionTime {
		valid = false
	} else {
		valid = true
	}

	if token != req.Key {
		return &session.SessionKeyResp{Valid: valid, Exist: false}, nil
	}
	return &session.SessionKeyResp{Valid: valid, Exist: true}, nil
}
