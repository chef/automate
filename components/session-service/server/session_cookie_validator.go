package server

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/chef/automate/api/interservice/session"
)

type SessionCookieValidator struct {
	pgDB *sql.DB
}

const (
	getSessionKey = `SELECT * FROM sessions WHERE session_id= (Key)`
)

func (s *SessionCookieValidator) ValidateSessionCookie(ctx context.Context,
	req *session.SessionKeyReq) (*session.SessionKeyResp, error) {

	// This is for tests which runs InMemory
	if s.pgDB == nil {
		return &session.SessionKeyResp{Valid: true, Exist: true}, nil
	}

	fmt.Println("REQ::", req)
	sessionKey := s.pgDB.QueryRow(getSessionKey, req.Key)
	fmt.Println(sessionKey, "sessionKey")

	return &session.SessionKeyResp{Valid: true, Exist: true}, nil
}
