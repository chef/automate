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

func (s *SessionCookieValidator) ValidateSessionCookie(ctx context.Context,
	req *session.SessionKeyReq) (*session.SessionKeyResp, error) {

	// This is for tests which runs InMemory
	if s.pgDB == nil {
		return &session.SessionKeyResp{Valid: true, Exist: true}, nil
	}

	fmt.Println("REQ::", req)

	return &session.SessionKeyResp{Valid: true, Exist: true}, nil
}
