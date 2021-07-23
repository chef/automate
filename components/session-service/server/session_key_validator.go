package server

import (
	"context"

	"github.com/chef/automate/api/interservice/session"
	"golang.org/x/net/context"
)

type Server struct {
}

func (s *Server) ValidateSessionCookie(ctx *context.Context, sessionKey *session.SessionKeyReq) (*session.SessionKeyResp, error) {

}
