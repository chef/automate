package server

import (
	"context"
	"database/sql"

	"github.com/chef/automate/components/session-service/IdTokenBlackLister"

	"github.com/chef/automate/api/interservice/id_token"
)

type IdTokenValidator struct {
	pgDB               *sql.DB
	idTokenBlackLister IdTokenBlackLister.IdTokenBlackLister
}

func (s *IdTokenValidator) ValidateIdToken(ctx context.Context,
	req *id_token.ValidateIdTokenRequest) (*id_token.ValidateIdTokenResponse, error) {

	// This is for tests which runs InMemory
	if s.pgDB == nil {
		return &id_token.ValidateIdTokenResponse{IsInvalid: false}, nil
	}

	isBlackListed, err := s.idTokenBlackLister.IsIdTokenBlacklisted(req.Token)
	if err != nil {
		return &id_token.ValidateIdTokenResponse{IsInvalid: false}, err
	}
	return &id_token.ValidateIdTokenResponse{IsInvalid: isBlackListed}, nil
}
