package server

import (
	"net/http"

	"github.com/pkg/errors"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"

	"github.com/chef/automate/components/authn-service/authenticator"
)

func (s *Server) authenticate(req *http.Request) (authenticator.Requestor, error) {
	for authnID, authn := range s.authenticators {
		requestor, err := authn.Authenticate(req)
		s.debugLogAuthenticateResult(authnID, requestor, err)
		if err == nil {
			return requestor, nil
		}
	}
	return nil, errors.New("request not authenticated")
}

func (s *Server) debugLogAuthenticateResult(authnID string, r authenticator.Requestor, err error) {
	fields := []zapcore.Field{
		zap.String("authenticator", authnID),
	}
	if err != nil {
		fields = append(fields, zap.String("err", err.Error()))
	}
	if r != nil {
		fields = append(fields, zap.String("subject", r.Subject()), zap.Strings("teams", r.Teams()))
	}
	s.logger.Debug("authenticator response", fields...)
}
