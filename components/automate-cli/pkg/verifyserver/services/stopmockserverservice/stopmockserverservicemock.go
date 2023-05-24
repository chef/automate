package stopmockserverservice

import (
	"context"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockStopMockServerService struct {
	StopMockServerFunc func(server *models.Server) error
}

func (msm *MockStopMockServerService) StopMockServer(server *models.Server) error {
	return msm.StopMockServerFunc(server)
}

type MockHTTPSServer struct {
	ShutdownFunc func(ctx context.Context) error
}

func (s *MockHTTPSServer) Shutdown(ctx context.Context) error {
	return s.ShutdownFunc(ctx)
}
