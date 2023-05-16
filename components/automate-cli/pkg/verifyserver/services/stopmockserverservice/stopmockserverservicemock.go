package stopmockserverservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockStopMockServerService struct {
	StopMockServerFunc func(server *models.Server) error
}

func (msm *MockStopMockServerService) StopMockServer(server *models.Server) error {
	return msm.StopMockServerFunc(server)
}
