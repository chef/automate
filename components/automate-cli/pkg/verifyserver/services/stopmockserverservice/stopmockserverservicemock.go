package stopmockserverservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockStopMockServerService struct {
	StopMockServerFunc  func(server *models.Server) error
	StopTCPServerFunc   func(server *models.Server) error
	StopUDPServerFunc   func(server *models.Server) error
	StopHTTPSServerFunc func(server *models.Server) error
}

func (msm *MockStopMockServerService) StopMockServer(server *models.Server) error {
	return msm.StopMockServerFunc(server)
}

func (msm *MockStopMockServerService) StopTCPServer(server *models.Server) error {
	return msm.StopTCPServerFunc(server)
}

func (msm *MockStopMockServerService) StopUDPServer(server *models.Server) error {
	return msm.StopUDPServerFunc(server)
}

func (msm *MockStopMockServerService) StopHTTPSServer(server *models.Server) error {
	return msm.StopHTTPSServerFunc(server)
}
