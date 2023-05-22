package startmockserverservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockServersService struct {
	StartMockServerFunc func(cfg *models.StartMockServerRequestBody) error
	GetMockServersFunc  func() []*models.Server
}

func (mss *MockServersService) StartMockServer(cfg *models.StartMockServerRequestBody) error {
	return mss.StartMockServerFunc(cfg)
}

func (mss *MockServersService) GetMockServers() []*models.Server {
	return mss.GetMockServersFunc()
}
