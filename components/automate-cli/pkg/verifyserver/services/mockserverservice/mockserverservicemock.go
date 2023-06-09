package mockserverservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockServersServiceMock struct {
	StartFunc func(cfg *models.StartMockServerRequestBody) error
	StopFunc  func(cfg *models.StopMockServerRequestBody) error
}

func (mss *MockServersServiceMock) Start(cfg *models.StartMockServerRequestBody) error {
	return mss.StartFunc(cfg)
}

func (mss *MockServersServiceMock) Stop(cfg *models.StopMockServerRequestBody) error {
	return mss.StopFunc(cfg)
}
