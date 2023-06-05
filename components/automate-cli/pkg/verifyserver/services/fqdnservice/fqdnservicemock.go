package fqdnservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockFqdnService struct {
	CheckFqdnReachabilityFunc func(models.FqdnRequest, string) models.FqdnResponse
}

func (mfq *MockFqdnService) CheckFqdnReachability(req models.FqdnRequest, port string) models.FqdnResponse {
	return mfq.CheckFqdnReachabilityFunc(req, port)
}
