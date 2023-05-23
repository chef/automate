package fqdnservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockFqdnService struct {
	CheckFqdnReachabilityFunc func(models.FqdnRequest) models.FqdnResponse
}

func (mfq *MockFqdnService) CheckFqdnReachability(req models.FqdnRequest) models.FqdnResponse {
	return mfq.CheckFqdnReachabilityFunc(req)
}
