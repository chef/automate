package fqdnservice

import (
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockFqdnService struct {
	CheckFqdnReachabilityFunc func(models.FqdnRequest, string, time.Duration) models.FqdnResponse
}

func (mfq *MockFqdnService) CheckFqdnReachability(req models.FqdnRequest, port string, duration time.Duration) models.FqdnResponse {
	return mfq.CheckFqdnReachabilityFunc(req, port, duration)
}
