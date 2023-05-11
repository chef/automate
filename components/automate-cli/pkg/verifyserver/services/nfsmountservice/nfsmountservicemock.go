package nfsmountservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockNFSMountService struct {
	GetNFSMountDetailsFunc func(reqBody models.NFSMountRequest) *[]models.NFSMountResponse
}

func (mnm *MockNFSMountService) GetNFSMountDetails(reqBody models.NFSMountRequest) *[]models.NFSMountResponse {
	return mnm.GetNFSMountDetailsFunc(reqBody)
}
