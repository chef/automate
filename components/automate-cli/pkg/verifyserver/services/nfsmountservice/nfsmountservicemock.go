package nfsmountservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockNFSMountService struct {
	GetNFSMountDetailsFunc func(reqBody models.NFSMountRequest) *[]models.NFSMountResponse
	GetNFSMountLocFunc     func(reqBody models.NFSMountLocRequest) *models.NFSMountLocResponse
}

func (mnm *MockNFSMountService) GetNFSMountDetails(reqBody models.NFSMountRequest) *[]models.NFSMountResponse {
	return mnm.GetNFSMountDetailsFunc(reqBody)
}

func (mnm *MockNFSMountService) GetNFSMountLoc(reqBody models.NFSMountLocRequest) *models.NFSMountLocResponse {
	return mnm.GetNFSMountLocFunc(reqBody)
}
