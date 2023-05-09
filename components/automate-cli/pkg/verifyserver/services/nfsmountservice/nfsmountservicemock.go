package nfsmountservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockNFSMountService struct {
	GetNFSMountDetailsFunc func(reqBody models.NFSMountRequest) *[]models.NFSMountResponse
	MakeConcurrentCallFunc func(ip string, node_type string, mountLocation string, ch chan string, nfsMountResultMap map[string][]models.NFSMountResponse, shareMap map[string]models.NFSMountLocResponse, key string, countMap map[models.NFSMountLocResponse]int)
	DoAPICallFunc          func(ip string, node_type string, mountLocation string, shareMap map[string]models.NFSMountLocResponse, key string, countMap map[models.NFSMountLocResponse]int) models.NFSMountResponse
}

func (mnm *MockNFSMountService) GetNFSMountDetails(reqBody models.NFSMountRequest) *[]models.NFSMountResponse {
	return mnm.GetNFSMountDetailsFunc(reqBody)
}

func (mnm *MockNFSMountService) MakeConcurrentCall(ip string, node_type string, mountLocation string, ch chan string, nfsMountResultMap map[string][]models.NFSMountResponse, shareMap map[string]models.NFSMountLocResponse, key string, countMap map[models.NFSMountLocResponse]int) {
	mnm.MakeConcurrentCallFunc(ip, node_type, mountLocation, ch, nfsMountResultMap, shareMap, key, countMap)
}

func (mnm *MockNFSMountService) DoAPICall(ip string, node_type string, mountLocation string, shareMap map[string]models.NFSMountLocResponse, key string, countMap map[models.NFSMountLocResponse]int) models.NFSMountResponse {
	return mnm.DoAPICallFunc(ip, node_type, mountLocation, shareMap, key, countMap)
}
