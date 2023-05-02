package batchcheckservice

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type IBatchCheckService interface {
	BatchCheck(checks []string, config models.Config)
}

type BatchCheckService struct {
}

func NewBatchCheckService() IBatchCheckService {
	return &BatchCheckService{}
}

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) {
	bastionCheckResultChan := make(chan bool, 3)
	go ss.hardwareResourceCountCheck(config, bastionCheckResultChan)
	go ss.sshUserAccessCheck(config, bastionCheckResultChan)
	go ss.certificateCheck(config, bastionCheckResultChan)
	for i := 0; i < 3; i++ {
		result := <-bastionCheckResultChan
		fmt.Println(result)
	}
	defer close(bastionCheckResultChan)
}

func (ss *BatchCheckService) hardwareResourceCountCheck(config models.Config, resultChan chan bool) {
	resultChan <- true
}

func (ss *BatchCheckService) sshUserAccessCheck(config models.Config, resultChan chan bool) {
	resultChan <- false
}

func (ss *BatchCheckService) certificateCheck(config models.Config, resultChan chan bool) {
	resultChan <- true
}