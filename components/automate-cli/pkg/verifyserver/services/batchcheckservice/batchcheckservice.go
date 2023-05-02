package batchcheckservice

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/trigger"
)

type IBatchCheckService interface {
	BatchCheck(checks []string, config models.Config) bool
	AddHardwareResourceCountCheckTrigger(h trigger.IHardwareResourceCountCheckTrigger) *BatchCheckService
}

type BatchCheckService struct {
	hardwareResourceCountCheckTrigger trigger.IHardwareResourceCountCheckTrigger
}

func NewBatchCheckService() IBatchCheckService {
	return &BatchCheckService{}
}

func (ss *BatchCheckService) AddHardwareResourceCountCheckTrigger(h trigger.IHardwareResourceCountCheckTrigger)  *BatchCheckService {
	ss.hardwareResourceCountCheckTrigger = h
	return ss
}

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) bool{
	bastionCheckResultChan := make(chan bool, 3)
	go ss.hardwareResourceCountCheck(config, bastionCheckResultChan)
	go ss.sshUserAccessCheck(config, bastionCheckResultChan)
	go ss.certificateCheck(config, bastionCheckResultChan)
	for i := 0; i < 3; i++ {
		result := <-bastionCheckResultChan
		fmt.Println(result)
	}
	defer close(bastionCheckResultChan)
	return true
}

func (ss *BatchCheckService) hardwareResourceCountCheck(config models.Config, resultChan chan bool) {
	ss.hardwareResourceCountCheckTrigger.Run()
	resultChan <- true
}

func (ss *BatchCheckService) sshUserAccessCheck(config models.Config, resultChan chan bool) {
	
	resultChan <- false
}

func (ss *BatchCheckService) certificateCheck(config models.Config, resultChan chan bool) {
	resultChan <- true
}