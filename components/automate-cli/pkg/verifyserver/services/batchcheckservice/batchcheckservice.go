package batchcheckservice

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
)

type IBatchCheckService interface {
	BatchCheck(checks []string, config models.Config) bool
}

type BatchCheckService struct {
	CheckTrigger trigger.ICheckTrigger
}

func NewBatchCheckService(trigger trigger.ICheckTrigger) IBatchCheckService {
	return &BatchCheckService{
		CheckTrigger: trigger,
	}
}

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) bool {
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
	ss.CheckTrigger.HardwareResourceCountCheck()
	resultChan <- true
}

func (ss *BatchCheckService) sshUserAccessCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SshUserAccessCheck()
	resultChan <- false
}

func (ss *BatchCheckService) certificateCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.CertificateCheck()
	resultChan <- true
}

func (ss *BatchCheckService) systemResourceCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SystemResourceCheck()
	resultChan <- true
}

func (ss *BatchCheckService) externalOpensearchCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.ExternalOpensearchCheck()
	resultChan <- true
}

func (ss *BatchCheckService) externalPostgresCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.ExternalPostgresCheck()
	resultChan <- true
}

func (ss *BatchCheckService) firewallCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.FirewallCheck()
	resultChan <- true
}

func (ss *BatchCheckService) fqdnCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.FqdnCheck()
	resultChan <- true
}

func (ss *BatchCheckService) nfsBackupConfigCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.NfsBackupConfigCheck()
	resultChan <- true
}

func (ss *BatchCheckService) s3BackupConfigCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.S3BackupConfigCheck()
	resultChan <- true
}

func (ss *BatchCheckService) softwareVersionCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SoftwareVersionCheck()
	resultChan <- true
}

func (ss *BatchCheckService) systemUserCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SystemUserCheck()
	resultChan <- true
}
