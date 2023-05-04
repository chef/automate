package batchcheckservice

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/stringutils"
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
	checksMap := ss.constructChecksMap()
	if shouldRunChecksOnBastion(checks) {
		bastionCheckResultChan := make(chan bool, len(checks))
		for _, check := range checks {
			go checksMap[check].(func(models.Config, chan bool))(config, bastionCheckResultChan)
		}
		for i := 0; i < 3; i++ {
			result := <-bastionCheckResultChan
			fmt.Println(result)
		}
		defer close(bastionCheckResultChan)
	}
	return true
}

func (ss *BatchCheckService) constructChecksMap() map[string]interface{} {
	checksMap := map[string]interface{}{
		constants.HARDWARE_RESOURCE_COUNT:         ss.hardwareResourceCountCheck,
		constants.CERTIFICATE:                     ss.certificateCheck,
		constants.SSH_USER:                        ss.sshUserAccessCheck,
		constants.SYSTEM_RESOURCES:                ss.systemResourceCheck,
		constants.SOFTWARE_VERSIONS:               ss.softwareVersionCheck,
		constants.SYSTEM_USER:                     ss.systemUserCheck,
		constants.S3_BACKUP_CONFIG:                ss.s3BackupConfigCheck,
		constants.FQDN:                            ss.fqdnCheck,
		constants.FIREWALL:                        ss.firewallCheck,
		constants.EXTERNAL_OPENSEARCH:             ss.externalOpensearchCheck,
		constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS: ss.opensearchS3BucketAccessCheck,
		constants.EXTERNAL_POSTGRESQL:             ss.externalPostgresCheck,
		constants.NFS_BACKUP_CONFIG:               ss.nfsBackupConfigCheck,
	}
	return checksMap
}

func shouldRunChecksOnBastion(checks []string) bool {
	if stringutils.SliceContains(checks, constants.HARDWARE_RESOURCE_COUNT) ||
		stringutils.SliceContains(checks, constants.CERTIFICATE) ||
		stringutils.SliceContains(checks, constants.SSH_USER) {
		return true
	}
	return false
}

func (ss *BatchCheckService) hardwareResourceCountCheck(config models.Config, resultChan chan bool) {
	res := ss.CheckTrigger.HardwareResourceCountCheck(config)
	fmt.Println(res)
	resultChan <- true
}

func (ss *BatchCheckService) sshUserAccessCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SshUserAccessCheck(config)
	resultChan <- false
}

func (ss *BatchCheckService) certificateCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.CertificateCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) systemResourceCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SystemResourceCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) externalOpensearchCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.ExternalOpensearchCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) externalPostgresCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.ExternalPostgresCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) firewallCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.FirewallCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) fqdnCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.FqdnCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) nfsBackupConfigCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.NfsBackupConfigCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) s3BackupConfigCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.S3BackupConfigCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) softwareVersionCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SoftwareVersionCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) systemUserCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.SystemUserCheck(config)
	resultChan <- true
}

func (ss *BatchCheckService) opensearchS3BucketAccessCheck(config models.Config, resultChan chan bool) {
	ss.CheckTrigger.OpensearchS3BucketAccessCheck(config)
	resultChan <- true
}
