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
	CheckTrigger trigger.CheckTrigger
}

func NewBatchCheckService(trigger trigger.CheckTrigger) IBatchCheckService {
	return &BatchCheckService{
		CheckTrigger: trigger,
	}
}

// func NewBatchCheckService() IBatchCheckService {
// 	return &BatchCheckService{}
// }

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) bool {
	if shouldRunChecksOnBastion(checks) {
		bastionCheckResultChan := make(chan map[string]models.CheckTriggerResponse, len(checks))
		for _, check := range checks {
			go ss.RunCheck(check, config, bastionCheckResultChan)
		}
		resultMap := make(map[string][]models.CheckTriggerResponse)
		for i := 0; i < len(checks); i++ {
			result := <-bastionCheckResultChan
			for k, v := range result {
				resultMap[k] = append(resultMap[k], v)
			}
		}
		fmt.Println(resultMap)
		defer close(bastionCheckResultChan)
	}
	return true
}

func (ss *BatchCheckService) RunCheck(check string, config models.Config, resultChan chan map[string]models.CheckTriggerResponse) {
	resp := ss.getCheckInstance(check).Run(config)
	resultChan <- resp
}

func (ss *BatchCheckService) getCheckInstance(check string) trigger.ICheck {
	switch check {
	case constants.HARDWARE_RESOURCE_COUNT:
		return ss.CheckTrigger.HardwareResourceCountCheck
	case constants.CERTIFICATE:
		return ss.NewCertificateCheck()
	case constants.SSH_USER:
		return ss.CheckTrigger.SshUserAccessCheck
	case constants.SYSTEM_RESOURCES:
		return ss.NewSystemResourceCheck()
	case constants.SOFTWARE_VERSIONS:
		return ss.NewSoftwareVersionCheck()
	case constants.SYSTEM_USER:
		return ss.NewSystemUserCheck()
	case constants.S3_BACKUP_CONFIG:
		return ss.NewS3BackupConfigCheck()
	case constants.FQDN:
		return ss.NewFqdnCheck()
	case constants.FIREWALL:
		return ss.NewFirewallCheck()
	case constants.EXTERNAL_OPENSEARCH:
		return ss.NewExternalOpensearchCheck()
	case constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS:
		return ss.NewOpensearchS3BucketAccessCheck()
	case constants.EXTERNAL_POSTGRESQL:
		return ss.NewExternalPostgresCheck()
	case constants.NFS_BACKUP_CONFIG:
		return ss.NewNfsBackupConfigCheck()
	}
	return nil
}

// func (ss *BatchCheckService) constructChecksMap() map[string]interface{} {
// 	checksMap := map[string]interface{}{
// 		constants.HARDWARE_RESOURCE_COUNT:         ss.hardwareResourceCountCheck,
// 		constants.CERTIFICATE:                     ss.certificateCheck,
// 		constants.SSH_USER:                        ss.sshUserAccessCheck,
// 		constants.SYSTEM_RESOURCES:                ss.systemResourceCheck,
// 		constants.SOFTWARE_VERSIONS:               ss.softwareVersionCheck,
// 		constants.SYSTEM_USER:                     ss.systemUserCheck,
// 		constants.S3_BACKUP_CONFIG:                ss.s3BackupConfigCheck,
// 		constants.FQDN:                            ss.fqdnCheck,
// 		constants.FIREWALL:                        ss.firewallCheck,
// 		constants.EXTERNAL_OPENSEARCH:             ss.externalOpensearchCheck,
// 		constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS: ss.opensearchS3BucketAccessCheck,
// 		constants.EXTERNAL_POSTGRESQL:             ss.externalPostgresCheck,
// 		constants.NFS_BACKUP_CONFIG:               ss.nfsBackupConfigCheck,
// 	}
// 	return checksMap
// }

func shouldRunChecksOnBastion(checks []string) bool {
	if stringutils.SliceContains(checks, constants.HARDWARE_RESOURCE_COUNT) ||
		stringutils.SliceContains(checks, constants.CERTIFICATE) ||
		stringutils.SliceContains(checks, constants.SSH_USER) {
		return true
	}
	return false
}

// func (ss *BatchCheckService) hardwareResourceCountCheck(config models.Config, resultChan chan bool) {
// 	res := ss.CheckTrigger.HardwareResourceCountCheck(config)
// 	fmt.Println(res)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) sshUserAccessCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.SshUserAccessCheck(config)
// 	resultChan <- false
// }

// func (ss *BatchCheckService) certificateCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.CertificateCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) systemResourceCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.SystemResourceCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) externalOpensearchCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.ExternalOpensearchCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) externalPostgresCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.ExternalPostgresCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) firewallCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.FirewallCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) fqdnCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.FqdnCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) nfsBackupConfigCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.NfsBackupConfigCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) s3BackupConfigCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.S3BackupConfigCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) softwareVersionCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.SoftwareVersionCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) systemUserCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.SystemUserCheck(config)
// 	resultChan <- true
// }

// func (ss *BatchCheckService) opensearchS3BucketAccessCheck(config models.Config, resultChan chan bool) {
// 	ss.CheckTrigger.OpensearchS3BucketAccessCheck(config)
// 	resultChan <- true
// }

func (ss *BatchCheckService) NewHardwareResourceCountCheck() *trigger.HardwareResourceCountCheck {
	return &trigger.HardwareResourceCountCheck{}
}

func (ss *BatchCheckService) NewSshUserAccessCheck() *trigger.SshUserAccessCheck {
	return &trigger.SshUserAccessCheck{}
}

func (ss *BatchCheckService) NewCertificateCheck() *trigger.CertificateCheck {
	return &trigger.CertificateCheck{}
}

func (ss *BatchCheckService) NewSystemResourceCheck() *trigger.SystemResourceCheck {
	return &trigger.SystemResourceCheck{}
}

func (ss *BatchCheckService) NewExternalOpensearchCheck() *trigger.ExternalOpensearchCheck {
	return &trigger.ExternalOpensearchCheck{}
}

func (ss *BatchCheckService) NewExternalPostgresCheck() *trigger.ExternalPostgresCheck {
	return &trigger.ExternalPostgresCheck{}
}

func (ss *BatchCheckService) NewFirewallCheck() *trigger.FirewallCheck {
	return &trigger.FirewallCheck{}
}

func (ss *BatchCheckService) NewFqdnCheck() *trigger.FqdnCheck {
	return &trigger.FqdnCheck{}
}

func (ss *BatchCheckService) NewNfsBackupConfigCheck() *trigger.NfsBackupConfigCheck {
	return &trigger.NfsBackupConfigCheck{}
}

func (ss *BatchCheckService) NewS3BackupConfigCheck() *trigger.S3BackupConfigCheck {
	return &trigger.S3BackupConfigCheck{}
}

func (ss *BatchCheckService) NewSoftwareVersionCheck() *trigger.SoftwareVersionCheck {
	return &trigger.SoftwareVersionCheck{}
}

func (ss *BatchCheckService) NewSystemUserCheck() *trigger.SystemUserCheck {
	return &trigger.SystemUserCheck{}
}

func (ss *BatchCheckService) NewOpensearchS3BucketAccessCheck() *trigger.OpensearchS3BucketAccessCheck {
	return &trigger.OpensearchS3BucketAccessCheck{}
}
