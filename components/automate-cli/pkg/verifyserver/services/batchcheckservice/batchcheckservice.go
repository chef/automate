package batchcheckservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/stringutils"
)

type IBatchCheckService interface {
	BatchCheck(checks []string, config models.Config) models.BatchCheckResponse
}

type BatchCheckService struct {
	CheckTrigger trigger.CheckTrigger
}

func NewBatchCheckService(trigger trigger.CheckTrigger) IBatchCheckService {
	return &BatchCheckService{
		CheckTrigger: trigger,
	}
}

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) models.BatchCheckResponse {
	batchApisResultMap := make(map[string][]models.ApiResult)
	if shouldRunChecksOnBastion(checks) {
		bastionCheckResultChan := make(chan map[string]models.CheckTriggerResponse, len(checks))
		for _, check := range checks {
			go ss.RunBastionCheck(check, config, bastionCheckResultChan)
		}
		for i := 0; i < len(checks); i++ {
			result := <-bastionCheckResultChan
			for k, v := range result {
				batchApisResultMap[k] = append(batchApisResultMap[k], v.Result)
			}
		}
		defer close(bastionCheckResultChan)
		return constructBatchCheckResponse(batchApisResultMap, config.Hardware)
	}
	for _, check := range checks {
		resp := ss.RunRemoteCheck(check, config)
		for k, v := range resp {
			batchApisResultMap[k] = append(batchApisResultMap[k], v.Result)
		}
	}
	return constructBatchCheckResponse(batchApisResultMap, config.Hardware)
}

func (ss *BatchCheckService) RunBastionCheck(check string, config models.Config, resultChan chan map[string]models.CheckTriggerResponse) {
	resp := ss.getCheckInstance(check).Run(config)
	resultChan <- resp
}

func (ss *BatchCheckService) RunRemoteCheck(check string, config models.Config) map[string]models.CheckTriggerResponse {
	return ss.getCheckInstance(check).Run(config)
}

func (ss *BatchCheckService) getCheckInstance(check string) trigger.ICheck {
	switch check {
	case constants.HARDWARE_RESOURCE_COUNT:
		return ss.CheckTrigger.HardwareResourceCountCheck
	case constants.CERTIFICATE:
		return ss.CheckTrigger.CertificateCheck
	case constants.SSH_USER:
		return ss.CheckTrigger.SshUserAccessCheck
	case constants.SYSTEM_RESOURCES:
		return ss.CheckTrigger.SystemResourceCheck
	case constants.SOFTWARE_VERSIONS:
		return ss.CheckTrigger.SoftwareVersionCheck
	case constants.SYSTEM_USER:
		return ss.CheckTrigger.SystemUserCheck
	case constants.S3_BACKUP_CONFIG:
		return ss.CheckTrigger.S3BackupConfigCheck
	case constants.FQDN:
		return ss.CheckTrigger.FqdnCheck
	case constants.FIREWALL:
		return ss.CheckTrigger.FirewallCheck
	case constants.EXTERNAL_OPENSEARCH:
		return ss.CheckTrigger.ExternalOpensearchCheck
	case constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS:
		return ss.CheckTrigger.OpensearchS3BucketAccessCheck
	case constants.EXTERNAL_POSTGRESQL:
		return ss.CheckTrigger.ExternalPostgresCheck
	case constants.NFS_BACKUP_CONFIG:
		return ss.CheckTrigger.NfsBackupConfigCheck
	}
	return nil
}

func shouldRunChecksOnBastion(checks []string) bool {
	if stringutils.SliceContains(checks, constants.HARDWARE_RESOURCE_COUNT) ||
		stringutils.SliceContains(checks, constants.CERTIFICATE) ||
		stringutils.SliceContains(checks, constants.SSH_USER) {
		return true
	}
	return false
}

func constructBatchCheckResponse(batchApisResultMap map[string][]models.ApiResult, hardwareDetails models.Hardware) models.BatchCheckResponse {
	var result = make([]models.BatchCheckResult, len(batchApisResultMap))
	var resultIndex = 0
	for k, v := range batchApisResultMap {
		result[resultIndex].Ip = k
		result[resultIndex].NodeType = getNodeTypeFromIp(k, hardwareDetails)
		result[resultIndex].Tests = v
		resultIndex = resultIndex + 1
	}
	return models.BatchCheckResponse{
		Status: "SUCCESS",
		Result: result,
	}
}

func getNodeTypeFromIp(ip string, hardwareDetails models.Hardware) string {
	if stringutils.SliceContains(hardwareDetails.AutomateNodeIps, ip) {
		return "automate"
	}
	if stringutils.SliceContains(hardwareDetails.ChefInfraServerNodeIps, ip) {
		return "chef_server"
	}
	if stringutils.SliceContains(hardwareDetails.PostgresqlNodeIps, ip) {
		return "postgresql"
	}
	if stringutils.SliceContains(hardwareDetails.OpensearchNodeIps, ip) {
		return "opensearch"
	}
	return ""
}
