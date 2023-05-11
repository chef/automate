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

func NewBatchCheckService(trigger trigger.CheckTrigger) *BatchCheckService {
	return &BatchCheckService{
		CheckTrigger: trigger,
	}
}

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) models.BatchCheckResponse {
	//batchApisResultMap := make(map[string][]models.ApiResult)
	var bastionChecks = stringutils.SliceIntersection(checks, constants.GetBastionChecks())
	var remoteChecks = stringutils.SliceIntersection(checks, constants.GetRemoteChecks())
	checkTriggerRespMap := make(map[string][]models.CheckTriggerResponse)
	if len(bastionChecks) > 0 {
		bastionCheckResultChan := make(chan []models.CheckTriggerResponse, len(bastionChecks))
		for _, check := range bastionChecks {
			go ss.RunBastionCheck(check, config, bastionCheckResultChan)
		}
		for i := 0; i < len(bastionChecks); i++ {
			result := <-bastionCheckResultChan
			if len(result) > 0 {
				checkTriggerRespMap[result[0].CheckType] = result
			}

		}
		defer close(bastionCheckResultChan)
	}
	if len(remoteChecks) > 0 {
		for _, check := range remoteChecks {
			resp := ss.RunRemoteCheck(check, config)
			checkTriggerRespMap[check] = resp
		}
	}
	return constructBatchCheckResponse(checkTriggerRespMap, config.Hardware, append(bastionChecks, remoteChecks...))
}

func getIndexOfCheck(checks []string, check string) (int, error) {
	return stringutils.IndexOf(checks, check)
}

func (ss *BatchCheckService) RunBastionCheck(check string, config models.Config, resultChan chan []models.CheckTriggerResponse) {
	resp := ss.getCheckInstance(check).Run(config)
	for i, _ := range resp {
		resp[i].CheckType = check
	}
	resultChan <- resp

}

func (ss *BatchCheckService) RunRemoteCheck(check string, config models.Config) []models.CheckTriggerResponse {
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
	default:
		return nil
	}
}

func constructBatchCheckResponse(checkTriggerRespMap map[string][]models.CheckTriggerResponse,
	hardwareDetails models.Hardware, checks []string) models.BatchCheckResponse {
	ipMap := make(map[string][]models.CheckTriggerResponse)
	for k, v := range checkTriggerRespMap {
		for _, checkResp := range v {
			checkIndex, _ := getIndexOfCheck(checks, k)
			ip := checkResp.Host
			nodeType := checkResp.NodeType
			ipMapKey := ip + "_" + nodeType
			if checkIndex >= len(ipMap[ipMapKey]) {
				ipMap[ipMapKey] = append(ipMap[ipMapKey], checkResp)
			} else {
				ipMap[ipMapKey] = append(ipMap[ipMapKey], models.CheckTriggerResponse{})
				copy(ipMap[ipMapKey][checkIndex+1:], ipMap[ipMapKey][checkIndex:])
				ipMap[ipMapKey][checkIndex] = checkResp
			}
		}
	}

	var result = make([]models.BatchCheckResult, len(ipMap))
	var resultIndex = 0
	for _, v := range ipMap {
		result[resultIndex].Ip = v[0].Host
		result[resultIndex].NodeType = v[0].NodeType
		resultArray := []models.ApiResult{}
		for _, checkResult := range v {
			resultArray = append(resultArray, checkResult.Result)
		}
		result[resultIndex].Tests = resultArray
		resultIndex = resultIndex + 1
	}
	return models.BatchCheckResponse{
		Status: "SUCCESS",
		Result: result,
	}
}
