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
			for ind, _ := range resp {
				resp[ind].CheckType = check
			}
			checkTriggerRespMap[check] = resp
		}
	}
	return constructBatchCheckResponse(checkTriggerRespMap, append(bastionChecks, remoteChecks...))
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

func constructBatchCheckResponse(checkTriggerRespMap map[string][]models.CheckTriggerResponse, checks []string) models.BatchCheckResponse {
	ipMap := make(map[string][]models.CheckTriggerResponse)

	//Construct map with unique ip+nodeType keys to segregate the response
	for checkName, checkResponses := range checkTriggerRespMap {
		for _, checkResponse := range checkResponses {
			checkIndex, _ := getIndexOfCheck(checks, checkName)
			ip := checkResponse.Host
			nodeType := checkResponse.NodeType
			ipMapKey := ip + "_" + nodeType
			_, ok := ipMap[ipMapKey]
			if ok {
				ipMap[ipMapKey][checkIndex] = checkResponse
			} else {
				ipMap[ipMapKey] = make([]models.CheckTriggerResponse, len(checks))
				ipMap[ipMapKey][checkIndex] = checkResponse
			}
		}
	}

	// Arranging the per map values in order in which we got the checks input.
	// Example if certificate check is passed first as input then in final response certificate will come up then other checks
	for k, v := range ipMap {
		arr := []models.CheckTriggerResponse{}
		for _, checkResp := range v {
			if checkResp.Host != "" {
				arr = append(arr, checkResp)
			}
		}
		ipMap[k] = arr
	}

	// Constructing response which is needed by the handler
	var result = make([]models.BatchCheckResult, len(ipMap))
	var resultIndex = 0
	for _, v := range ipMap {
		if len(v) == 0 {
			continue
		}
		result[resultIndex].Ip = v[0].Host
		result[resultIndex].NodeType = v[0].NodeType
		resultArray := []models.ApiResult{}
		for _, checkResult := range v {
			checkResult.Result.Message = checkMsg(checkResult.CheckType)
			checkResult.Result.Check = checkResult.CheckType
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

func checkMsg(check string) string {
	switch check {
	case constants.HARDWARE_RESOURCE_COUNT:
		return constants.HARDWARE_RESOURCE_COUNT_MSG
	case constants.CERTIFICATE:
		return constants.CERTIFICATE_MSG
	case constants.SSH_USER:
		return constants.SSH_USER_MSG
	case constants.SYSTEM_RESOURCES:
		return constants.SYSTEM_RESOURCES_MSG
	case constants.SOFTWARE_VERSIONS:
		return constants.SOFTWARE_VERSIONS_MSG
	case constants.SYSTEM_USER:
		return constants.SYSTEM_USER_MSG
	case constants.S3_BACKUP_CONFIG:
		return constants.S3_BACKUP_CONFIG_MSG
	case constants.FQDN:
		return constants.FQDN_MSG
	case constants.FIREWALL:
		return constants.FIREWALL_MSG
	case constants.EXTERNAL_OPENSEARCH:
		return constants.EXTERNAL_OPENSEARCH_MSG
	case constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS:
		return constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG
	case constants.EXTERNAL_POSTGRESQL:
		return constants.EXTERNAL_POSTGRESQL_MSG
	case constants.NFS_BACKUP_CONFIG:
		return constants.NFS_BACKUP_CONFIG_MSG
	default:
		return ""
	}
}
