package nfsmountbackupchecktrigger

import (
	"encoding/json"
	"io"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"
)

type NfsBackupConfigCheck struct {
	log  logger.Logger
	port string
	host string
}

func NewNfsBackupConfigCheck(log logger.Logger, port string) *NfsBackupConfigCheck {
	return &NfsBackupConfigCheck{
		log:  log,
		port: port,
		host: constants.LOCALHOST,
	}
}

func (nbc *NfsBackupConfigCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	if config.Hardware == nil {
		return trigger.NilResp(constants.NFS_BACKUP_CONFIG, true, true, false)
	}
	if config.Backup.FileSystem == nil {
		return nilNFSMountBackupResp(config, constants.NFS_BACKUP_CONFIG)
	}
	if isBackupEmpty(config.Backup) {
		return emptyNFSMountBackupResp(config, constants.NFS_BACKUP_CONFIG)
	}

	nfsMountReq := models.NFSMountRequest{
		AutomateNodeIPs:        config.Hardware.AutomateNodeIps,
		ChefInfraServerNodeIPs: config.Hardware.ChefInfraServerNodeIps,
		PostgresqlNodeIPs:      config.Hardware.PostgresqlNodeIps,
		OpensearchNodeIPs:      config.Hardware.OpenSearchNodeIps,
		MountLocation:          config.Backup.FileSystem.MountLocation,
	}

	//Triggers only one API call for nfs mount API
	nfsMountAPIResponse, err := nbc.triggerCheckForMountService(nfsMountReq)
	if err != nil {
		response := constructErrorResult(config, err)
		return response

	}
	result := constructSuccessResult(*nfsMountAPIResponse)

	return result

}

// triggerCheckForMountService - Call the Hardware resource API and format response
func (ss *NfsBackupConfigCheck) triggerCheckForMountService(body models.NFSMountRequest) (*models.NFSMountCheckResponse, error) {
	url := checkutils.PrepareEndPoint(ss.host, ss.port, constants.NFS_MOUNT_API_PATH)
	resp, err := httputils.MakeRequest(http.MethodPost, url, body)
	if err != nil {
		ss.log.Error("Error while triggering NFS Mount API from Batch Check API: ", err)
		return nil, err
	}
	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		ss.log.Error("error while connecting to the endpoint:%s", err)
		return nil, err
	}
	apiResp := &models.NFSMountCheckResponse{}
	err = json.Unmarshal(respBody, apiResp)
	if err != nil {
		ss.log.Error("Error while reading unmarshalling response of NFS Mount respons from batch Check API : ", err)
		return apiResp, err
	}
	return apiResp, nil
}

// constructErrorResult constructs the error response when recived from the API
func constructErrorResult(config *models.Config, err error) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse

	hostMap := configutils.GetNodeTypeMap(config.Hardware)
	for ip, types := range hostMap {
		for i := 0; i < len(types); i++ {
			result = append(result, checkutils.PrepareTriggerResponse(nil, ip, types[i], err.Error(), "", "", true))
		}
	}

	return result
}

// constructSuccessResult returns the result if we got a response the API
func constructSuccessResult(resp models.NFSMountCheckResponse) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	for _, res := range resp.Result {
		isPassed := true
		for _, check := range res.CheckList {
			if !check.Passed {
				isPassed = false
			}
		}
		result = append(result,
			models.CheckTriggerResponse{
				Status: resp.Status,
				Result: models.ApiResult{
					Checks: res.CheckList,
					Passed: isPassed,
				},
				NodeType: res.NodeType,
				Host:     res.IP,
			})
	}

	return result
}

func (ss *NfsBackupConfigCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}

func isBackupEmpty(backup *models.Backup) bool {
	return (backup.FileSystem.MountLocation == "")
}

func nilNFSMountBackupResp(config *models.Config, checktype string) []models.CheckTriggerResponse {
	resps := []models.CheckTriggerResponse{}
	for _, ip := range config.Hardware.AutomateNodeIps {
		resps = append(resps, trigger.GetSkippedTriggerCheckResp(ip, checktype, constants.AUTOMATE))
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		resps = append(resps, trigger.GetSkippedTriggerCheckResp(ip, checktype, constants.CHEF_INFRA_SERVER))
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		resps = append(resps, trigger.GetSkippedTriggerCheckResp(ip, checktype, constants.POSTGRESQL))
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		resps = append(resps, trigger.GetSkippedTriggerCheckResp(ip, checktype, constants.OPENSEARCH))
	}

	return resps
}

func emptyNFSMountBackupResp(config *models.Config, checktype string) []models.CheckTriggerResponse {
	resps := []models.CheckTriggerResponse{}
	for _, ip := range config.Hardware.AutomateNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.AUTOMATE, "MountLocation is missing"))
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.CHEF_INFRA_SERVER, "MountLocation is missing"))
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.POSTGRESQL, "MountLocation is missing"))
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.OPENSEARCH, "MountLocation is missing"))
	}

	return resps
}
