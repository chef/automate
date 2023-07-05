package nfsmountbackupchecktrigger

import (
	"encoding/json"
	"errors"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/logger"
)

type NfsBackupConfigCheck struct {
	log               logger.Logger
	port              string
	host              string
	httpRequestClient httputils.HTTPClient
}

func NewNfsBackupConfigCheck(log logger.Logger, port string) *NfsBackupConfigCheck {
	return &NfsBackupConfigCheck{
		log:               log,
		port:              port,
		host:              constants.LOCALHOST,
		httpRequestClient: httputils.NewClient(log),
	}
}

func (nbc *NfsBackupConfigCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	if config.Hardware == nil {
		return trigger.HardwareNil(constants.NFS_BACKUP_CONFIG, true, true, false)
	}
	if config.Backup == nil || config.Backup.FileSystem == nil {
		return trigger.ConstructNilResp(config, constants.NFS_BACKUP_CONFIG)
	}
	if isMountLocationEmpty(config.Backup) {
		return trigger.ConstructEmptyResp(config, constants.NFS_BACKUP_CONFIG, constants.MOUNT_LOCATION_MISSING)
	}

	nfsMountReq := models.NFSMountRequest{
		ExternalDbType:         config.ExternalDbType,
		AutomateNodeIPs:        config.Hardware.AutomateNodeIps,
		ChefInfraServerNodeIPs: config.Hardware.ChefInfraServerNodeIps,
		PostgresqlNodeIPs:      config.Hardware.PostgresqlNodeIps,
		OpensearchNodeIPs:      config.Hardware.OpenSearchNodeIps,
		MountLocation:          config.Backup.FileSystem.MountLocation,
	}

	//Triggers only one API call for nfs mount API
	nfsMountAPIResponse, err := nbc.triggerCheckForMountService(nfsMountReq)
	if err != nil {
		if nfsMountAPIResponse != nil {
			return constructErrorResult(config, nfsMountAPIResponse.Error.Message, nfsMountAPIResponse.Error.Code)
		}
		return constructErrorResult(config, err.Error(), 500)

	}
	result := constructSuccessResult(*nfsMountAPIResponse)

	return result

}

// triggerCheckForMountService - Call the Hardware resource API and format response
func (ss *NfsBackupConfigCheck) triggerCheckForMountService(body models.NFSMountRequest) (*models.NFSMountCheckResponse, error) {
	url := checkutils.PrepareEndPoint(ss.host, ss.port, constants.NFS_MOUNT_API_PATH)
	_, resp, err := ss.httpRequestClient.MakeRequest(http.MethodPost, url, body)
	if err != nil {
		ss.log.Error("Error while triggering NFS Mount API from Batch Check API: ", err)
		return nil, err
	}
	ss.log.Debug("the value of response from the trigger = ", string(resp))
	apiResp := &models.NFSMountCheckResponse{}
	err = json.Unmarshal(resp, &apiResp)
	if err != nil {
		ss.log.Error("Error while reading unmarshalled response of NFS Mount response from batch Check API : ", err)
		return nil, err
	}
	if apiResp.Error.Message != "" {
		return apiResp, errors.New(apiResp.Error.Message)
	}
	return apiResp, nil
}

// constructErrorResult constructs the error response when recived from the API
func constructErrorResult(config *models.Config, errMessage string, statusCode int) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse

	hostMap := configutils.GetNodeTypeMap(config.Hardware)
	for ip, types := range hostMap {
		for i := 0; i < len(types); i++ {
			result = append(result, checkutils.PrepareTriggerResponse(nil, ip, types[i], errMessage, "", "", true, statusCode))
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

func isMountLocationEmpty(backup *models.Backup) bool {
	return (backup.FileSystem.MountLocation == "")
}
