package nfsmountbackupchecktrigger

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
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

func (nbc *NfsBackupConfigCheck) Run(config models.Config) []models.CheckTriggerResponse {

	nfsMountReq := models.NFSMountRequest{
		AutomateNodeIPs:        config.Hardware.AutomateNodeIps,
		ChefInfraServerNodeIPs: config.Hardware.ChefInfraServerNodeIps,
		PostgresqlNodeIPs:      config.Hardware.PostgresqlNodeIps,
		OpensearchNodeIPs:      config.Hardware.OpenSearchNodeIps,
		MountLocation:          config.Backup.FileSystem.MountLocation,
	}

	nfsMountAPIResponse, err := nbc.TriggerCheckForMountService(nfsMountReq)
	if err != nil {
		response := constructErrorResult(config, err)
		return response

	}
	result := constructSuccessResult(*nfsMountAPIResponse)

	return result

}

// TriggerHardwareResourceCountCheck - Call the Hardware resource API and format response
func (ss *NfsBackupConfigCheck) TriggerCheckForMountService(body interface{}) (*models.NFSMountCheckResponse, error) {
	url := fmt.Sprintf("http://%s:%s%s", ss.host, ss.port, constants.NFS_MOUNT_API_PATH)
	fmt.Println(url)
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

func constructErrorResult(config models.Config, err error) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	for _, ip := range config.Hardware.AutomateNodeIps {
		result = prepareErrorResult(result, ip, constants.AUTOMATE, err)
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		result = prepareErrorResult(result, ip, constants.CHEF_INFRA_SERVER, err)
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		result = prepareErrorResult(result, ip, constants.POSTGRESQL, err)
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		result = prepareErrorResult(result, ip, constants.OPENSEARCH, err)
	}
	return result
}

func prepareErrorResult(finalResult []models.CheckTriggerResponse, host, nodeType string, err error) []models.CheckTriggerResponse {
	finalResult = append(finalResult, models.CheckTriggerResponse{
		Host:     host,
		NodeType: nodeType,
		Error: &fiber.Error{
			Code:    http.StatusInternalServerError,
			Message: fmt.Sprintf("Error while triggering NFS Mount API from Batch Check API: %s", err.Error()),
		},
	})
	return finalResult
}

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
