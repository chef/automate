package batchcheckservice

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/arrayutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"
)

type IBatchCheckService interface {
	BatchCheck(checks []string, config models.Config) (models.BatchCheckResponse, error)
}

type IStartMockServer interface {
	TriggerRequest(url string, mockServerRequestBody models.MockServerRequestBody) models.StartMockServerFromBatchServiceResponse
}

type BatchCheckService struct {
	CheckTrigger     trigger.CheckTrigger
	port             string
	log              logger.Logger
	mockServerClient IStartMockServer
}

type MockServerClient struct{}

func NewMockServerClient() *MockServerClient {
	return &MockServerClient{}
}

func NewBatchCheckService(trigger trigger.CheckTrigger, log logger.Logger, port string) *BatchCheckService {
	return &BatchCheckService{
		CheckTrigger:     trigger,
		port:             port,
		log:              log,
		mockServerClient: NewMockServerClient(),
	}
}

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) (models.BatchCheckResponse, error) {
	//batchApisResultMap := make(map[string][]models.ApiResult)
	var bastionChecks = stringutils.SliceIntersection(checks, constants.GetBastionChecks())
	var remoteChecks = stringutils.SliceIntersection(checks, constants.GetRemoteChecks())
	checkTriggerRespMap := make(map[string][]models.CheckTriggerResponse)

	// Get bastion check trigger resp
	if len(bastionChecks) > 0 {
		checkTriggerRespMap = getBastionCheckResp(ss, bastionChecks, config)
	}

	if len(remoteChecks) > 0 {
		startMockServer, err := ss.shouldStartMockServer(remoteChecks)
		if err != nil {
			return models.BatchCheckResponse{}, err
		}
		if startMockServer {
			arr = ss.startMockServer(remoteChecks, config.Hardware)
		}
	}

	if len(remoteChecks) > 0 {
		for _, check := range remoteChecks {
			resp := ss.RunRemoteCheck(check, config)
			checkTriggerRespMap[check] = resp
		}
	}

	if len(arr) > 0 {
		for _, req := range arr {
			stopMockServerRequestBody := models.MockServerRequestBody{
				Port:     req.Port,
				Protocol: constants.TCP,
				Cert:     "",
				Key:      "",
			}
			go ss.stopMockServerOnHostAndPort(req.Host, ss.port, stopMockServerRequestBody)
		}
	}
	// stop mock services
	return constructBatchCheckResponse(checkTriggerRespMap, append(bastionChecks, remoteChecks...)), nil
}

func (ss *BatchCheckService) shouldStartMockServer(remoteChecks []string) (bool, error) {
	deploymentState, err := ss.GetDeploymentState()
	if err != nil {
		ss.log.Error("Error while calling status api from batch check service:", err)
		return false, err
	}
	return deploymentState == "pre_deploy", nil
}

func (ss *BatchCheckService) startMockServer(remoteChecks []string, hardwareDetails models.Hardware) []models.StartMockServerFromBatchServiceResponse {
	nodeTypePortMap := map[string]map[string][]int{
		constants.AUTOMATE: {
			constants.TCP:   []int{},
			constants.UDP:   []int{},
			constants.HTTPS: []int{},
		},
		constants.CHEF_INFRA_SERVER: {
			constants.TCP:   []int{},
			constants.UDP:   []int{},
			constants.HTTPS: []int{},
		},
		constants.POSTGRESQL: {
			constants.TCP:   []int{},
			constants.UDP:   []int{},
			constants.HTTPS: []int{},
		},
		constants.OPENSEARCH: {
			constants.TCP:   []int{},
			constants.UDP:   []int{},
			constants.HTTPS: []int{},
		},
	}
	for _, check := range remoteChecks {
		resp := ss.GetPortsToOpenForCheck(check)
		if len(resp) != 0 {
			constructUniquePortMap(resp, &nodeTypePortMap)
		}
	}
	ipMap := map[string][]string{
		constants.AUTOMATE:          hardwareDetails.AutomateNodeIps,
		constants.CHEF_INFRA_SERVER: hardwareDetails.ChefInfraServerNodeIps,
		constants.POSTGRESQL:        hardwareDetails.PostgresqlNodeIps,
		constants.OPENSEARCH:        hardwareDetails.OpenSearchNodeIps,
	}

	nodeTypeAndIpWithPortProtocolMap := make(map[string]map[string][]int)
	for nodeType, ipArray := range ipMap {
		for _, ip := range ipArray {
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip] = make(map[string][]int)
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip][constants.TCP] = nodeTypePortMap[nodeType][constants.TCP]
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip][constants.UDP] = nodeTypePortMap[nodeType][constants.UDP]
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip][constants.HTTPS] = nodeTypePortMap[nodeType][constants.HTTPS]
		}
	}

	ipPortProtocolMap := make(map[string]bool)
	startMockServerChannel := make(chan models.StartMockServerFromBatchServiceResponse)
	totalReq := 0
	for nodeTypeWithIp, protocolMap := range nodeTypeAndIpWithPortProtocolMap {
		if len(protocolMap[constants.TCP]) != 0 {
			for _, port := range protocolMap[constants.TCP] {
				startMockServerRequestBody := models.MockServerRequestBody{
					Port:     port,
					Protocol: constants.TCP,
					Cert:     "",
					Key:      "",
				}
				host := getHostFromNodeTypeAndIpCombination(nodeTypeWithIp)
				key := fmt.Sprint(host, "_", constants.TCP, "_", port)

				if !ipPortProtocolMap[key] {
					fmt.Println("Request triggered for", key)
					go ss.startMockServerOnHostAndPort(host, ss.port, startMockServerRequestBody, startMockServerChannel)
					totalReq = totalReq + 1
					ipPortProtocolMap[key] = true
				}
			}
		}
		if len(protocolMap[constants.UDP]) != 0 {
			for _, port := range protocolMap[constants.UDP] {
				startMockServerRequestBody := models.MockServerRequestBody{
					Port:     port,
					Protocol: constants.UDP,
					Cert:     "",
					Key:      "",
				}
				host := getHostFromNodeTypeAndIpCombination(nodeTypeWithIp)
				key := fmt.Sprint(host, "_", constants.UDP, "_", port)

				if !ipPortProtocolMap[key] {
					fmt.Println("Request triggered for", key)
					go ss.startMockServerOnHostAndPort(host, ss.port, startMockServerRequestBody, startMockServerChannel)
					totalReq = totalReq + 1
					ipPortProtocolMap[key] = true
				}
			}
		}
		if len(protocolMap[constants.HTTPS]) != 0 {
			for _, port := range protocolMap[constants.HTTPS] {
				startMockServerRequestBody := models.MockServerRequestBody{
					Port:     port,
					Protocol: constants.HTTPS,
					Cert:     "",
					Key:      "",
				}
				host := getHostFromNodeTypeAndIpCombination(nodeTypeWithIp)
				key := fmt.Sprint(host, "_", constants.HTTPS, "_", port)

				if !ipPortProtocolMap[key] {
					fmt.Println("Request triggered for", key)
					go ss.startMockServerOnHostAndPort(host, ss.port, startMockServerRequestBody, startMockServerChannel)
					totalReq = totalReq + 1
					ipPortProtocolMap[key] = true
				}
			}
		}
	}

	arr := []models.StartMockServerFromBatchServiceResponse{}
	for i := 0; i < totalReq; i++ {
		result := <-startMockServerChannel
		arr = append(arr, result)
	}
	defer close(startMockServerChannel)
	fmt.Println("reponse of all trigger", arr)
	return arr
}

func getHostFromNodeTypeAndIpCombination(nodeTypeWithIp string) string {
	return nodeTypeWithIp[strings.Index(nodeTypeWithIp, "_")+1:]
}

func constructUniquePortMap(resp map[string]map[string][]int, nodeTypePortMap *map[string]map[string][]int) {
	nodeTypeMap := []string{
		constants.AUTOMATE,
		constants.CHEF_INFRA_SERVER,
		constants.POSTGRESQL,
		constants.OPENSEARCH,
	}

	for _, nodeType := range nodeTypeMap {
		if resp[nodeType] != nil {
			if resp[nodeType][constants.TCP] != nil {
				newPortsToBeAdded := arrayutils.SliceDifference(resp[nodeType][constants.TCP], (*nodeTypePortMap)[nodeType][constants.TCP])
				(*nodeTypePortMap)[nodeType][constants.TCP] = append((*nodeTypePortMap)[nodeType][constants.TCP], newPortsToBeAdded...)
			}
			if resp[nodeType][constants.UDP] != nil {
				newPortsToBeAdded := arrayutils.SliceDifference(resp[nodeType][constants.UDP], (*nodeTypePortMap)[nodeType][constants.UDP])
				(*nodeTypePortMap)[nodeType][constants.UDP] = append((*nodeTypePortMap)[nodeType][constants.UDP], newPortsToBeAdded...)
			}
			if resp[nodeType][constants.HTTPS] != nil {
				newPortsToBeAdded := arrayutils.SliceDifference(resp[nodeType][constants.HTTPS], (*nodeTypePortMap)[nodeType][constants.HTTPS])
				(*nodeTypePortMap)[nodeType][constants.HTTPS] = append((*nodeTypePortMap)[nodeType][constants.HTTPS], newPortsToBeAdded...)
			}
		}
	}
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

func (ss *BatchCheckService) GetPortsToOpenForCheck(check string) map[string]map[string][]int {
	return ss.getCheckInstance(check).GetPortsForMockServer()
}

func (ss *BatchCheckService) GetDeploymentState() (string, error) {
	url := fmt.Sprintf("%s:%s%s", constants.LOCAL_HOST_URL, ss.port, constants.STATUS_API_PATH)
	resp, err := httputils.MakeRequest(http.MethodGet, url, nil)
	if err != nil {
		ss.log.Error("Error while calling status api from batch check service:", err)
		return "", err
	}
	var statusApiResponse models.StatusApiResponse
	err = json.NewDecoder(resp.Body).Decode(&statusApiResponse)
	if err != nil {
		ss.log.Error("Error while reading unmarshalling response of status api from batch check service:", err)
		return "", err
	}
	if len(*statusApiResponse.Result.Services) == 0 && statusApiResponse.Result.Error != "" {
		return "pre_deploy", nil
	}
	return "post_deploy", nil
}

func (ss *BatchCheckService) startMockServerOnHostAndPort(host, port string, startMockServerRequestBody models.MockServerRequestBody, respChan chan models.StartMockServerFromBatchServiceResponse) {
	fmt.Println("Received request to run mock server on host", host, "with body", startMockServerRequestBody)
	url := fmt.Sprintf("%s%s:%s%s", "http://", host, port, constants.START_MOCK_SERVER)
	fmt.Println("url for request", url)
	resp := ss.mockServerClient.TriggerRequest(url, startMockServerRequestBody)
	resp.Host = host
	respChan <- resp
}

func (ss *BatchCheckService) stopMockServerOnHostAndPort(host, port string, stopMockServerRequestBody models.MockServerRequestBody) {
	fmt.Println("Received request to run mock server on host", host, "with body", stopMockServerRequestBody)
	url := fmt.Sprintf("%s%s:%s%s", "http://", host, port, constants.STOP_MOCK_SERVER)
	fmt.Println("url for request", url)
	resp := ss.mockServerClient.TriggerRequest(url, stopMockServerRequestBody)
	resp.Host = host
	fmt.Println("Stop mock server resp", resp)
}

func (ss *MockServerClient) TriggerRequest(url string, startMockServerRequestBody models.MockServerRequestBody) models.StartMockServerFromBatchServiceResponse {
	_, err := httputils.MakeRequest(http.MethodPost, url, startMockServerRequestBody)
	return models.StartMockServerFromBatchServiceResponse{
		Error:      err,
		StatusCode: 200,
		Protocol:   startMockServerRequestBody.Protocol,
		Port:       startMockServerRequestBody.Port,
	}
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
	result := constructResult(ipMap)

	return models.BatchCheckResponse{
		Status: "SUCCESS",
		Result: result,
	}
}

func constructResult(ipMap map[string][]models.CheckTriggerResponse) []models.BatchCheckResult {
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
			resultArray = append(resultArray, checkResult.Result)
		}
		result[resultIndex].Tests = resultArray
		resultIndex = resultIndex + 1
	}

	return result
}

func getBastionCheckResp(ss *BatchCheckService, bastionChecks []string, config models.Config) map[string][]models.CheckTriggerResponse {

	checkTriggerRespMap := make(map[string][]models.CheckTriggerResponse)
	bastionCheckResultChan := make(chan []models.CheckTriggerResponse, len(bastionChecks))

	// Trigger the routine
	for _, check := range bastionChecks {
		go ss.RunBastionCheck(check, config, bastionCheckResultChan)
	}

	// iterate over the chan and take the value out and populate checkTriggerRespMap
	for i := 0; i < len(bastionChecks); i++ {
		var message string
		result := <-bastionCheckResultChan

		if len(result) > 0 {
			message = constants.GetCheckMessageByName(result[0].CheckType)
		}

		for i, _ := range result {
			result[i].Result.Check = result[i].CheckType
			result[i].Result.Message = message
		}

		if len(result) > 0 {
			checkTriggerRespMap[result[0].CheckType] = result
		}

	}
	defer close(bastionCheckResultChan)

	return checkTriggerRespMap
}

func getRemoteCheckResp(ss *BatchCheckService, remoteChecks []string, config models.Config) map[string][]models.CheckTriggerResponse {
	checkTriggerRespMap := make(map[string][]models.CheckTriggerResponse)
	for _, check := range remoteChecks {
		resp := ss.RunRemoteCheck(check, config)

		message := constants.GetCheckMessageByName(check)

		for ind, _ := range resp {
			resp[ind].CheckType = check
			resp[ind].Result.Check = check
			resp[ind].Result.Message = message
		}
		checkTriggerRespMap[check] = resp
	}
	return checkTriggerRespMap
}
