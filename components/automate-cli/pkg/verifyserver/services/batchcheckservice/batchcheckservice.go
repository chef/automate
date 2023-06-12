package batchcheckservice

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/arrayutils"
	"github.com/chef/automate/lib/certgenerateutils"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"
)

type IBatchCheckService interface {
	BatchCheck(checks []string, config models.Config) (models.BatchCheckResponse, error)
}

type BatchCheckService struct {
	CheckTrigger      trigger.CheckTrigger
	port              string
	log               logger.Logger
	httpRequestClient httputils.IHttpRequestClient
	fileUtils         fileutils.FileUtils
}

func NewBatchCheckService(trigger trigger.CheckTrigger, log logger.Logger, port string) *BatchCheckService {
	return &BatchCheckService{
		CheckTrigger:      trigger,
		port:              port,
		log:               log,
		httpRequestClient: httputils.NewHttpRequestClient(),
		fileUtils:         fileutils.NewFileSystemUtils(),
	}
}

func (ss *BatchCheckService) BatchCheck(checks []string, config models.Config) (models.BatchCheckResponse, error) {
	var bastionChecks = stringutils.SliceIntersection(checks, constants.GetBastionChecks())
	var remoteChecks = stringutils.SliceIntersection(checks, constants.GetRemoteChecks())
	checkTriggerRespMap := make(map[string][]models.CheckTriggerResponse)

	// Get bastion check trigger resp
	if len(bastionChecks) > 0 {
		checkTriggerRespMap = getBastionCheckResp(ss, bastionChecks, config)
	}

	successfullyStartedMockServers, notStartedMockServers, err := ss.startMockServerIfNeeded(remoteChecks, config)
	if err != nil {
		ss.log.Error("Error occurred while starting mock server")
		return models.BatchCheckResponse{}, err
	}

	if len(notStartedMockServers) > 0 {
		mockServersStoppedSuccessfully := ss.stopAllMockServers(successfullyStartedMockServers)
		if !mockServersStoppedSuccessfully {
			return models.BatchCheckResponse{}, errors.New("mock server stop failed on some nodes")
		}
		return models.BatchCheckResponse{}, errors.New("mock server start failed on some nodes")
	}
	if len(remoteChecks) > 0 {
		for key, value := range getRemoteCheckResp(ss, remoteChecks, config) {
			checkTriggerRespMap[key] = value
		}
	}
	mockServersStoppedSuccessfully := ss.stopAllMockServers(successfullyStartedMockServers)
	if !mockServersStoppedSuccessfully {
		return models.BatchCheckResponse{}, errors.New("mock server stop failed on some nodes")
	}
	return constructBatchCheckResponse(checkTriggerRespMap, append(bastionChecks, remoteChecks...)), nil
}

func (ss *BatchCheckService) stopAllMockServers(successfullyStartedMockServers []models.MockServerFromBatchServiceResponse) bool {
	if len(successfullyStartedMockServers) > 0 {
		stopMockServerChannel := make(chan models.MockServerFromBatchServiceResponse)
		totalReq := 0
		mockServersStoppedSuccessfully := true
		for _, successfullyStartedMockServer := range successfullyStartedMockServers {
			if successfullyStartedMockServer.Host != "" {
				totalReq = totalReq + 1
				go ss.StopMockServerOnHostAndPort(successfullyStartedMockServer.Host, successfullyStartedMockServer.Protocol, successfullyStartedMockServer.Port, stopMockServerChannel)
			}
		}
		for i := 0; i < totalReq; i++ {
			result := <-stopMockServerChannel
			if result.Error != nil {
				mockServersStoppedSuccessfully = false
				errMsg := fmt.Sprintf("Error occurred while stoping mock server on host %s for port %v for protocol %s", result.Host, result.Port, result.Protocol)
				ss.log.Error(errMsg, result.Error.Error())
			}
		}
		return mockServersStoppedSuccessfully
	}
	return true
}

func (ss *BatchCheckService) startMockServerIfNeeded(remoteChecks []string, config models.Config) ([]models.MockServerFromBatchServiceResponse, []models.MockServerFromBatchServiceResponse, error) {
	successfullyStartedMockServers := []models.MockServerFromBatchServiceResponse{}
	notStartedMockServers := []models.MockServerFromBatchServiceResponse{}

	if len(remoteChecks) > 0 {
		startMockServer, err := ss.shouldStartMockServer(remoteChecks, config)
		if err != nil {
			return successfullyStartedMockServers, notStartedMockServers, err
		}
		if startMockServer {
			successfullyStartedMockServers, notStartedMockServers = ss.StartMockServer(remoteChecks, config.Hardware)
		}
	}
	return successfullyStartedMockServers, notStartedMockServers, nil
}

func (ss *BatchCheckService) shouldStartMockServer(remoteChecks []string, config models.Config) (bool, error) {
	if stringutils.SliceContains(remoteChecks, constants.FIREWALL) || stringutils.SliceContains(remoteChecks, constants.FQDN) {
		deploymentState, err := ss.getDeploymentState(config)
		if err != nil {
			ss.log.Error("Error while calling status api from batch check service:", err)
			return false, err
		}
		return deploymentState == constants.PRE_DEPLOY, nil
	}
	return false, nil
}

func (ss *BatchCheckService) createNodeTypeMap(remoteChecks []string) map[string]map[string][]int {
	nodeTypePortMap := map[string]map[string][]int{
		constants.AUTOMATE: {
			constants.TCP:   []int{},
			constants.HTTP:  []int{},
			constants.UDP:   []int{},
			constants.HTTPS: []int{},
		},
		constants.CHEF_INFRA_SERVER: {
			constants.TCP:   []int{},
			constants.HTTP:  []int{},
			constants.UDP:   []int{},
			constants.HTTPS: []int{},
		},
		constants.POSTGRESQL: {
			constants.TCP:   []int{},
			constants.HTTP:  []int{},
			constants.UDP:   []int{},
			constants.HTTPS: []int{},
		},
		constants.OPENSEARCH: {
			constants.TCP:   []int{},
			constants.HTTP:  []int{},
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
	return nodeTypePortMap
}

func (ss *BatchCheckService) StartMockServer(remoteChecks []string, hardwareDetails models.Hardware) ([]models.MockServerFromBatchServiceResponse, []models.MockServerFromBatchServiceResponse) {
	nodeTypePortMap := ss.createNodeTypeMap(remoteChecks)
	ipMap := map[string][]string{
		constants.AUTOMATE:          hardwareDetails.AutomateNodeIps,
		constants.CHEF_INFRA_SERVER: hardwareDetails.ChefInfraServerNodeIps,
		constants.POSTGRESQL:        hardwareDetails.PostgresqlNodeIps,
		constants.OPENSEARCH:        hardwareDetails.OpenSearchNodeIps,
	}

	// construct a map that holds list of ports to start on a particular ip
	nodeTypeAndIpWithPortProtocolMap := make(map[string]map[string][]int)
	for nodeType, ipArray := range ipMap {
		for _, ip := range ipArray {
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip] = make(map[string][]int)
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip][constants.TCP] = nodeTypePortMap[nodeType][constants.TCP]
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip][constants.HTTP] = nodeTypePortMap[nodeType][constants.HTTP]
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip][constants.UDP] = nodeTypePortMap[nodeType][constants.UDP]
			nodeTypeAndIpWithPortProtocolMap[nodeType+"_"+ip][constants.HTTPS] = nodeTypePortMap[nodeType][constants.HTTPS]
		}
	}

	// iterrate over each port for an ip and make request to start server.
	ipPortProtocolMap := make(map[string]bool)
	startMockServerChannel := make(chan models.MockServerFromBatchServiceResponse)
	totalReq := 0
	for nodeTypeWithIp, protocolMap := range nodeTypeAndIpWithPortProtocolMap {
		host := getHostFromNodeTypeAndIpCombination(nodeTypeWithIp)
		startMockServerRequestBody := models.MockServerRequestBody{}
		if len(protocolMap[constants.TCP]) != 0 {
			startMockServerRequestBody.Protocol = constants.TCP
			ss.constructRequestForStartingMockServer(protocolMap, host, startMockServerRequestBody, startMockServerChannel, &ipPortProtocolMap, &totalReq)
		}
		if len(protocolMap[constants.UDP]) != 0 {
			startMockServerRequestBody.Protocol = constants.UDP
			ss.constructRequestForStartingMockServer(protocolMap, host, startMockServerRequestBody, startMockServerChannel, &ipPortProtocolMap, &totalReq)
		}
		if len(protocolMap[constants.HTTP]) != 0 {
			startMockServerRequestBody.Protocol = constants.HTTP
			ss.constructRequestForStartingMockServer(protocolMap, host, startMockServerRequestBody, startMockServerChannel, &ipPortProtocolMap, &totalReq)
		}
		if len(protocolMap[constants.HTTPS]) != 0 {
			startMockServerRequestBody.Protocol = constants.HTTPS
			ss.generateRootCaAndPrivateKeyForHost(host, &startMockServerRequestBody)
			ss.constructRequestForStartingMockServer(protocolMap, host, startMockServerRequestBody, startMockServerChannel, &ipPortProtocolMap, &totalReq)
		}
	}

	successfullyStartedMockServers := []models.MockServerFromBatchServiceResponse{}
	notStartedMockServers := []models.MockServerFromBatchServiceResponse{}
	for i := 0; i < totalReq; i++ {
		result := <-startMockServerChannel
		if result.Error == nil {
			successfullyStartedMockServers = append(successfullyStartedMockServers, result)
		} else {
			notStartedMockServers = append(notStartedMockServers, result)
		}

	}
	defer close(startMockServerChannel)
	return successfullyStartedMockServers, notStartedMockServers
}

func (ss *BatchCheckService) constructRequestForStartingMockServer(protocolMap map[string][]int, host string, startMockServerRequestBody models.MockServerRequestBody, startMockServerChannel chan models.MockServerFromBatchServiceResponse, ipPortProtocolMap *map[string]bool, totalReq *int) {
	protocolType := startMockServerRequestBody.Protocol
	for _, port := range protocolMap[protocolType] {
		startMockServerRequestBody.Port = port
		key := fmt.Sprint(host, "_", protocolType, "_", port)

		if !(*ipPortProtocolMap)[key] {
			go ss.startMockServerOnHostAndPort(host, ss.port, startMockServerRequestBody, startMockServerChannel)
			*totalReq = *totalReq + 1
			(*ipPortProtocolMap)[key] = true
		}
	}
}

func getHostFromNodeTypeAndIpCombination(nodeTypeWithIp string) string {
	return nodeTypeWithIp[strings.Index(nodeTypeWithIp, "_")+1:]
}

func (ss *BatchCheckService) generateRootCaAndPrivateKeyForHost(host string, startMockServerRequestBody *models.MockServerRequestBody) {
	err := certgenerateutils.GenerateCert(host)
	if err != nil {
		ss.log.Error(err)
		return
	}
	certificateFileName := "certificate.pem"
	privateKeyFileName := "private_key.pem"
	cert, err := ss.fileUtils.ReadFile(certificateFileName)
	if err != nil {
		ss.log.Error("Unable to read certificate.pem file", err)
		defer ss.fileUtils.DeleteFile(certificateFileName)
		return
	}
	rootCA := string(cert[:])
	startMockServerRequestBody.Cert = rootCA
	defer ss.fileUtils.DeleteFile(certificateFileName)
	key, err := ss.fileUtils.ReadFile(privateKeyFileName)
	if err != nil {
		ss.log.Error("Unable to read private_key.pem file", err)
		defer ss.fileUtils.DeleteFile(privateKeyFileName)
		return
	}
	privateKey := string(key[:])
	startMockServerRequestBody.Key = privateKey
	defer ss.fileUtils.DeleteFile(privateKeyFileName)
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
			if resp[nodeType][constants.HTTP] != nil {
				newPortsToBeAdded := arrayutils.SliceDifference(resp[nodeType][constants.HTTP], (*nodeTypePortMap)[nodeType][constants.HTTP])
				(*nodeTypePortMap)[nodeType][constants.HTTP] = append((*nodeTypePortMap)[nodeType][constants.HTTP], newPortsToBeAdded...)
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

func (ss *BatchCheckService) getDeploymentState(config models.Config) (string, error) {
	if config.Hardware.AutomateNodeCount > 0 {
		for index, ip := range config.Hardware.AutomateNodeIps {
			result, err := ss.getStatusFromNode(ip)
			if err != nil && index == len(config.Hardware.AutomateNodeIps)-1 {
				errMsg := "received no response for status api from any automate nodes"
				ss.log.Error(errMsg)
				return "", errors.New(errMsg)
			}
			if result != "" {
				return result, nil
			}
		}
	}
	return "", errors.New("automate nodes not present")
}

func (ss *BatchCheckService) getStatusFromNode(ip string) (string, error) {
	url := fmt.Sprintf("http://%s:%s%s", ip, ss.port, constants.STATUS_API_PATH)
	resp, err := ss.httpRequestClient.MakeRequest(http.MethodGet, url, nil)
	if err != nil {
		ss.log.Error("Error while calling status API from batch check service:", err)
		return "", err
	}

	var statusApiResponse models.StatusApiResponse
	err = json.NewDecoder(resp.Body).Decode(&statusApiResponse)
	if err != nil {
		ss.log.Error("Error while unmarshalling response of status API from batch check service:", err)
		return "", err
	}

	if len(*statusApiResponse.Result.Services) == 0 && statusApiResponse.Result.Error != "" {
		return constants.PRE_DEPLOY, nil
	}

	return constants.POST_DEPLOY, nil
}

func (ss *BatchCheckService) startMockServerOnHostAndPort(host, port string, startMockServerRequestBody models.MockServerRequestBody, respChan chan models.MockServerFromBatchServiceResponse) {
	url := fmt.Sprintf("%s%s:%s%s", "http://", host, port, constants.START_MOCK_SERVER)
	resp, err := ss.httpRequestClient.MakeRequest(http.MethodPost, url, startMockServerRequestBody)
	if err != nil && resp == nil {
		ss.log.Error(err.Error())
		respChan <- models.MockServerFromBatchServiceResponse{}
		return
	}
	if err != nil {
		ss.log.Error("Error occurred while starting mock server: ", err.Error())
	}
	chanResponse := models.MockServerFromBatchServiceResponse{
		Host:       host,
		Error:      err,
		StatusCode: resp.StatusCode,
		Protocol:   startMockServerRequestBody.Protocol,
		Port:       startMockServerRequestBody.Port,
	}
	respChan <- chanResponse
}

func (ss *BatchCheckService) StopMockServerOnHostAndPort(host, protocol string, port int, stopMockServerChannel chan models.MockServerFromBatchServiceResponse) {
	stopMockServerRequestBody := models.MockServerRequestBody{
		Port:     port,
		Protocol: protocol,
	}
	url := fmt.Sprintf("%s%s:%s%s", "http://", host, ss.port, constants.STOP_MOCK_SERVER)
	resp, err := ss.httpRequestClient.MakeRequest(http.MethodPost, url, stopMockServerRequestBody)
	if err != nil && resp == nil {
		ss.log.Error("Error occurred while making request to stop mock server: ", err.Error())
		chanResponse := models.MockServerFromBatchServiceResponse{
			Host:       host,
			Error:      err,
			StatusCode: http.StatusInternalServerError,
			Protocol:   stopMockServerRequestBody.Protocol,
			Port:       stopMockServerRequestBody.Port,
		}
		stopMockServerChannel <- chanResponse
		return
	}
	if err != nil {
		ss.log.Error("Error occurred while stoping mock server: ", err.Error())
		chanResponse := models.MockServerFromBatchServiceResponse{
			Host:       host,
			Error:      err,
			StatusCode: resp.StatusCode,
			Protocol:   stopMockServerRequestBody.Protocol,
			Port:       stopMockServerRequestBody.Port,
		}
		stopMockServerChannel <- chanResponse
		return
	}
	ss.log.Debug("Successfully stopped mock server")
	stopMockServerChannel <- models.MockServerFromBatchServiceResponse{}
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
