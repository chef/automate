package hardwareresourcecount

import (
	"fmt"
	"net"
	"strconv"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IHardwareResourceCountService interface {
	GetHardwareResourceCount(models.Hardware) []models.HardwareResourceResponse
}

type HardwareResourceCountService struct {
	log logger.Logger
}

func NewHardwareResourceCountService(log logger.Logger) *HardwareResourceCountService {
	return &HardwareResourceCountService{
		log: log,
	}
}

// runHardwareResourceCountCheck function will make call to ValidateHardwareResources function, and store the response in channel.
// In this function, nodeSet parameter contains set of nodes of similar node type.
// OppositeTypeSet parameter contains nodes of opposite type.
// For eg: nodeSet contains automate nodes, then OppositeTypeSet will contains Postgresql and Opensearch Nodes.
func runHardwareResourceCountCheck(reqNodeCount int, nodeType string, ip string, nodeSet, oppositeTypeSet map[string]string, ch chan map[string]models.HardwareResourceResponse, key string) {
	minNodeCount := getMinNodesHARequirement(nodeType)
	response := validateHardwareResources(minNodeCount, reqNodeCount, nodeType, ip, nodeSet, oppositeTypeSet)
	respMap := make(map[string]models.HardwareResourceResponse)
	respMap[key] = response
	ch <- respMap
}

// validateHardwareResources function is mainly used for calling the 4 main checks, and preparing the response.
func validateHardwareResources(minNodeCount, reqNodeCount int, nodeType string, ip string, nodeSet, oppositeTypeSet map[string]string) models.HardwareResourceResponse {
	var res = models.HardwareResourceResponse{}
	res.NodeType = nodeType
	res.IP = ip

	checks := uniqueIP(nodeType, len(nodeSet), reqNodeCount)
	res.Checks = append(res.Checks, checks)

	checks = validFormat(ip)
	res.Checks = append(res.Checks, checks)

	checks = sharedIP(nodeType, ip, oppositeTypeSet)
	res.Checks = append(res.Checks, checks)

	checks = validCount(minNodeCount, len(nodeSet), nodeType)
	res.Checks = append(res.Checks, checks)

	return res
}

// uniqueIP function will check that are we getting unique ips for each service.
// Like for automate if we get node count 2 in request, then we are also getting 2 unique ips of automate.
func uniqueIP(nodeType string, nodeSetLen int, reqNodeCount int) models.Checks {
	if nodeSetLen == reqNodeCount {
		return createCheck(constants.IP_ADDRESSS, true, constants.UNIQUE_SUCCESS_MESSAGE, "", "")
	}
	return createCheck(constants.IP_ADDRESSS, false, "", constants.UNIQUE_ERROR_MESSAGE, fmt.Sprintf(constants.UNIQUE_RESOLUTION_MESSAGE, nodeType))
}

// validFormat function will check that if our ip is in correct format or not.
func validFormat(ip string) models.Checks {
	if net.ParseIP(ip) == nil {
		return createCheck(constants.IP_ADDRESSS, false, "", constants.VALID_FORMAT_ERROR_MESSAGE, fmt.Sprintf(constants.VALID_FORMAT_RESOLUTION_MESSAGE, ip))
	}
	return createCheck(constants.IP_ADDRESSS, true, constants.VALID_FORMAT_SUCCESS_MESSAGE, "", "")
}

// sharedIP function will check if any of the frontend nodes(Automate and CS) are shared with any of the backend nodes(Postgres and Opensearch) and vice-versa.
func sharedIP(nodeType, ip string, oppositeTypeSet map[string]string) models.Checks {
	var oppositeClusterType string

	// This is for giving the appropriate success message.
	if nodeType == constants.AUTOMATE || nodeType == constants.CHEF_INFRA_SERVER {
		oppositeClusterType = constants.BACKEND_CLUSTER
	} else {
		oppositeClusterType = constants.FRONTEND_CLUSTER
	}

	if oppositeTypeSet[ip] == "" {
		return createCheck(constants.IP_ADDRESSS, true, fmt.Sprintf(constants.SHARED_SUCCESS_MESSAGE, oppositeClusterType), "", "")
	}
	return createCheck(constants.IP_ADDRESSS, false, "", fmt.Sprintf(constants.SHARED_ERROR_MESSAGE, nodeType, oppositeTypeSet[ip]), fmt.Sprintf(constants.SHARED_RESOLUTION_MESSAGE, nodeType, oppositeTypeSet[ip]))
}

// validCount function will check if the node count we are getting in request is fulfilling the minimum HA requirements.
func validCount(minNodeCount, nodeSetLen int, nodeType string) models.Checks {
	if nodeSetLen >= minNodeCount {
		return createCheck(constants.IP_ADDRESSS, true, fmt.Sprintf(constants.VALID_COUNT_SUCCESS_MESSAGE, nodeType), "", "")
	}
	return createCheck(constants.IP_ADDRESSS, false, "", fmt.Sprintf(constants.VALID_COUNT_ERROR_MESSAGE, nodeType), fmt.Sprintf(constants.VALID_COUNT_RESOLUTION_MESSAGE, nodeType))
}

func createCheck(title string, passed bool, successMsg, errorMsg, resolutionMsg string) models.Checks {
	return models.Checks{
		Title:         title,
		Passed:        passed,
		SuccessMsg:    successMsg,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
}

// getMinNodesHARequirement function returns minimum HA requirements as per Public Doc
func getMinNodesHARequirement(nodeType string) int {
	if nodeType == constants.AUTOMATE {
		return constants.MIN_AUTOMATE_REQ
	} else if nodeType == constants.CHEF_INFRA_SERVER {
		return constants.MIN_CHEF_INFRA_SERVER_REQ
	} else if nodeType == constants.POSTGRESQL {
		return constants.MIN_POSTGRESQL_REQ
	} else {
		return constants.MIN_OPENSEARCH_REQ
	}
}

// createNodeSet function is used for storing all the nodes of particular services.
func createNodeSet(req models.Hardware) (map[string]string, map[string]string, map[string]string, map[string]string, map[string]string, map[string]string) {
	setAutomate := make(map[string]string)
	setChefServer := make(map[string]string)
	setPostgresql := make(map[string]string)
	setOpensearch := make(map[string]string)
	setBackend := make(map[string]string)
	setFrontend := make(map[string]string)
	for _, ip := range req.AutomateNodeIps {
		setAutomate[ip] = constants.AUTOMATE
		setFrontend[ip] = constants.AUTOMATE
	}
	for _, ip := range req.ChefInfraServerNodeIps {
		setChefServer[ip] = constants.CHEF_INFRA_SERVER
		setFrontend[ip] = constants.CHEF_INFRA_SERVER
	}
	for _, ip := range req.PostgresqlNodeIps {
		setPostgresql[ip] = constants.POSTGRESQL
		setBackend[ip] = constants.POSTGRESQL
	}
	for _, ip := range req.OpenSearchNodeIps {
		setOpensearch[ip] = constants.OPENSEARCH
		setBackend[ip] = constants.OPENSEARCH
	}
	return setAutomate, setChefServer, setPostgresql, setOpensearch, setFrontend, setBackend
}

func (hrc *HardwareResourceCountService) GetHardwareResourceCount(req models.Hardware) []models.HardwareResourceResponse {
	var response = []models.HardwareResourceResponse{}
	isManagedServices := false
	ch := make(chan map[string]models.HardwareResourceResponse)

	if req.PostgresqlNodeCount == 0 && req.OpenSearchNodeCount == 0 {
		isManagedServices = true
	}

	// This map is for temporarily storing the output of go routine.
	hardwareResultMap := make(map[string]models.HardwareResourceResponse)
	// For storing the response in a specified order.
	var hardwareResultOrderList []string

	setAutomate, setChefServer, setPostgresql, setOpensearch, setFrontend, setBackend := createNodeSet(req)

	for index, ip := range req.AutomateNodeIps {
		key := constants.AUTOMATE + ip + strconv.Itoa(index)
		go runHardwareResourceCountCheck(req.AutomateNodeCount, constants.AUTOMATE, ip, setAutomate, setBackend, ch, key)
		hardwareResultOrderList = append(hardwareResultOrderList, key)
	}

	for index, ip := range req.ChefInfraServerNodeIps {
		key := constants.CHEF_INFRA_SERVER + ip + strconv.Itoa(index)
		go runHardwareResourceCountCheck(req.ChefInfraServerNodeCount, constants.CHEF_INFRA_SERVER, ip, setChefServer, setBackend, ch, key)
		hardwareResultOrderList = append(hardwareResultOrderList, key)
	}

	if !isManagedServices {
		for index, ip := range req.PostgresqlNodeIps {
			key := constants.POSTGRESQL + ip + strconv.Itoa(index)
			go runHardwareResourceCountCheck(req.PostgresqlNodeCount, constants.POSTGRESQL, ip, setPostgresql, setFrontend, ch, key)
			hardwareResultOrderList = append(hardwareResultOrderList, key)
		}

		for index, ip := range req.OpenSearchNodeIps {
			key := constants.OPENSEARCH + ip + strconv.Itoa(index)
			go runHardwareResourceCountCheck(req.OpenSearchNodeCount, constants.OPENSEARCH, ip, setOpensearch, setFrontend, ch, key)
			hardwareResultOrderList = append(hardwareResultOrderList, key)
		}
	}

	for i := 0; i < len(hardwareResultOrderList); i++ {
		tempResponse := <-ch
		for k, v := range tempResponse {
			hardwareResultMap[k] = v
		}
	}

	for _, e := range hardwareResultOrderList {
		response = append(response, hardwareResultMap[e])
	}

	return response
}
