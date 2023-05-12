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
	GetHardwareResourceCount(models.HardwareResourceRequest) []models.HardwareResourceResponse
}

type HardwareResourceCountService struct {
	log logger.Logger
}

func NewHardwareResourceCountService(log logger.Logger) *HardwareResourceCountService {
	return &HardwareResourceCountService{
		log: log,
	}
}

func RunHardwareResourceCountCheck(minNodeCount, reqNodeCount int, nodetype string, ip string, set, setbackend map[string]string, ch chan map[string]models.HardwareResourceResponse, key string) {
	response := ValidateHardwareResources(minNodeCount, reqNodeCount, nodetype, ip, set, setbackend)
	respMap := make(map[string]models.HardwareResourceResponse)
	respMap[key] = response
	ch <- respMap
}

func ValidateHardwareResources(minNodeCount, reqNodeCount int, nodetype string, ip string, set, setbackend map[string]string) models.HardwareResourceResponse {
	var res = models.HardwareResourceResponse{}
	res.NodeType = nodetype
	res.IP = ip

	checks := UniqueIP(nodetype, len(set), reqNodeCount)
	res.Checks = append(res.Checks, checks)

	checks = ValidFormat(ip)
	res.Checks = append(res.Checks, checks)

	checks = SharedIP(nodetype, ip, setbackend)
	res.Checks = append(res.Checks, checks)

	checks = ValidCount(minNodeCount, reqNodeCount, nodetype)
	res.Checks = append(res.Checks, checks)

	return res
}

func UniqueIP(nodetype string, set int, nodecount int) models.Checks {
	var check = models.Checks{}
	if set == nodecount {
		check = createCheck(constants.IP_ADDRESSS, true, constants.UNIQUE_SUCCESS_MESSAGE, "", "")
	} else {
		check = createCheck(constants.IP_ADDRESSS, false, "", constants.UNIQUE_ERROR_MESSAGE, fmt.Sprintf(constants.UNIQUE_RESOLUTION_MESSAGE, nodetype))
	}
	return check
}

func ValidFormat(ip string) models.Checks {
	var check = models.Checks{}
	if net.ParseIP(ip) == nil {
		check = createCheck(constants.IP_ADDRESSS, false, "", constants.VALID_FORMAT_ERROR_MESSAGE, fmt.Sprintf(constants.VALID_FORMAT_RESOLUTION_MESSAGE, ip))
	} else {
		check = createCheck(constants.IP_ADDRESSS, true, constants.VALID_FORMAT_SUCCESS_MESSAGE, "", "")
	}
	return check
}

// This function will check if any of the frontend nodes(Automate and CS) are shared with any of the backend nodes(Postgres and Opensearch) and vice-versa.
func SharedIP(nodetype, ip string, set map[string]string) models.Checks {
	var check = models.Checks{}
	var opposite_cluster_type string

	//This is for giving the appropriate success message.
	if nodetype == constants.AUTOMATE || nodetype == constants.CHEF_INFRA_SERVER {
		opposite_cluster_type = "backend"
	} else {
		opposite_cluster_type = "frontend"
	}

	if set[ip] == "" {
		check = createCheck(constants.IP_ADDRESSS, true, fmt.Sprintf(constants.SHARED_SUCCESS_MESSAGE, opposite_cluster_type), "", "")
	} else {
		check = createCheck(constants.IP_ADDRESSS, false, "", fmt.Sprintf(constants.SHARED_ERROR_MESSAGE, nodetype, set[ip]), fmt.Sprintf(constants.SHARED_RESOLUTION_MESSAGE, nodetype, set[ip]))
	}
	return check
}

// This function will check if the node count we are getting in request is fulfilling the minimum HA requirements.
func ValidCount(minNodeCount, reqNodeCount int, nodetype string) models.Checks {
	var check = models.Checks{}
	if reqNodeCount >= minNodeCount {
		check = createCheck(constants.IP_ADDRESSS, true, fmt.Sprintf(constants.VALID_COUNT_SUCCESS_MESSAGE, nodetype), "", "")
	} else {
		check = createCheck(constants.IP_ADDRESSS, false, "", fmt.Sprintf(constants.VALID_COUNT_ERROR_MESSAGE, nodetype), fmt.Sprintf(constants.VALID_COUNT_RESOLUTION_MESSAGE, nodetype))
	}
	return check
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

func (hrc *HardwareResourceCountService) GetHardwareResourceCount(req models.HardwareResourceRequest) []models.HardwareResourceResponse {
	var response = []models.HardwareResourceResponse{}
	is_managed_services := false
	ch := make(chan map[string]models.HardwareResourceResponse)

	//This structure holds minimum HA requirements as per Public Doc
	var minNodeCount = models.CountPerHARequirements{
		AutomateCount:        2,
		ChefInfraServerCount: 2,
		PostgresqlCount:      3,
		OpenSearchCount:      3,
	}

	if req.PostgresqlNodeCount == 0 && req.OpenSearchNodeCount == 0 {
		is_managed_services = true
	}

	//This map is for temporarily storing the output of go routine.
	hardwareResultMap := make(map[string]models.HardwareResourceResponse)
	//For storing the response in a specified order.
	var hardwareResultOrderList []string

	//These maps are used for storing all the nodes of particular services.
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

	for index, ip := range req.AutomateNodeIps {
		key := constants.AUTOMATE + ip + strconv.Itoa(index)
		go RunHardwareResourceCountCheck(minNodeCount.AutomateCount, req.AutomateNodeCount, constants.AUTOMATE, ip, setAutomate, setBackend, ch, key)
		hardwareResultOrderList = append(hardwareResultOrderList, key)
	}

	for index, ip := range req.ChefInfraServerNodeIps {
		key := constants.CHEF_INFRA_SERVER + ip + strconv.Itoa(index)
		go RunHardwareResourceCountCheck(minNodeCount.ChefInfraServerCount, req.ChefInfraServerNodeCount, constants.CHEF_INFRA_SERVER, ip, setChefServer, setBackend, ch, key)
		hardwareResultOrderList = append(hardwareResultOrderList, key)
	}

	if !is_managed_services {
		for index, ip := range req.PostgresqlNodeIps {
			key := constants.POSTGRESQL + ip + strconv.Itoa(index)
			go RunHardwareResourceCountCheck(minNodeCount.PostgresqlCount, req.PostgresqlNodeCount, constants.POSTGRESQL, ip, setPostgresql, setFrontend, ch, key)
			hardwareResultOrderList = append(hardwareResultOrderList, key)
		}

		for index, ip := range req.OpenSearchNodeIps {
			key := constants.OPENSEARCH + ip + strconv.Itoa(index)
			go RunHardwareResourceCountCheck(minNodeCount.OpenSearchCount, req.OpenSearchNodeCount, constants.OPENSEARCH, ip, setOpensearch, setFrontend, ch, key)
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
