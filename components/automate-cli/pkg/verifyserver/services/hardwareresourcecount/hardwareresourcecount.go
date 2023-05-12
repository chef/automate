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
	res := ValidateHardwareResources(minNodeCount, reqNodeCount, nodetype, ip, set, setbackend)
	respMap := make(map[string]models.HardwareResourceResponse)
	respMap[key] = res
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
		check = createCheck("IP address", true, constants.UNIQUE_SUCCESS_MESSAGE, "", "")
	} else {
		check = createCheck("IP address", false, "", constants.UNIQUE_ERROR_MESSAGE, fmt.Sprintf(constants.UNIQUE_RESOLUTION_MESSAGE, nodetype))
	}
	return check
}

func ValidFormat(ip string) models.Checks {
	var check = models.Checks{}
	if net.ParseIP(ip) == nil {
		check = createCheck("IP address", false, "", constants.VALID_FORMAT_ERROR_MESSAGE, fmt.Sprintf(constants.VALID_FORMAT_RESOLUTION_MESSAGE, ip))
	} else {
		check = createCheck("IP address", true, constants.VALID_FORMAT_SUCCESS_MESSAGE, "", "")
	}
	return check
}

// This function will check if any of the frontend nodes(Automate and CS) are shared with any of the backend nodes(Postgres and Opensearch) and vice-versa.
func SharedIP(nodetype, ip string, set map[string]string) models.Checks {
	var check = models.Checks{}
	if set[ip] == "" {
		check = createCheck("IP address", true, constants.SHARED_SUCCESS_MESSAGE, "", "")
	} else {
		check = createCheck("IP address", false, "", fmt.Sprintf(constants.SHARED_ERROR_MESSAGE, nodetype, set[ip]), fmt.Sprintf(constants.SHARED_RESOLUTION_MESSAGE, nodetype, set[ip]))
	}
	return check
}

// This function will check if the node count we are getting in request is fulfilling the minimum HA requirements.
func ValidCount(minNodeCount, reqNodeCount int, nodetype string) models.Checks {
	var check = models.Checks{}
	if reqNodeCount >= minNodeCount {
		check = createCheck("IP address", true, fmt.Sprintf(constants.VALID_COUNT_SUCCESS_MESSAGE, nodetype), "", "")
	} else {
		check = createCheck("IP address", false, "", fmt.Sprintf(constants.VALID_COUNT_ERROR_MESSAGE, nodetype), fmt.Sprintf(constants.VALID_COUNT_RESOLUTION_MESSAGE, nodetype))
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
	var res = []models.HardwareResourceResponse{}
	is_managed_services := false
	ch := make(chan map[string]models.HardwareResourceResponse)

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
	var orderList []string

	//These maps are used for storing all the nodes of particular services.
	setAutomate := make(map[string]string)
	setChefServer := make(map[string]string)
	setPostgresql := make(map[string]string)
	setOpensearch := make(map[string]string)
	setBackend := make(map[string]string)
	setFrontend := make(map[string]string)
	for _, ip := range req.AutomateNodeIps {
		setAutomate[ip] = "Automate"
		setFrontend[ip] = "Automate"
	}
	for _, ip := range req.ChefInfraServerNodeIps {
		setChefServer[ip] = "Chef-infra-server"
		setFrontend[ip] = "Chef-infra-server"
	}
	for _, ip := range req.PostgresqlNodeIps {
		setPostgresql[ip] = "Postgresql"
		setBackend[ip] = "Postgresql"
	}
	for _, ip := range req.OpenSearchNodeIps {
		setOpensearch[ip] = "Opensearch"
		setBackend[ip] = "Opensearch"
	}

	for index, ip := range req.AutomateNodeIps {
		key := "automate" + ip + strconv.Itoa(index)
		go RunHardwareResourceCountCheck(minNodeCount.AutomateCount, req.AutomateNodeCount, "Automate", ip, setAutomate, setBackend, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range req.ChefInfraServerNodeIps {
		key := "chef-infra-server" + ip + strconv.Itoa(index)
		go RunHardwareResourceCountCheck(minNodeCount.ChefInfraServerCount, req.ChefInfraServerNodeCount, "Chef-infra-server", ip, setChefServer, setBackend, ch, key)
		orderList = append(orderList, key)
	}

	if !is_managed_services {
		for index, ip := range req.PostgresqlNodeIps {
			key := "postgresql" + ip + strconv.Itoa(index)
			go RunHardwareResourceCountCheck(minNodeCount.PostgresqlCount, req.PostgresqlNodeCount, "Postgresql", ip, setPostgresql, setFrontend, ch, key)
			orderList = append(orderList, key)
		}

		for index, ip := range req.OpenSearchNodeIps {
			key := "opensearch" + ip + strconv.Itoa(index)
			go RunHardwareResourceCountCheck(minNodeCount.OpenSearchCount, req.OpenSearchNodeCount, "Opensearch", ip, setOpensearch, setFrontend, ch, key)
			orderList = append(orderList, key)
		}
	}

	for i := 0; i < len(orderList); i++ {
		tempResponse := <-ch
		for k, v := range tempResponse {
			hardwareResultMap[k] = v
		}
	}

	for _, e := range orderList {
		res = append(res, hardwareResultMap[e])
	}

	return res
}
