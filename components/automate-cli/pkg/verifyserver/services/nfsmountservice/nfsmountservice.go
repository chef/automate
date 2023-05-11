package nfsmountservice

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/gofiber/fiber"
	"io"
	"io/ioutil"
	"net/http"
	"strconv"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/chef/automate/lib/logger"
)

type INFSService interface {
	GetNFSMountDetails(models.NFSMountRequest) *[]models.NFSMountResponse
	//MakeConcurrentCall(string, string, string, chan string, map[string]models.NFSMountResponse, map[string]models.NFSMountLocResponse, string, map[models.NFSMountLocResponse]int)
	//DoAPICall(string, string, string, map[string]models.NFSMountLocResponse, string, map[models.NFSMountLocResponse]int) models.NFSMountResponse
}

type NFSMountService struct {
	port string
	log  logger.Logger
}

func NewNFSMountService(log logger.Logger, port string) *NFSMountService {
	return &NFSMountService{
		port: port,
		log:  log,
	}
}

func (nm *NFSMountService) MakeConcurrentCall(ip string, nodeType string, mountLocation string, respChan chan map[string]TempResponse, key string) {
	res, err := nm.DoAPICall(ip, mountLocation)

	mountResp := prepareMountResp(ip, nodeType, mountLocation, res, err)

	respMap := make(map[string]TempResponse)
	tempResp := TempResponse{
		MountResp: mountResp,
	}
	if err == nil {
		tempResp.MountLocRes = *res
	}
	respMap[key] = tempResp
	respChan <- respMap
}

func prepareMountResp(ip, nodeType, mountLocation string, mountLocResp *models.NFSMountLocResponse, err error) models.NFSMountResponse {
	node := models.NFSMountResponse{}
	node.IP = ip
	node.NodeType = nodeType

	if err != nil {
		node.Error = fiber.NewError(http.StatusBadRequest, err.Error())
		return node
	}
	CheckMount(mountLocation, &node, mountLocResp)
	return node
}

func MakeRespBody(respBody *[]models.NFSMountResponse, countMap map[models.NFSMountLocResponse]int,
	orderList []string, nfsMountResultMap map[string]models.NFSMountResponse, shareMap map[string]models.NFSMountLocResponse) {
	compareWith := models.NFSMountLocResponse{}
	currMax := 0
	// Finding the Majority element and putting in the compareWith variable for the later comparisons
	for k, v := range countMap {
		if v > currMax {
			currMax = v
			compareWith = k
		}
	}

	for _, key := range orderList {
		val := nfsMountResultMap[key]
		// Need to check error first. Because while making call to the API if something get wrong
		// then we are storing checkList as nil and putting the error in Error field.
		if val.Error == nil {
			// if nfs is mounted then in our first check for the particular node it will be true. hence we are passing that which checking shareability
			// Test2 - Check for NFS Volume is Shared among all nodes or not
			if val.CheckList[0].Passed {
				CheckShare(shareMap[key], compareWith, &val, true)
			} else {
				CheckShare(shareMap[key], compareWith, &val, false)
			}
		}
		*respBody = append(*respBody, val)
	}
}

type TempResponse struct {
	MountLocRes models.NFSMountLocResponse
	MountResp   models.NFSMountResponse
}

func (nm *NFSMountService) GetNFSMountDetails(reqBody models.NFSMountRequest) *[]models.NFSMountResponse {
	respBody := new([]models.NFSMountResponse)
	// For storing the output of go routine temporary in nfsMountResultMap
	nfsMountResultMap := make(map[string]models.NFSMountResponse)
	ch := make(chan map[string]TempResponse)
	// Will use these both the maps in checking the shareability
	// countMap will help us to find the Majority element.
	// we are storing each node response in share map corresponds to unique key.
	// So that we don't need to again call the API for checking the shareability
	shareMap := make(map[string]models.NFSMountLocResponse)
	countMap := make(map[models.NFSMountLocResponse]int)
	// using orderList to store the order. so we can generate the same reponse order which create response.
	var orderList []string

	for index, ip := range reqBody.AutomateNodeIPs {
		key := "automate" + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, "automate", reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.ChefInfraServerNodeIPs {
		key := "chef-infra-server" + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, "chef-infra-server", reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.PostgresqlNodeIPs {
		key := "postgresql" + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, "postgresql", reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.OpensearchNodeIPs {
		key := "opensearch" + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, "opensearch", reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	// it will help us to hold the program until all triggered go routines cameback
	for i := 0; i < len(orderList); i++ {
		select {
		case tempResponse := <-ch:
			for k, v := range tempResponse {
				nfsMountResultMap[k] = v.MountResp
				if v.MountResp.Error == nil {
					shareMap[k] = v.MountLocRes
					countMap[v.MountLocRes] = countMap[v.MountLocRes] + 1
				}
			}
		}
	}

	MakeRespBody(respBody, countMap, orderList, nfsMountResultMap, shareMap)
	return respBody
}

func (nm *NFSMountService) DoAPICall(ip string, mountLocation string) (*models.NFSMountLocResponse, error) {

	reqURL := fmt.Sprintf("http://%s:%s%s", ip, nm.port, "/api/v1/fetch/nfs-mount-loc")

	reqBody := models.NFSMountLocRequest{
		MountLocation: mountLocation,
	}

	// Converting Body into json encoding for passing into request
	reqBodyJSON, err := json.Marshal(reqBody)
	if err != nil {
		return nil, errors.New("Failed to Marshal: " + err.Error())
	}

	httpReq, err := http.NewRequest(http.MethodPost, reqURL, bytes.NewBuffer(reqBodyJSON))
	if err != nil {
		return nil, errors.New("Failed to Create HTTP request: " + err.Error())
	}

	client := http.Client{
		Timeout: 30 * time.Second,
	}

	// Making the actual call
	resp, err := client.Do(httpReq)
	if err != nil {
		return nil, errors.New("Failed to send the HTTP request: " + err.Error())
	}

	return GetResultStructFromRespBody(resp.Body)
}

func GetResultStructFromRespBody(respBody io.Reader) (*models.NFSMountLocResponse, error) {
	body, err := ioutil.ReadAll(respBody) // nosemgrep
	if err != nil {
		return nil, errors.New("Cannot able to read data from response body: " + err.Error())
	}

	// Converting API Response Body into Generic Response Struct.
	APIRespStruct := response.ResponseBody{}
	err = json.Unmarshal(body, &APIRespStruct)
	if err != nil {
		return nil, errors.New("Failed to Unmarshal: " + err.Error())
	}

	// If API(/nfs-mount-loc) is itself failing.
	if APIRespStruct.Error != nil {
		return nil, APIRespStruct.Error
	}

	// Converting interface into JSON encoding. APIResp.Result is a interface and for accessing the values we are converting that into json.
	resultByte, err := json.Marshal(APIRespStruct.Result)
	if err != nil {
		return nil, errors.New("Failed to Marshal: " + err.Error())
	}

	resultField := new(models.NFSMountLocResponse)
	// converting JSON into struct.
	err = json.Unmarshal(resultByte, &resultField)
	if err != nil {
		return nil, errors.New("Failed to Unmarshal: " + err.Error())
	}
	// fmt.Println(resultField)

	return resultField, nil
}

func CheckMount(mountLocation string, node *models.NFSMountResponse, data *models.NFSMountLocResponse) {
	if data.Address != "" {
		check := createCheck("NFS Mount", true, constants.MOUNT_SUCCESS_MSG, "", "")
		node.CheckList = append(node.CheckList, check)
	} else {
		check := createCheck("NFS Mount", false, "", constants.MOUNT_ERROR_MSG, fmt.Sprintf(constants.MOUNT_RESOLUTION_MSG, mountLocation))
		node.CheckList = append(node.CheckList, check)
	}
}

func CheckShare(data models.NFSMountLocResponse, compareWith models.NFSMountLocResponse, node *models.NFSMountResponse, nfsMounted bool) {
	// nfsMounted is holding volume is mounted or not. If volume is not mounted then how we can check it's shareability
	if nfsMounted && data.Address == compareWith.Address && data.Nfs == compareWith.Nfs && data.MountLocation == compareWith.MountLocation {
		check := createCheck("NFS Mount", true, constants.SHARE_SUCCESS_MSG, "", "")
		node.CheckList = append(node.CheckList, check)
	} else {
		check := createCheck("NFS Mount", false, "", fmt.Sprintf(constants.SHARE_ERROR_MSG, compareWith.MountLocation), fmt.Sprintf(constants.SHARE_RESOLUTION_MSG, compareWith.MountLocation))
		node.CheckList = append(node.CheckList, check)
	}
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
