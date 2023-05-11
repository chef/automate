package nfsmountservice

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"strconv"
	"time"

	"github.com/gofiber/fiber"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/chef/automate/lib/logger"
)

type INFSService interface {
	GetNFSMountDetails(models.NFSMountRequest) *[]models.NFSMountResponse
}

type NFSMountService struct {
	port string
	log  logger.Logger
}

type TempResponse struct {
	MountLocRes models.NFSMountLocResponse
	MountResp   models.NFSMountResponse
}

func NewNFSMountService(log logger.Logger, port string) *NFSMountService {
	return &NFSMountService{
		port: port,
		log:  log,
	}
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
		key := constants.AUTOMATE + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, constants.AUTOMATE, reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.ChefInfraServerNodeIPs {
		key := constants.CHEF_INFRA_SERVER + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, constants.CHEF_INFRA_SERVER, reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.PostgresqlNodeIPs {
		key := constants.POSTGRESQL + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, constants.POSTGRESQL, reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.OpensearchNodeIPs {
		key := constants.OPENSEARCH + ip + strconv.Itoa(index)
		go nm.MakeConcurrentCall(ip, constants.OPENSEARCH, reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for i := 0; i < len(orderList); i++ {
		tempResponse := <-ch
		for k, v := range tempResponse {
			nfsMountResultMap[k] = v.MountResp
			if v.MountResp.Error == nil {
				shareMap[k] = v.MountLocRes
				countMap[v.MountLocRes] = countMap[v.MountLocRes] + 1
			}
		}
	}
	MakeRespBody(respBody, countMap, orderList, nfsMountResultMap, shareMap, reqBody.MountLocation)
	return respBody
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

func MakeRespBody(respBody *[]models.NFSMountResponse, countMap map[models.NFSMountLocResponse]int, orderList []string,
	nfsMountResultMap map[string]models.NFSMountResponse, shareMap map[string]models.NFSMountLocResponse, mountLocation string) {
	var isShared bool = false
	if len(countMap) == 1 {
		isShared = true
	}

	for _, key := range orderList {
		val := nfsMountResultMap[key]
		// Need to check error first. Because while making call to the API if something get wrong
		// then we are storing checkList as nil and putting the error in Error field.
		if val.Error == nil {
			// Test2 - Check for NFS Volume is Shared among all nodes or not
			CheckShare(val.CheckList[0].Passed, isShared, mountLocation, shareMap[key], &val)
		}
		*respBody = append(*respBody, val)
	}
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

func CheckShare(nfsMounted, nfsShared bool, mountLocation string, data models.NFSMountLocResponse, node *models.NFSMountResponse) {
	var check models.Checks
	if nfsMounted && nfsShared {
		// nfs is mounted and shared among all the nodes
		check = createCheck("NFS Mount", true, constants.SHARE_SUCCESS_MSG, "", "")
	} else if nfsMounted {
		// nfs is mounted but it's not shared
		check = createCheck("NFS Mount", false, "", fmt.Sprintf(constants.SHARE_ERROR_MSG, data.Nfs), fmt.Sprintf(constants.SHARE_RESOLUTION_MSG, data.Nfs, mountLocation))
	} else {
		// nfs volume is not mounted with the node.
		check = createCheck("NFS Mount", false, "", fmt.Sprintf(constants.SHARE_ERROR_MSG_WITHOUT_MOUNT, mountLocation), fmt.Sprintf(constants.SHARE_RESOLUTION_MSG_WITHOUT_MOUNT, mountLocation))
	}
	node.CheckList = append(node.CheckList, check)
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
