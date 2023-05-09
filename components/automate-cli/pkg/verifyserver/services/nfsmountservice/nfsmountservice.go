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
	"sync"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

type INFSService interface {
	GetNFSMountDetails(models.NFSMountRequest, bool) *[]models.NFSMountResponse
}

type NFSMountService struct {
}

func NewNFSMountService() INFSService {
	return &NFSMountService{}
}

var mut *sync.Mutex = &sync.Mutex{}

func makeConcurrentCall(ip string, test bool, node_type string, mountLocation string, ch chan string, nfsMountResultMap map[string][]models.NFSMountResponse, shareMap map[string]models.NFSMountLocResponse, key string, countMap map[models.NFSMountLocResponse]int) {
	res := doAPICall(ip, test, node_type, mountLocation, shareMap, key, countMap)
	// Mutex Lock is Mandatory. What if two routines tries to write at same time.
	mut.Lock()
	nfsMountResultMap[key] = append(nfsMountResultMap[key], res)
	mut.Unlock()
	ch <- "done"
}

func makeRespBody(respBody *[]models.NFSMountResponse, countMap map[models.NFSMountLocResponse]int, orderList []string, nfsMountResultMap map[string][]models.NFSMountResponse, shareMap map[string]models.NFSMountLocResponse) {
	compareWith := models.NFSMountLocResponse{}
	currMax := 0
	// Finding the Majority element and putting in the compareWith variable for the later comparisons
	for k, v := range countMap {
		// fmt.Println(k, v)
		if v > currMax {
			currMax = v
			compareWith = k
		}
	}

	for _, key := range orderList {
		for _, val := range nfsMountResultMap[key] {
			// Need to check error first. Because while making call to the API if something get wrong
			// then we are storing checkList as nil and putting the error in Error field.
			if val.Error == nil {
				// if nfs is mounted then in our first check for the particular node it will be true. hence we are passing that which checking shareability
				// Test2 - Check for NFS Volume is Shared among all nodes or not
				if val.CheckList[0].Passed {
					checkShare(shareMap[key], compareWith, &val, true)
				} else {
					checkShare(shareMap[key], compareWith, &val, false)
				}
			}
			*respBody = append(*respBody, val)
		}
	}
}

func (nm *NFSMountService) GetNFSMountDetails(reqBody models.NFSMountRequest, test bool) *[]models.NFSMountResponse {
	respBody := new([]models.NFSMountResponse)
	// For storing the output of go routine temporary in nfsMountResultMap
	nfsMountResultMap := make(map[string][]models.NFSMountResponse)
	ch := make(chan string)
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
		go makeConcurrentCall(ip, test, "automate", reqBody.MountLocation, ch, nfsMountResultMap, shareMap, key, countMap)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.ChefInfraServerNodeIPs {
		key := "chef-infra-server" + ip + strconv.Itoa(index)
		go makeConcurrentCall(ip, test, "chef-infra-server", reqBody.MountLocation, ch, nfsMountResultMap, shareMap, key, countMap)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.PostgresqlNodeIPs {
		key := "postgresql" + ip + strconv.Itoa(index)
		go makeConcurrentCall(ip, test, "postgresql", reqBody.MountLocation, ch, nfsMountResultMap, shareMap, key, countMap)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.OpensearchNodeIPs {
		key := "opensearch" + ip + strconv.Itoa(index)
		go makeConcurrentCall(ip, test, "opensearch", reqBody.MountLocation, ch, nfsMountResultMap, shareMap, key, countMap)
		orderList = append(orderList, key)
	}

	// it will help us to hold the program until all triggered go routines cameback
	for i := 0; i < len(orderList); i++ {
		<-ch
	}

	makeRespBody(respBody, countMap, orderList, nfsMountResultMap, shareMap)

	close(ch)
	return respBody
}

func doAPICall(ip string, test bool, node_type string, mountLocation string, shareMap map[string]models.NFSMountLocResponse, key string, countMap map[models.NFSMountLocResponse]int) models.NFSMountResponse {
	node := models.NFSMountResponse{}
	node.IP = ip
	node.NodeType = node_type
	var url string
	// If test is true that means we are running the unit test cases
	// And there we are passing using testserver and we are passing it's URL
	// ts.URL contains protocol+URL+portnumber hence we directly need to pass that
	if test {
		url = ip
	} else {
		url = fmt.Sprintf("http://%s:7799", ip)
	}

	// It will trigger /nfs-mount-loc API
	resp, err := triggerAPI(url, mountLocation)
	if err != nil {
		node.Error = fiber.NewError(http.StatusBadRequest, err.Error())
		return node
	}

	// Getting the result struct from whole response
	result, err := getResultStructFromRespBody(resp.Body)
	if err != nil {
		node.Error = fiber.NewError(http.StatusBadRequest, err.Error())
		return node
	}
	// if address is empty then we are not storing it. Because it's of no use while doing the comparisons
	if result.Address != "" {
		mut.Lock()
		countMap[*result] = countMap[*result] + 1
		shareMap[key] = *result
		mut.Unlock()
	}
	// fmt.Println(result)

	// Test1 - Check for NFS Volume is mounted at correct Mount Location or not
	checkMount(mountLocation, &node, result)

	return node
}

func triggerAPI(url, mountLocation string) (*http.Response, error) {
	// Request Body for /nfs-mount-loc API
	reqBody := models.NFSMountLocRequest{
		MountLocation: mountLocation,
	}

	// Converting Body into json encoding for passing into request
	reqBodyJSON, err := json.Marshal(reqBody)
	if err != nil {
		return nil, errors.New("Failed to Marshal: " + err.Error())
	}

	reqURL := url + "/api/v1/fetch/nfs-mount-loc"
	// fmt.Println(reqURL)
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

	return resp, nil
}

func getResultStructFromRespBody(respBody io.Reader) (*models.NFSMountLocResponse, error) {
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

func checkMount(mountLocation string, node *models.NFSMountResponse, data *models.NFSMountLocResponse) {
	if data.Address != "" {
		check := createCheck("NFS Mount", true, constants.MOUNT_SUCCESS_MSG, "", "")
		node.CheckList = append(node.CheckList, check)
	} else {
		check := createCheck("NFS Mount", false, "", constants.MOUNT_ERROR_MSG, fmt.Sprintf(constants.MOUNT_RESOLUTION_MSG, mountLocation))
		node.CheckList = append(node.CheckList, check)
	}
}

func checkShare(data models.NFSMountLocResponse, compareWith models.NFSMountLocResponse, node *models.NFSMountResponse, nfsMounted bool) {
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
