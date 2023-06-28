package nfsmountservice

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/systemresource"
)

type NFSService interface {
	GetNFSMountDetails(models.NFSMountRequest) *[]models.NFSMountResponse
	GetNFSMountLoc(req models.NFSMountLocRequest) *models.NFSMountLocResponse
}

type NfsServiceImp struct {
	SystemResourceInfo systemresource.SystemResourceInfo
	port               string
	log                logger.Logger
}

type TempResponse struct {
	MountLocRes models.NFSMountLocResponse
	MountResp   models.NFSMountResponse
}

func NewNFSMountService(log logger.Logger, port string, sysResInfo systemresource.SystemResourceInfo) *NfsServiceImp {
	return &NfsServiceImp{
		port:               port,
		log:                log,
		SystemResourceInfo: sysResInfo,
	}
}

func (nm *NfsServiceImp) GetNFSMountDetails(reqBody models.NFSMountRequest) *[]models.NFSMountResponse {
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
	// using orderList to store the order. so we can generate the same response order which create response.
	var orderList []string

	for index, ip := range reqBody.AutomateNodeIPs {
		key := constants.AUTOMATE + ip + strconv.Itoa(index)
		nm.log.Debug("Call Initiated for Automate node having IP: ", ip)
		go nm.makeConcurrentCall(ip, constants.AUTOMATE, reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.ChefInfraServerNodeIPs {
		key := constants.CHEF_INFRA_SERVER + ip + strconv.Itoa(index)
		nm.log.Debug("Call Initiated for Chefserver node having IP: ", ip)
		go nm.makeConcurrentCall(ip, constants.CHEF_INFRA_SERVER, reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.PostgresqlNodeIPs {
		key := constants.POSTGRESQL + ip + strconv.Itoa(index)
		nm.log.Debug("Call Initiated for Postgresql node having IP: ", ip)
		go nm.makeConcurrentCall(ip, constants.POSTGRESQL, reqBody.MountLocation, ch, key)
		orderList = append(orderList, key)
	}

	for index, ip := range reqBody.OpensearchNodeIPs {
		key := constants.OPENSEARCH + ip + strconv.Itoa(index)
		nm.log.Debug("Call Initiated for Opensearch node having IP: ", ip)
		go nm.makeConcurrentCall(ip, constants.OPENSEARCH, reqBody.MountLocation, ch, key)
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
	nm.log.Debug(prettyMap(nfsMountResultMap))
	nm.log.Debug("All calls Completed")

	makeRespBody(respBody, countMap, orderList, nfsMountResultMap, shareMap, reqBody.MountLocation)
	return respBody
}

func (nm *NfsServiceImp) GetNFSMountLoc(req models.NFSMountLocRequest) *models.NFSMountLocResponse {
	mounts := nm.getMountDetails(req.MountLocation)
	return mounts
}

func prettyMap(mp map[string]models.NFSMountResponse) string {
	b, _ := json.MarshalIndent(mp, "", "  ")
	return string(b)
}

func (nm *NfsServiceImp) makeConcurrentCall(ip string, nodeType string, mountLocation string, respChan chan map[string]TempResponse, key string) {
	res, err := nm.doAPICall(ip, mountLocation)
	nm.log.Debugf("result got from /nfs-mount-loc API for %s: %v", ip, res)

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

func (nm *NfsServiceImp) doAPICall(ip string, mountLocation string) (*models.NFSMountLocResponse, error) {
	reqURL := fmt.Sprintf("http://%s:%s%s", ip, nm.port, constants.NFS_MOUNT_LOC_API_PATH)
	nm.log.Debug("Request URL: ", reqURL)

	reqBody := models.NFSMountLocRequest{
		MountLocation: mountLocation,
	}

	resp, err := httputils.MakeRequest(http.MethodPost, reqURL, reqBody)
	if err != nil {
		nm.log.Error(err.Error())
		return nil, errors.New("Failed to send the HTTP request: " + err.Error())
	}

	return nm.getResultStructFromRespBody(resp.Body)
}

func (nm *NfsServiceImp) getResultStructFromRespBody(respBody io.Reader) (*models.NFSMountLocResponse, error) {
	body, err := io.ReadAll(respBody) // nosemgrep
	if err != nil {
		nm.log.Error(err.Error())
		return nil, errors.New("Cannot able to read data from response body: " + err.Error())
	}

	// Converting API Response Body into Generic Response Struct.
	apiRespStruct := response.ResponseBody{}
	err = json.Unmarshal(body, &apiRespStruct)
	if err != nil {
		return nil, errors.New("Failed to Unmarshal: " + err.Error())
	}

	// If API(/nfs-mount-loc) is itself failing.
	if apiRespStruct.Error != nil {
		nm.log.Error(apiRespStruct.Error)
		return nil, apiRespStruct.Error
	}

	// Converting interface into JSON encoding. apiResp.Result is a interface and for accessing the values we are converting that into json.
	resultByte, err := json.Marshal(apiRespStruct.Result)
	if err != nil {
		return nil, errors.New("Failed to Marshal: " + err.Error())
	}

	resultField := new(models.NFSMountLocResponse)
	// converting JSON into struct.
	err = json.Unmarshal(resultByte, &resultField)
	if err != nil {
		return nil, errors.New("Failed to Unmarshal: " + err.Error())
	}

	return resultField, nil
}

func prepareMountResp(ip, nodeType, mountLocation string, mountLocResp *models.NFSMountLocResponse, err error) models.NFSMountResponse {
	node := models.NFSMountResponse{}
	node.IP = ip
	node.NodeType = nodeType

	if err != nil {
		checkList := createCheck("NFS mount", false, "", "NFS mount location not found", "NFS volume should be mounted on "+mountLocation)
		node.CheckList = append(node.CheckList, checkList)
		return node
	}
	checkMount(mountLocation, &node, mountLocResp)
	return node
}

func makeRespBody(respBody *[]models.NFSMountResponse, countMap map[models.NFSMountLocResponse]int, orderList []string,
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
			checkShare(val.CheckList[0].Passed, isShared, mountLocation, shareMap[key], &val)
		}
		*respBody = append(*respBody, val)
	}
}

func checkMount(mountLocation string, node *models.NFSMountResponse, data *models.NFSMountLocResponse) {
	if data.Address != "" {
		check := createCheck(constants.NFS_MOUNT, true, constants.MOUNT_SUCCESS_MSG, "", "")
		node.CheckList = append(node.CheckList, check)
	} else {
		check := createCheck(constants.NFS_MOUNT, false, "", constants.MOUNT_ERROR_MSG, fmt.Sprintf(constants.MOUNT_RESOLUTION_MSG, mountLocation))
		node.CheckList = append(node.CheckList, check)
	}
}

func checkShare(nfsMounted, nfsShared bool, mountLocation string, data models.NFSMountLocResponse, node *models.NFSMountResponse) {
	var check models.Checks
	if nfsMounted && nfsShared {
		// nfs is mounted and shared among all the nodes
		check = createCheck(constants.NFS_MOUNT, true, constants.SHARE_SUCCESS_MSG, "", "")
	} else if nfsMounted {
		// nfs is mounted but it's not shared
		check = createCheck(constants.NFS_MOUNT, false, "", fmt.Sprintf(constants.SHARE_ERROR_MSG, data.Nfs), fmt.Sprintf(constants.SHARE_RESOLUTION_MSG, data.Nfs, mountLocation))
	} else {
		// nfs volume is not mounted with the node.
		check = createCheck(constants.NFS_MOUNT, false, "", fmt.Sprintf(constants.SHARE_ERROR_MSG_WITHOUT_MOUNT, mountLocation), fmt.Sprintf(constants.SHARE_RESOLUTION_MSG_WITHOUT_MOUNT, mountLocation))
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

func (nm *NfsServiceImp) getMountDetails(mountLocation string) *models.NFSMountLocResponse {
	// If we make disk partitions as false,
	// we can't list mount info
	diskPartition, err := nm.SystemResourceInfo.GetDiskPartitions(true)
	if err != nil {
		nm.log.Error("Error getting disk partions: " + err.Error())
		return &models.NFSMountLocResponse{
			Address:       "",
			Nfs:           "",
			MountLocation: mountLocation,
		}
	}
	for _, partition := range diskPartition {
		if partition.Mountpoint == mountLocation {
			usageStat, err := nm.SystemResourceInfo.GetDiskSpaceInfo(mountLocation)
			if err != nil {
				nm.log.Error("Failed to retrieve disk usage: " + err.Error())
				return &models.NFSMountLocResponse{
					Address:       "",
					Nfs:           "",
					MountLocation: mountLocation,
				}
			}
			return &models.NFSMountLocResponse{
				Address:            strings.Split(partition.Device, ":")[0],
				Nfs:                partition.Device,
				MountLocation:      mountLocation,
				StorageCapacity:    nm.SystemResourceInfo.FormatBytes(usageStat.Total),
				AvailableFreeSpace: nm.SystemResourceInfo.FormatBytes(usageStat.Free),
			}
		}
	}
	nm.log.Debug("Mount location Not found")
	return &models.NFSMountLocResponse{
		MountLocation: mountLocation,
	}
}
