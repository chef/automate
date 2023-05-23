package systemresourceservice

import (
	"fmt"
	"math"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type ISystemResourcesService interface {
	GetSystemResourcesForDeployment(string, string) (*models.ApiResult, error)
}

type SystemResourcesService struct {
	logger                 logger.Logger
	GetOsAndFileSystemInfo IGetOsAndFileSystemInfo
}

func NewSystemResourceService(log logger.Logger, osFsUtil IGetOsAndFileSystemInfo) *SystemResourcesService {
	return &SystemResourcesService{
		logger:                 log,
		GetOsAndFileSystemInfo: osFsUtil,
	}
}

const (
	CPU_INFO_FILE = "/proc/cpuinfo"

	CPU_COUNT_CHECK_TITLE   = "CPU count check"
	CPU_SPEED_CHECK_TITLE   = "CPU speed check"
	MEMORY_SIZE_CHECK_TITLE = "Memory size check"
	FREE_SPACE_CHECK        = "%s free space check"

	SUCCESS_MSG        = "%s should have free space >=%vGB"
	ERROR_MSG          = "%s free space is %0.2fGB"
	SUCCESS_MSG_IN_PER = " or %v%% of total size of /hab"

	INVALID_NODE_TYPE_ERR = "given query node_type with value=%s is not supported"
)

func (srs *SystemResourcesService) GetSystemResourcesForDeployment(nodeType, deploymentState string) (*models.ApiResult, error) {
	srsResponse := &models.ApiResult{
		Passed: true,
		Checks: []models.Checks{},
	}

	cpuCountCheck := srs.GetCpuCountCheck()

	if !cpuCountCheck.Passed {
		srsResponse.Passed = false
	}

	srsResponse.Checks = append(srsResponse.Checks, *cpuCountCheck)

	cpuSpeedCheck := srs.GetCpuSpeedCheck(CPU_INFO_FILE)

	if !cpuSpeedCheck.Passed {
		srsResponse.Passed = false
	}

	srsResponse.Checks = append(srsResponse.Checks, *cpuSpeedCheck)

	memorySizeCheck := srs.GetMemorySizeCheck()

	if !memorySizeCheck.Passed {
		srsResponse.Passed = false
	}

	srsResponse.Checks = append(srsResponse.Checks, *memorySizeCheck)

	switch deploymentState {
	case constants.PRE_DEPLOY:
		habFreeSpaceCheckPreDepl, err := srs.GetHabFreeSpaceCheckPreDeployment(nodeType)
		if err != nil {
			return nil, err
		}
		if !habFreeSpaceCheckPreDepl.Passed {
			srsResponse.Passed = false
		}
		srsResponse.Checks = append(srsResponse.Checks, *habFreeSpaceCheckPreDepl)

	case constants.POST_DEPLOY:
		habFreeSpaceCheckPostDepl, err := srs.GetHabFreeSpaceCheckPostDeployment(nodeType)
		if err != nil {
			return nil, err
		}
		if !habFreeSpaceCheckPostDepl.Passed {
			srsResponse.Passed = false
		}
		srsResponse.Checks = append(srsResponse.Checks, *habFreeSpaceCheckPostDepl)
	default:
		srs.logger.Errorf("Wrong query deployment_state=%s", deploymentState)
		return nil, fmt.Errorf("given query deployment_state with value=%s is not supported", deploymentState)
	}

	tmpFreeSpaceCheck := srs.GetFreeDiskSpaceCheckOfDir("/tmp", constants.TMP_FREE_DISK_IN_PER, constants.TMP_FREE_DISK_IN_GB, "Temp")

	if !tmpFreeSpaceCheck.Passed {
		srsResponse.Passed = false
	}

	srsResponse.Checks = append(srsResponse.Checks, *tmpFreeSpaceCheck)

	rootFreeSpaceCheck := srs.GetFreeDiskSpaceCheckOfDir("/", constants.ROOT_FREE_DISK_IN_PER, constants.ROOT_FREE_DISK_IN_GB, "/(root volume)")

	if !rootFreeSpaceCheck.Passed {
		srsResponse.Passed = false
	}
	srsResponse.Checks = append(srsResponse.Checks, *rootFreeSpaceCheck)
	return srsResponse, nil
}

func (srs *SystemResourcesService) GetCpuCountCheck() *models.Checks {
	srs.logger.Debug("CPU count check is running")
	cpuCount := srs.GetOsAndFileSystemInfo.GetNumberOfCPU() //need to find better way for unit test
	srs.logger.Debug("CPU count is : ", cpuCount)

	var checkResp *models.Checks
	if cpuCount >= constants.MIN_CPU_COUNT {
		successMsg := fmt.Sprintf("CPU count is >=%v", constants.MIN_CPU_COUNT)
		checkResp = srs.GetChecksModel(true, CPU_COUNT_CHECK_TITLE, successMsg, "", "")
	} else {
		errorMsg := fmt.Sprintf("CPU count is %v", cpuCount)
		resolutionMsg := fmt.Sprintf("CPU count should be >=%v", constants.MIN_CPU_COUNT)
		checkResp = srs.GetChecksModel(false, CPU_COUNT_CHECK_TITLE, "", errorMsg, resolutionMsg)
	}
	return checkResp
}

func (srs *SystemResourcesService) GetCpuSpeedCheck(cpuInfoFile string) *models.Checks {
	srs.logger.Debug("CPU speed check is running")
	cpuSpeed, err := srs.GetOsAndFileSystemInfo.GetCPUSpeed(cpuInfoFile)
	var resp *models.Checks

	if err != nil {
		//TODO error also needs to be written from this function to above function needs to confirm.
		srs.logger.Error("Error occured while getting cpu speed :", err)
		ResolutionMsg := "Please run system on supported platform"
		resp = srs.GetChecksModel(false, CPU_SPEED_CHECK_TITLE, "", err.Error(), ResolutionMsg)
		return resp
	}

	srs.logger.Debug("CPU speed is : ", cpuSpeed)

	if cpuSpeed >= constants.MIN_CPU_SPEED {
		srs.logger.Debug("CPU speed is ", cpuSpeed)
		successMsg := fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED)
		resp = srs.GetChecksModel(true, CPU_SPEED_CHECK_TITLE, successMsg, "", "")
	} else {
		srs.logger.Debug("CPU speed is ", cpuSpeed)
		errorMsg := fmt.Sprintf("CPU speed is %vGHz", cpuSpeed)
		resolutionMsg := fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED)
		resp = srs.GetChecksModel(false, CPU_SPEED_CHECK_TITLE, "", errorMsg, resolutionMsg)
	}
	return resp
}

func (srs *SystemResourcesService) GetMemorySizeCheck() *models.Checks {
	srs.logger.Debug("Memory size check is running")
	memoryInGB, err := srs.GetOsAndFileSystemInfo.GetMemory()
	srs.logger.Debug("Current memory of system is :", memoryInGB)

	var resp *models.Checks
	if err != nil {
		srs.logger.Error("Error occured while getting memory information : ", err.Error())
		resolutionMsg := "Please run system on supported platform"
		resp = srs.GetChecksModel(false, MEMORY_SIZE_CHECK_TITLE, "", err.Error(), resolutionMsg)
		return resp
	}

	srs.logger.Debugf("Current memory of system is :%vGB", memoryInGB)
	if memoryInGB >= constants.MIN_MEMORY {
		successMsg := fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY)
		resp = srs.GetChecksModel(true, MEMORY_SIZE_CHECK_TITLE, successMsg, "", "")
	} else {
		errorMsg := fmt.Sprintf("Memory is %.2fGB", memoryInGB)
		resolutionMsg := fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY)
		resp = srs.GetChecksModel(false, MEMORY_SIZE_CHECK_TITLE, "", errorMsg, resolutionMsg)
	}
	return resp
}

func (srs *SystemResourcesService) GetHabFreeSpaceCheckPreDeployment(nodeType string) (*models.Checks, error) {
	srs.logger.Debug("Hab free space check is running for node_type : ", nodeType, " and deployment_state :", constants.PRE_DEPLOY)

	currentFreeSpaceInGB, err := srs.GetFreeDiskSpaceOfGivenDir("/hab")
	var resp *models.Checks
	if err != nil {
		srs.logger.Error("Unable to determine free space of /hab :", err)
		resp = srs.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", err.Error(), "Please run system on supported platform")
		return resp, nil
	}

	srs.logger.Debugf("The current total free space in hab : %0.2fGB", currentFreeSpaceInGB)

	switch nodeType {
	case constants.AUTOMATE:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_A2, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_A2, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.CHEF_INFRA_SERVER:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_CS, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_CS, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.OPENSEARCH:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_OS, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_OS, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.POSTGRESQL:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_PG, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_PG, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.BASTION:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_BASTION, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_BASTION, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	default:
		srs.logger.Errorf(INVALID_NODE_TYPE_ERR, nodeType)
		return nil, fmt.Errorf(INVALID_NODE_TYPE_ERR, nodeType)
	}
	return resp, nil
}

func (srs *SystemResourcesService) GetHabFreeSpaceCheckPostDeployment(nodeType string) (*models.Checks, error) {
	srs.logger.Debugf("Hab free space check is running for node_type : %s  and deployment_state : %s", nodeType, constants.POST_DEPLOY)
	currentFreeSpaceInGB, err := srs.GetFreeDiskSpaceOfGivenDir("/hab")

	var resp *models.Checks
	if err != nil {
		srs.logger.Error("Unable to determine free space of /hab :", err.Error())
		resp = srs.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", err.Error(), "Please run system on supported platform")
		return resp, nil
	}
	srs.logger.Debugf("current free space in /hab after deployment :%0.2fGB", currentFreeSpaceInGB)

	totalSpaceInGBInHab, err := srs.GetTotalSpaceOfGivenDir("/hab")

	if err != nil {
		srs.logger.Error("Unable to retrive total space of /hab :", err.Error())
		resp = srs.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "/hab"), "", err.Error(), "Please run system on supported platform")
		return resp, nil
	}

	srs.logger.Debugf("current total space in /hab : %0.2f", totalSpaceInGBInHab)

	switch nodeType {
	case constants.AUTOMATE:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER, float64(constants.HAB_FREE_DISK_AFTER_DEP_A2), constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.CHEF_INFRA_SERVER:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_CS_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_CS)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_CS_IN_PER, float64(constants.HAB_FREE_DISK_AFTER_DEP_CS), constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.OPENSEARCH:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER, float64(constants.HAB_FREE_DISK_AFTER_DEP_OS), constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.POSTGRESQL:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_PG)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER, float64(constants.HAB_FREE_DISK_AFTER_DEP_PG), constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case constants.BASTION:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_BASTION)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER, float64(constants.HAB_FREE_DISK_AFTER_DEP_BASTION), constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	default:
		srs.logger.Debugf(INVALID_NODE_TYPE_ERR, nodeType)
		return nil, fmt.Errorf(INVALID_NODE_TYPE_ERR, nodeType)
	}
	return resp, nil
}

func (srs *SystemResourcesService) GetFreeDiskSpaceCheckOfDir(dirPath string, freeDiskSpaceWantInPer, freeDiskSpaceWantInGB float64, checkTitle string) *models.Checks {
	srs.logger.Debugf("free disk space check is running for %s directory", dirPath)

	var resp *models.Checks
	currentFreeSpaceInDir, err := srs.GetFreeDiskSpaceOfGivenDir(dirPath)
	if err != nil {
		srs.logger.Errorf("Unable to determine free space of %s error=%v", dirPath, err)
		resp = srs.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, checkTitle), "", err.Error(), "Please run system on supported platform")
		return resp
	}

	srs.logger.Debugf("Current free disk space in %s is %0.2fGB", dirPath, currentFreeSpaceInDir)

	var totalSpaceInHab float64
	totalSpaceInHab, err = srs.GetTotalSpaceOfGivenDir("/hab")
	if err != nil {
		srs.logger.Error("Unable to determine total space of /hab :", err)
		resp = srs.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, checkTitle), "", err.Error(), "Please run system on supported platform")
		return resp
	}

	srs.logger.Debugf("Current total space in /hab : %0.2fGB", totalSpaceInHab)

	expectedFreeSpaceValue := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInHab, freeDiskSpaceWantInPer, freeDiskSpaceWantInGB)
	srs.logger.Debugf("Expected free space calculated for %s is %0.2fGB", dirPath, expectedFreeSpaceValue)

	passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpaceValue, currentFreeSpaceInDir, freeDiskSpaceWantInPer, freeDiskSpaceWantInGB, constants.POST_DEPLOY, dirPath)
	resp = srs.GetChecksModel(passed, fmt.Sprintf(FREE_SPACE_CHECK, checkTitle), successMsg, errorMsg, resolutionMsg)
	return resp
}

func (srs *SystemResourcesService) GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpaceCalculated, currentFreeSpace, expectedFreeSpaceWantInPer, expectedFreeSpaceWantInGB float64, deploymentState, dirName string) (bool, string, string, string) {

	if dirName == "/" {
		dirName = "/(root volume)"
	}

	successMsg := ""
	resolutionMsg := ""
	errorMsg := ""
	passed := false
	if currentFreeSpace >= expectedFreeSpaceCalculated {
		passed = true
		successMsg = fmt.Sprintf(SUCCESS_MSG, dirName, expectedFreeSpaceWantInGB)
		errorMsg = ""
		resolutionMsg = ""
	} else {
		successMsg = ""
		errorMsg = fmt.Sprintf(ERROR_MSG, dirName, currentFreeSpace)
		resolutionMsg = fmt.Sprintf(SUCCESS_MSG, dirName, expectedFreeSpaceWantInGB)
	}

	if deploymentState == constants.POST_DEPLOY {
		if passed {
			successMsg = successMsg + fmt.Sprintf(SUCCESS_MSG_IN_PER, expectedFreeSpaceWantInPer*100)
			errorMsg = ""
			resolutionMsg = ""
		} else {
			successMsg = ""
			resolutionMsg = resolutionMsg + fmt.Sprintf(SUCCESS_MSG_IN_PER, expectedFreeSpaceWantInPer*100)
		}
	}
	return passed, successMsg, errorMsg, resolutionMsg
}

func (srs *SystemResourcesService) GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGB, freeSpaceWantInPercentage, freeSpaceWantInGB float64) float64 {
	percentageValueOfTotalSpace := totalSpaceInGB * freeSpaceWantInPercentage
	return math.Max(percentageValueOfTotalSpace, float64(freeSpaceWantInGB))
}

func (srs *SystemResourcesService) GetChecksModel(passed bool, checkTitle, successMsg, errorMsg, resolutionMsg string) *models.Checks {
	return &models.Checks{
		Title:         checkTitle,
		Passed:        passed,
		SuccessMsg:    successMsg,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
}

func (srs *SystemResourcesService) GetFreeDiskSpaceOfGivenDir(dirPath string) (float64, error) {
	isPathExist, err := srs.GetOsAndFileSystemInfo.CheckPathExists(dirPath)
	if err != nil {
		return 0, err
	}

	if !isPathExist {
		dirPath = "/"
	}
	_, freeSpace, err := srs.GetOsAndFileSystemInfo.GetDiskSpaceInfo(dirPath)
	return freeSpace, err
}

func (srs *SystemResourcesService) GetTotalSpaceOfGivenDir(dirPath string) (float64, error) {
	isPathExist, err := srs.GetOsAndFileSystemInfo.CheckPathExists(dirPath)

	if err != nil {
		return 0, err
	}

	if !isPathExist {
		dirPath = "/"
	}
	totalSpace, _, err := srs.GetOsAndFileSystemInfo.GetDiskSpaceInfo(dirPath)
	return totalSpace, err
}
